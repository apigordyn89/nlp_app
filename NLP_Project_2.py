import spacy
import neuralcoref
import en_core_web_sm
nlp = en_core_web_sm.load()
import google.cloud.language_v1 as language_v1
import regex as re
from spacy.matcher import Matcher
import pandas as pd

client = language_v1.LanguageServiceClient.from_service_account_file('API01-e2eb0b73f5c2.json')
neuralcoref.add_to_pipe(nlp)

def coreference(text):
    # Add neural coref to SpaCy's pipe
    # You're done. You can now use NeuralCoref as you usually manipulate a SpaCy document annotations.
    doc = nlp(text)
    doc_coref = doc._.coref_resolved
    coref_clusters = doc._.coref_clusters
    # print(coref_clusters)
    return (doc_coref, coref_clusters)

def entity_extraction(text):
    document = language_v1.Document(content=text, type_=language_v1.Document.Type.PLAIN_TEXT)
    entities = client.analyze_entities(request={'document': document}).entities
    entities_list = [entity_name for i in range(0, len(entities)) for entity_name in [entities[i].name]]
    return entities_list

def wrangle_trained_extracts(file):
    f = open(file, 'r')
    return [train_extract.lstrip().lstrip("'").rstrip("',\n") for train_extract in f.readlines()]


def retokenize_entities(text, entities_list):
    doc = nlp(text)
    for entity in entities_list:
        if ' ' in entity:
            entity_position = re.search(entity,doc.text)
            if entity_position:
                start = entity_position.start()
                end = entity_position.end()
                with doc.retokenize() as retokenizer:
                    if entity[0].isupper():
                        attrs = {"POS": "PROPN"}
                    else:
                        attrs = {"POS": "NOUN"}
                    retokenizer.merge(doc.char_span(start, end), attrs=attrs)
    return(doc)

def build_matcher():
    matcher = Matcher(nlp.vocab)
    total_pattern = []
    training_extracts = wrangle_trained_extracts('train_data.txt')
    docs = nlp.pipe(training_extracts)
    for doc in docs:
        pattern_list = []
        for token in doc:
            pattern = {'POS': token.pos_}
            pattern_list.append(pattern)
        total_pattern.append(pattern_list)
    i = 0
    for pattern in total_pattern:
        pattern_name = 'pattern' + str(i)
        matcher.add(pattern_name, None, pattern)
        i += 1
    return matcher

def drop_subset(doc, matches):
    matches_dictionary = dict()
    list_set = []
    list_set_final = []
    for match_id, start, end in matches:
        matches_dictionary[doc[start:end].text] = (match_id, start, end)
    matches = []
    for key in matches_dictionary.keys():
        list_set.append(key)
    for i in range(0, len(list_set)):
        test = list_set.pop(i)
        counter = 0
        for j in list_set:
            if test not in j:
                counter += 1
            else:
                break
        if counter == len(list_set):
            list_set_final.append(test)
        list_set.insert(i,test)
    list_set = list_set_final
    for element in list_set:
        matches.append(matches_dictionary[element])
    return matches
def merge(s1, s2):
    s1_list = s1.split()
    s2_list = s2.split()
    i = 0
    while i != len(s1_list):
        j = 0
        if i==0:
            string_to_print = s1_list[-1]
        else:
            string_to_print = s1_list[-1-i] + ' ' + string_to_print
        string_to_print_list = string_to_print.split()
        for word in string_to_print_list:
            if word in s2_list:
                j +=1
        if s2.startswith(string_to_print) and len(string_to_print_list)==j:
            return[" ".join(s1_list[0:len(s1_list) - i -1]) + " " + s2]
        i += 1
    return[s1,s2]

def join_reviews(review_matches_list):
    if len(review_matches_list)>1:
        result_list = []
        test_list = review_matches_list
        for i in range(len(test_list)):
            if i==0:
                merge_result = merge(test_list[i], test_list[i+1])
                for i in merge_result:
                    result_list.append(i)
            else:
                if i!= len(test_list)-1:
                    merge_result = merge(result_list[-1], test_list[i+1])
                    if len(merge_result)==2:
                        result_list.append(merge_result[-1])
                    else:
                        result_list[-1] = merge_result[0]
        return(result_list)
    else:
        return(review_matches_list)


def extract_non_relevant(text):
    max_salience = 0
    entity_list = []
    document = language_v1.Document(content=text, type_=language_v1.Document.Type.PLAIN_TEXT)
    entities = client.analyze_entities(request={'document': document}).entities
    for entity in entities:
        entity_list.append(entity.name)
        entity_salience = entity.salience
        if entity_salience > max_salience:
            max_salience = entity_salience
            main_entity = entity.name
    entities_non_relevant = list(set(entity_list))
    entities_non_relevant.remove(main_entity)
    return entities_non_relevant


def extract_aspect_opinion(original_text):
    i = 0
    aspect_opinion_df = pd.DataFrame()
    review_list = []
    text = coreference(original_text)[0]
    entities_list = entity_extraction(text)
    doc = retokenize_entities(text, entities_list)
    matcher = build_matcher()
    matches = matcher(doc)
    matches = drop_subset(doc, matches)
    for match_id, start, end in matches:
        review_extract = doc[start:end].text
        for entity in set(entities_list):
            if entity in review_extract:
                aspect_opinion_df.loc[i, 'aspect'] = entity
                aspect_opinion_df.loc[i, 'extract'] = review_extract
                review_list.append(review_extract)
    review_list_corrected = review_list.copy()
    for j in range(1, len(review_list)):
        if review_list[j-1] + ' and ' + review_list[j] in text or review_list[j-1] + ' but ' + review_list[j] in text:
            for entity in set(entities_list):
                if entity in review_list[j-1]:
                    entity_1 = entity
                if entity in review_list[j]:
                    entity_2 = entity
            if entity_1 == entity_2:
                extract_to_remove = review_list[j-1]
                review_list_corrected.remove(extract_to_remove)
    review_list = join_reviews(review_list_corrected)
    review_list = [extract.lstrip() for extract in review_list]
    for entity in set(entities_list):
        for review_extract in review_list:
            non_relevant_entities = extract_non_relevant(review_extract)
            if entity in review_extract and entity not in non_relevant_entities:
                aspect_opinion_df.loc[i, 'aspect'] = entity
                aspect_opinion_df.loc[i, 'extract'] = review_extract
                i += 1
    return aspect_opinion_df

def sentiment_analysis(text, main_entity):
    document = language_v1.Document(content=text, type_=language_v1.Document.Type.PLAIN_TEXT)
    entities = client.analyze_entity_sentiment(request={'document': document}).entities
    for entity in entities:
        if entity.name == main_entity:
            sentiment_score = entity.sentiment.score
            if sentiment_score > 0.2:
                sentiment = 'Positive'
            elif sentiment_score < 0:
                sentiment = 'Negative'
            else:
                sentiment = 'Neutral'
        else:
            sentiment = 'Neutral'
        return sentiment

def extract_sentiment_analysis(text):
    try:
        j = 0
        coreference_cluster = coreference(text)[1]
        aspect_opinion_sentiment_df = pd.DataFrame()
        aspect_opinion_df = extract_aspect_opinion(text)
        for i in range(0, len(aspect_opinion_df)):
            aspect = aspect_opinion_df.loc[i, 'aspect']
            extract = aspect_opinion_df.loc[i, 'extract']
            sentiment = sentiment_analysis(extract, aspect)
            if sentiment != 'Neutral':
                aspect_opinion_sentiment_df.loc[j, 'aspect'] = aspect
                aspect_opinion_sentiment_df.loc[j, 'extract_original'] = extract
                aspect_opinion_sentiment_df.loc[j, 'extract_coref'] = extract
                aspect_opinion_sentiment_df.loc[j, 'sentiment'] = sentiment
                if extract in text:
                    aspect_opinion_sentiment_df.loc[j, 'starts_in'] = text.find(extract)
                j += 1
        for cluster in coreference_cluster:
            break_variable = 0
            main_word = str(cluster.main)
            mentions = cluster.mentions
            for i in range(0, len(aspect_opinion_sentiment_df)):
                if break_variable == 1:
                    break
                extract = aspect_opinion_sentiment_df.loc[i, 'extract_coref']
                if main_word in extract:
                    for mention in mentions:
                        for word_count in range(1, extract.count(main_word) + 1):
                            extract_original = extract.replace(main_word, str(mention), word_count)
                            extract_original_backwards = extract[::-1].replace(main_word[::-1], str(mention)[::-1], word_count)[::-1]
                            if extract_original in text:
                                aspect_opinion_sentiment_df.loc[i, 'extract_original'] = extract_original
                                aspect_opinion_sentiment_df.loc[i, 'starts_in'] = text.find(extract_original)
                                # break_variable = 1
                                break
                            if extract_original_backwards in text:
                                aspect_opinion_sentiment_df.loc[i, 'extract_original'] = extract_original_backwards
                                aspect_opinion_sentiment_df.loc[i, 'starts_in'] = text.find(extract_original_backwards)
                                # break_variable = 1
                                break
        if len(aspect_opinion_sentiment_df) > 0:
            aspect_opinion_sentiment_df = aspect_opinion_sentiment_df.sort_values(by=['starts_in']).reset_index(drop=True)
            return aspect_opinion_sentiment_df
        else:
            return 'No matches.'
    except:
        return 'No matches.'

