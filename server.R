library(reticulate)
library(shiny)
library(stringr)
library(shinybusy)

return_aspect_polarity <- function(reviews_df, polarity){
  
  aspects <- reviews_df[reviews_df$sentiment== polarity, ]$aspect
  if (length(aspects) >0){
    aspects_str <- ''
    for (aspect in unique(aspects)){
      if(aspects_str!=''){
      aspects_str <- paste(aspects_str,', ', aspect, sep='')}
      else{
        
        aspects_str <- paste(aspects_str, aspect, sep='')
        
      }
      
    }}
  else{
    
    aspects_str <- paste('There are no', tolower(polarity), 'aspects.')
  }
  
  return(aspects_str)
  
}

colour_multiple_aspects <- function(reviews_df, reviews_string, recursive_review, review_extracts, i){
  review_extract <- review_extracts[i]
  input_aspect <- as.character(reviews_df[reviews_df$extract_original == review_extract, ][1, 'aspect'])
  input_polarity <- as.character(reviews_df[reviews_df$extract_original == review_extract, ][1, 'sentiment'])
    if(input_polarity == 'Positive'){
      aspect_colour <- '#00ff0080'
    }
    else if(input_polarity == 'Negative'){
      aspect_colour <- '#FF7F50'
    }
  review_split <- str_split(recursive_review, review_extract, simplify = TRUE)
  if(length(review_split)>1){
    first_part <- review_split[1, 1]
    second_part <- review_split[1,2]
  }
  if (i == length(review_extracts)){
    return(paste(reviews_string, first_part, '<a class="ui-tooltip" title = "Aspect: ', input_aspect, '&#013Polarity: ', input_polarity,'"<span style="cursor: help;"><span style="background-color: ', aspect_colour ,'">', review_extract,'</span></span></a>', second_part, sep = ''))
  }
  else{
    reviews_string <- paste(reviews_string, first_part, '<a class="ui-tooltip" title = "Aspect: ', input_aspect, '&#013Polarity: ', input_polarity,'"<span style="cursor: help;"><span style="background-color: ', aspect_colour ,'">', review_extract,'</span></span></a>', colour_multiple_aspects(reviews_df, reviews_string, second_part, review_extracts, i+1), sep ='')}}

print_analysis <- function(reviews_df, review){
  
    reviews_extracts <- c()
    for (review_extract in reviews_df$extract_original){
      if(grepl(review_extract, review, fixed=TRUE)){
        reviews_extracts <- c(reviews_extracts, review_extract)
      }
    }
    
    reviews_body <- colour_multiple_aspects(reviews_df, '', review, reviews_extracts, 1)
    return(paste('<div style = "border-radius:8px; background-color: #ededed; border-style:solid; border-color:#b1d1fc;"><div style = "margin: 5px; padding: 12px">', '<b>ANALYSIS RESULT</b></br></br>', reviews_body, '</br></br><p style="color:green">Positive Aspects: <b>', return_aspect_polarity(reviews_df, 'Positive') , '</b></br><p style="color:red">Negative Aspects: <b>', return_aspect_polarity(reviews_df, 'Negative') , '</b></b></p></div></div>' ,sep =''))
  
}

PYTHON_DEPENDENCIES = c('pip','spacy==2.3.2', 'google-cloud-language', 'regex', 'pandas==1.2.4', 'cython==0.29.2', 'neuralcoref==4.0', 'https://github.com/explosion/spacy-models/releases/download/en_core_web_sm-2.3.1/en_core_web_sm-2.3.1.tar.gz')

server<-function(input, output, session) {
  virtualenv_dir = 'nlp_trial'
  reticulate::use_python('/Users/jgordyn/opt/anaconda3/python3.7')
  
  # Create virtual env and install dependencies
  #reticulate::virtualenv_create(envname = 'nlp/trial')
  #reticulate::virtualenv_install(virtualenv_dir, packages = PYTHON_DEPENDENCIES, ignore_installed=TRUE)
  reticulate::use_virtualenv('/Users/jgordyn/opt/anaconda3/envs/nlp_new', required = T)
  
  observeEvent(input$button, {
  source_python("NLP_Project_2.py")
  show_modal_spinner() # show the modal window
  review_to_analyze <- input$review
  analyzed_review_df <- extract_sentiment_analysis(review_to_analyze)
  output$value <- renderText({print_analysis(analyzed_review_df, review_to_analyze)})
  remove_modal_spinner()
  })
  
  observeEvent(input$review_example, {
    if (input$review_example == 'Hotel Review'){example = 'The hotel was outstanding! The decorations were amazing and the staff was extremely helpful. The prices were a little too much though but totally worth it! Completely recommendable experience!' }
    else if (input$review_example == 'Restaurant Review'){example = 'I went to Starbucks the other day and it was amazing! I had a latte which was exquisite! I also ordered a sandwich: the best I have ever tried! Highly recommend the place! The ambience was not the best though, it was really noisy.'}
    else if(input$review_example == 'Uni Review'){example = 'FIT5212 is a really good unit. The teachers were really helpful. I believe the students learned a lot, very useful Machine Learning tools. That being said, FIT5212 is pretty hard and has too much content so you need to stay focused the whole semester'}
    else if(input$review_example == 'App Review'){example = 'Voice Reviews works like a charm! It is the best customer service app on the market! It uses sophisticated Machine Learning techniques to provide a complete and accurate analysis of your reviews! Voice Reviews is still a little slow, but we are working on that. Bottomline: such a useful customer success tool to consider.'}
    else{example =''}
    updateTextAreaInput(session, 'review', paste(input$review_example, 'Example', sep=' '), example)
    output$value <- renderText({''})
  })
  
  observeEvent(input$review, {
    output$value <- renderText({''})
  })
  
}