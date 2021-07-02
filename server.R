library('tidyverse')
library('shiny')
library('ggplot2')
library('gridExtra')
library('shinythemes')
library('bsplus')
library('lubridate')
library('plotly')
library('shinyalert')
library('shinybusy')

# gcs_auth(json_file = "/Users/jgordyn/Google Drive/DATA SCIENCE MASTER/NLP/API01-e2eb0b73f5c2.json")
# reviews_df <- gcs_get_object('gs://tu-vieja-en-recontra-tanga/aspects_reviews_final.csv')
reviews_df <- read_csv('aspects_reviews_final_trial.csv')
reviews_df$date<- as.character(reviews_df$date)
trend_no <- 0
data_click_global <- 0
data_click_global_all <- 0
data_click_global_aspect <- 0

return_aspect_polarity_2 <- function(reviews_df, polarity){
  
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

colour_multiple_aspects_2 <- function(reviews_df, reviews_string, recursive_review, review_extracts, i){
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
    reviews_string <- paste(reviews_string, first_part, '<a class="ui-tooltip" title = "Aspect: ', input_aspect, '&#013Polarity: ', input_polarity,'"<span style="cursor: help;"><span style="background-color: ', aspect_colour ,'">', review_extract,'</span></span></a>', colour_multiple_aspects_2(reviews_df, reviews_string, second_part, review_extracts, i+1), sep ='')}}

print_analysis_2 <- function(reviews_df, review){
  if(reviews_df == 'No matches.'){
    
    return(paste('<div style = "border-radius:8px; background-color: #ededed; border-style:solid; border-color:#b1d1fc;"><div style = "margin: 5px; padding: 12px">', '<b>ANALYSIS RESULT</b></br></br>', reviews_df, '</div></div></br>', sep =''))
  }
  else{
    reviews_extracts <- c()
    for (review_extract in reviews_df$extract_original){
      if(grepl(review_extract, review, fixed=TRUE)){
        reviews_extracts <- c(reviews_extracts, review_extract)
      }
    }
    
    reviews_body <- colour_multiple_aspects_2(reviews_df, '', review, reviews_extracts, 1)
    return(paste('<div style = "border-radius:8px; background-color: #ededed; border-style:solid; border-color:#b1d1fc;"><div style = "margin: 5px; padding: 12px">', '<b>ANALYSIS RESULT</b></br></br>', reviews_body, '</br></br><p style="color:green">Positive Aspects: <b>', return_aspect_polarity_2(reviews_df, 'Positive') , '</b></br><p style="color:red">Negative Aspects: <b>', return_aspect_polarity_2(reviews_df, 'Negative') , '</b></b></p></div></div></br>' ,sep =''))
    
  }}

print_trends_general <-function(n, pos_neg, input_category, datemin, datemax){
  reviews_df_date_subset <- reviews_df %>% filter(as.POSIXlt(date, "%Y-%m-%d") <= as.POSIXlt(datemax, "%d-%m-%Y") , as.POSIXlt(date, "%Y-%m-%d") >=as.POSIXlt(datemin, "%d-%m-%Y"))
  if(pos_neg == 'All'){
    if(input_category == 'All'){
      reviews_df <- reviews_df %>% filter(as.POSIXlt(date, "%Y-%m-%d") <= as.POSIXlt(datemax, "%d-%m-%Y") , as.POSIXlt(date, "%Y-%m-%d") >=as.POSIXlt(datemin, "%d-%m-%Y"))
    }
    else{
      reviews_df <- reviews_df %>% filter(category == input_category)
      reviews_df <- reviews_df %>% filter(as.POSIXlt(date, "%Y-%m-%d") <= as.POSIXlt(datemax, "%d-%m-%Y") , as.POSIXlt(date, "%Y-%m-%d") >=as.POSIXlt(datemin, "%d-%m-%Y"))
    }
  }
  else{
    if(input_category == 'All'){
      reviews_df <- reviews_df %>% filter(aspect_polarity == pos_neg)
      reviews_df <- reviews_df %>% filter(as.POSIXlt(date, "%Y-%m-%d") <= as.POSIXlt(datemax, "%d-%m-%Y") , as.POSIXlt(date, "%Y-%m-%d") >=as.POSIXlt(datemin, "%d-%m-%Y"))
    }
    else{
      reviews_df <- reviews_df %>% filter(aspect_polarity == pos_neg, category == input_category)
      reviews_df <- reviews_df %>% filter(as.POSIXlt(date, "%Y-%m-%d") <= as.POSIXlt(datemax, "%d-%m-%Y") , as.POSIXlt(date, "%Y-%m-%d") >=as.POSIXlt(datemin, "%d-%m-%Y"))
    }
  }
  aspect_trends <- reviews_df %>% count(aspect_polarity, aspect, review)
  aspect_trends$n <- 1
  aspect_trends_aggregate <- aggregate(list(count_aspect = aspect_trends$n), list(aspect_polarity = aspect_trends$aspect_polarity, aspect = aspect_trends$aspect), sum)
  aspect_trends_aggregate$count_aspect <- round(aspect_trends_aggregate$count_aspect/length(unique(reviews_df_date_subset$review)) *100, 2)
  aspect_trends_aggregate_top <- head(aspect_trends_aggregate %>% arrange(-count_aspect, aspect),n)
  count_aspect_print <- aspect_trends_aggregate_top[n, 'count_aspect']
  aspect_print <- aspect_trends_aggregate_top[n, 'aspect']
  polarity_print <- aspect_trends_aggregate_top[n, 'aspect_polarity']
  if(polarity_print == 'Positive'){
    color_to_print <- 'green'
  }
  else if(polarity_print == 'Negative'){
    color_to_print <- 'red'
  }
  text_to_print <- paste('<span style="color:', color_to_print, '">', count_aspect_print, '%</span> of clients give ','<span style="color:', color_to_print, '">', polarity_print, '</span> reviews about ', '<span style="color:', color_to_print, '">', aspect_print, '</span>', sep ='')
  return(text_to_print)
  
}

retrieve_trends_general <- function(n, pos_neg, input_category, datemin, datemax){
  if(pos_neg == 'All'){
    if(input_category == 'All'){
      reviews_df <- reviews_df %>% filter(as.POSIXlt(date, "%Y-%m-%d") <= as.POSIXlt(datemax, "%d-%m-%Y") , as.POSIXlt(date, "%Y-%m-%d") >=as.POSIXlt(datemin, "%d-%m-%Y"))
    }
    else{
      reviews_df <- reviews_df %>% filter(category == input_category) 
      reviews_df <- reviews_df %>% filter(as.POSIXlt(date, "%Y-%m-%d") <= as.POSIXlt(datemax, "%d-%m-%Y") , as.POSIXlt(date, "%Y-%m-%d") >=as.POSIXlt(datemin, "%d-%m-%Y"))
    }
  }
  else{
    if(input_category == 'All'){
      reviews_df <- reviews_df %>% filter(aspect_polarity == pos_neg)
      reviews_df <- reviews_df %>% filter(as.POSIXlt(date, "%Y-%m-%d") <= as.POSIXlt(datemax, "%d-%m-%Y") , as.POSIXlt(date, "%Y-%m-%d") >=as.POSIXlt(datemin, "%d-%m-%Y"))
    }
    else{
      reviews_df <- reviews_df %>% filter(aspect_polarity == pos_neg, category == input_category)
      reviews_df <- reviews_df %>% filter(as.POSIXlt(date, "%Y-%m-%d") <= as.POSIXlt(datemax, "%d-%m-%Y") , as.POSIXlt(date, "%Y-%m-%d") >=as.POSIXlt(datemin, "%d-%m-%Y"))
    }
  }
  aspect_trends <- reviews_df %>% count(aspect_polarity, aspect, review)
  aspect_trends$n <- 1
  aspect_trends_aggregate <- aggregate(list(count_aspect = aspect_trends$n), list(aspect_polarity = aspect_trends$aspect_polarity, aspect = aspect_trends$aspect), sum)
  aspect_trends_aggregate_top <- head(aspect_trends_aggregate %>% arrange(-count_aspect, aspect),n)
  return(aspect_trends_aggregate_top)}

retrieve_data_plot_categories_all <- function(pos_neg, datemin, datemax){
  reviews_df_date_subset <- reviews_df %>% filter(as.POSIXlt(date, "%Y-%m-%d") <= as.POSIXlt(datemax, "%d-%m-%Y") , as.POSIXlt(date, "%Y-%m-%d") >=as.POSIXlt(datemin, "%d-%m-%Y"))
  categories_count <- reviews_df %>% filter(as.POSIXlt(date, "%Y-%m-%d") <= as.POSIXlt(datemax, "%d-%m-%Y") , as.POSIXlt(date, "%Y-%m-%d") >=as.POSIXlt(datemin, "%d-%m-%Y")) %>% count(aspect_polarity, category, review)
  categories_count$n <- 1
  categories_count_aggregate <- aggregate(list(category_count = categories_count$n), list(aspect_polarity = categories_count$aspect_polarity, category = categories_count$category), sum)
  categories_count_aggregate$category_count <- round(categories_count_aggregate$category_count/length(unique(reviews_df_date_subset$review)) *100,2)
  if(pos_neg!='All'){
    categories_count_aggregate <- categories_count_aggregate %>% filter(aspect_polarity==pos_neg)
  }
  return(categories_count_aggregate)
}


plot_categories_all <- function(pos_neg, datemin, datemax){
  categories_count_aggregate <- retrieve_data_plot_categories_all(pos_neg, datemin, datemax)
  key = row.names(categories_count_aggregate)
  key_list = list()
  for(i in (1:length(key))){
    key_list[[i]] <- c(key[i], pos_neg)
  }
  if(pos_neg!='All'){
    plot_category_all_pos_neg <- ggplot(categories_count_aggregate, aes(x = category, y = category_count, fill = category, group = 1, key = key_list, text = paste('Sentiment: ' , pos_neg, '</br></br>Category: ', category, '</br>Category %: ', category_count))) + geom_bar(stat = 'identity', colour = 'black') +
      scale_fill_manual(values = c("black", "blue", "orange", "purple")) + 
      theme(plot.title = element_text(face ='bold'), legend.position='none', axis.text.x = element_text(angle = 90)) + 
      labs(x = 'Category' , y='Category percentage')
    plot_category_all_pos_neg <- ggplotly(, tooltip = c('text'), source = 'B')
    return(plot_category_all_pos_neg)
  }
  else{
    plot_category_all_all <- ggplot(categories_count_aggregate, aes(fill=aspect_polarity, y=category_count, x=category, key = key_list, text = paste('Sentiment: ' , aspect_polarity, '</br></br>Category: ', category, '</br>Category %: ', category_count))) + geom_bar(position="dodge", stat="identity") +
      theme(plot.title = element_text(face ='bold'), axis.text.x = element_text(angle = 90)) + 
      labs(x = 'Category' , y='Category percentage', fill = 'Sentiment')
    plot_category_all_all <- ggplotly(, tooltip = c('text'), source = 'B')
    return(plot_category_all_all)
  }
}

retrieve_data_plot_category_specific <- function(pos_neg, input_category, datemin, datemax){
  reviews_df_date_subset <- reviews_df %>% filter(as.POSIXlt(date, "%Y-%m-%d") <= as.POSIXlt(datemax, "%d-%m-%Y") , as.POSIXlt(date, "%Y-%m-%d") >=as.POSIXlt(datemin, "%d-%m-%Y"))
  reviews_df<- reviews_df %>% filter(category == input_category)
  reviews_df<- reviews_df %>% filter(as.POSIXlt(date, "%Y-%m-%d") <= as.POSIXlt(datemax, "%d-%m-%Y") , as.POSIXlt(date, "%Y-%m-%d") >=as.POSIXlt(datemin, "%d-%m-%Y"))
  aspect_count <- reviews_df %>% count(aspect_polarity, aspect, review)
  aspect_count$n <- 1
  aspect_count_aggregate <- aggregate(list(aspect_count = aspect_count$n), list(aspect_polarity = aspect_count$aspect_polarity, aspect = aspect_count$aspect), sum)
  aspect_count_aggregate$aspect_count <- round(aspect_count_aggregate$aspect_count/length(unique(reviews_df_date_subset$review))*100, 2)
  if(pos_neg!='All'){
    aspect_count_aggregate <- aspect_count_aggregate %>% filter(aspect_polarity == pos_neg)
    aspect_counter_df_top <- head(aspect_count_aggregate %>% arrange(-aspect_count, aspect), 10)
  }
  return(aspect_counter_df_top)
}

plot_category_specific <- function(pos_neg, input_category, datemin, datemax){
  if(pos_neg!='All'){
    aspect_counter_df_top <- retrieve_data_plot_category_specific(pos_neg, input_category, datemin, datemax)
    key = row.names(aspect_counter_df_top)
    key_list = list()
    for(i in (1:length(key))){
      key_list[[i]] <- c(key[i], pos_neg, input_category)
    }
    plot_category_specific_pos_neg <- ggplot(aspect_counter_df_top, aes(x = reorder(aspect, -aspect_count), y = aspect_count, fill = aspect, key = key_list, text = paste('Sentiment: ' , pos_neg, '</br></br>Aspect: ', aspect, '</br>Aspect %: ', aspect_count))) + geom_bar(stat = 'identity', colour = 'black') +
      theme(plot.title = element_text(face ='bold'), legend.position='none', axis.text.x = element_text(angle = 90)) + 
      labs(x = 'Aspect' , y='Aspect percentage')
    plot_category_specific_pos_neg <- ggplotly(, tooltip = c('text'), source = 'A')
    return(plot_category_specific_pos_neg)
  }
  else{
    return(subplot(plot_category_specific('Positive', input_category, datemin, datemax), plot_category_specific('Negative', input_category, datemin, datemax), nrows =2, margin = 0.15))
  }
}

retrieve_aspect <-function(pos_neg, input_category, datemin, datemax){
  if(pos_neg == 'All'){
    if(input_category == 'All'){
      reviews_df <- reviews_df %>% filter(as.POSIXlt(date, "%Y-%m-%d") <= as.POSIXlt(datemax, "%d-%m-%Y") , as.POSIXlt(date, "%Y-%m-%d") >=as.POSIXlt(datemin, "%d-%m-%Y"))
    }
    else{
      reviews_df <- reviews_df %>% filter(category == input_category)
      reviews_df <- reviews_df %>% filter(as.POSIXlt(date, "%Y-%m-%d") <= as.POSIXlt(datemax, "%d-%m-%Y") , as.POSIXlt(date, "%Y-%m-%d") >=as.POSIXlt(datemin, "%d-%m-%Y"))
    }
  }
  else{
    if(input_category == 'All'){
      reviews_df <- reviews_df %>% filter(aspect_polarity == pos_neg)
      reviews_df <- reviews_df %>% filter(as.POSIXlt(date, "%Y-%m-%d") <= as.POSIXlt(datemax, "%d-%m-%Y") , as.POSIXlt(date, "%Y-%m-%d") >=as.POSIXlt(datemin, "%d-%m-%Y"))
    }
    else{
      reviews_df <- reviews_df %>% filter(aspect_polarity == pos_neg, category == input_category)
      reviews_df <- reviews_df %>% filter(as.POSIXlt(date, "%Y-%m-%d") <= as.POSIXlt(datemax, "%d-%m-%Y") , as.POSIXlt(date, "%Y-%m-%d") >=as.POSIXlt(datemin, "%d-%m-%Y"))
    }
  }
  aspect_trends <- reviews_df %>% count(aspect_polarity, aspect, review)
  aspect_trends$n <- 1
  aspect_trends_aggregate <- aggregate(list(count_aspect = aspect_trends$n), list(aspect_polarity = aspect_trends$aspect_polarity, aspect = aspect_trends$aspect), sum)
  aspect_trends_aggregate<- aspect_trends_aggregate%>% arrange(-count_aspect, aspect)
  return(aspect_trends_aggregate)}

retrieve_aspect_df_month <- function(input_aspect, datemin, datemax){
  
  reviews_aspect_subset <- reviews_df %>% filter(aspect == input_aspect)
  reviews_aspect_subset <- reviews_aspect_subset %>% filter(as.POSIXlt(date, "%Y-%m-%d") <= as.POSIXlt(datemax, "%d-%m-%Y") , as.POSIXlt(date, "%Y-%m-%d") >=as.POSIXlt(datemin, "%d-%m-%Y"))
  max_date <- max(as.POSIXlt(reviews_aspect_subset$date, "%Y-%m-%d"))
  min_date <- min(as.POSIXlt(reviews_aspect_subset$date, "%Y-%m-%d"))
  monthly_dates <- seq(min_date, max_date, by = 'month')
  if (length(monthly_dates) > 24){
    breaks = '3 months'
  }
  else if(length(monthly_dates) < 13){
    
    breaks = '1 month'
  }
  else{
    breaks = '2 months'
  }
  count_monthly_aspect_df <- data.frame(month = rep(monthly_dates, 2), count_monthly_aspect = rep(0, 2 * length(monthly_dates)), aspect_polarity = c(rep(c('Positive'), length(monthly_dates)), rep(c('Negative'), length(monthly_dates))))
  count_monthly_aspect_df <- count_monthly_aspect_df %>% arrange(month)
  count_monthly_aspect_df$month <-paste('01-', month(count_monthly_aspect_df$month), '-', year(count_monthly_aspect_df$month), sep= '')
  for(input_review in unique(reviews_aspect_subset$review)){
    for(input_polarity in c('Positive', 'Negative')){
      reviews_aspect_reviews_subset <- reviews_aspect_subset %>% filter(review == input_review, aspect_polarity == input_polarity)
      if(nrow(reviews_aspect_reviews_subset)!= 0){
        input_month <- month(as.POSIXlt(as.character(reviews_aspect_reviews_subset$date)[1]))
        input_year <- year(as.POSIXlt(as.character(reviews_aspect_reviews_subset$date)[1]))
        input_date <- paste('01-', input_month, '-', input_year, sep= '')
        count_monthly_aspect_df[count_monthly_aspect_df$month == input_date & count_monthly_aspect_df$aspect_polarity == input_polarity , 'count_monthly_aspect'] <- count_monthly_aspect_df[count_monthly_aspect_df$month == input_date & count_monthly_aspect_df$aspect_polarity == input_polarity , 'count_monthly_aspect'][1] + 1
      }
    }
    
  }
  count_monthly_aspect_df <- count_monthly_aspect_df %>% mutate(month = as.Date(month, format = "%d-%m-%Y"))
  return(count_monthly_aspect_df)}

retrieve_aspect_df_day <- function(input_aspect, datemin, datemax){
  
  reviews_aspect_subset <- reviews_df %>% filter(aspect == input_aspect)
  reviews_aspect_subset <- reviews_aspect_subset %>% filter(as.POSIXlt(date, "%Y-%m-%d") <= as.POSIXlt(datemax, "%d-%m-%Y") , as.POSIXlt(date, "%Y-%m-%d") >=as.POSIXlt(datemin, "%d-%m-%Y"))
  max_date <- max(as.POSIXlt(reviews_aspect_subset$date, "%Y-%m-%d"))
  min_date <- min(as.POSIXlt(reviews_aspect_subset$date, "%Y-%m-%d"))
  monthly_dates <- seq(min_date, max_date, by = 'day')
  if (length(monthly_dates) > 24){
    breaks = '3 days'
  }
  else if(length(monthly_dates) < 13){
    
    breaks = '1 day'
  }
  else{
    breaks = '2 days'
  }
  count_monthly_aspect_df <- data.frame(day = rep(monthly_dates, 2), count_monthly_aspect = rep(0, 2 * length(monthly_dates)), aspect_polarity = c(rep(c('Positive'), length(monthly_dates)), rep(c('Negative'), length(monthly_dates))))
  count_monthly_aspect_df <- count_monthly_aspect_df %>% arrange(day)
  for(input_review in unique(reviews_aspect_subset$review)){
    for(input_polarity in c('Positive', 'Negative')){
      reviews_aspect_reviews_subset <- reviews_aspect_subset %>% filter(review == input_review, aspect_polarity == input_polarity)
      if(nrow(reviews_aspect_reviews_subset)!= 0){
        input_date <- as.POSIXlt(reviews_aspect_reviews_subset$date[1], "%Y-%m-%d")
        count_monthly_aspect_df[count_monthly_aspect_df$day == input_date & count_monthly_aspect_df$aspect_polarity == input_polarity , 'count_monthly_aspect'] <- count_monthly_aspect_df[count_monthly_aspect_df$day == input_date & count_monthly_aspect_df$aspect_polarity == input_polarity , 'count_monthly_aspect'][1] + 1
      }
    }
    
  }
  count_monthly_aspect_df <- count_monthly_aspect_df %>% mutate(day = as.Date(day, format = "%d-%m-%Y"))
  return(count_monthly_aspect_df)}

plot_aspect_daily_line <- function(input_aspect, datemin, datemax){
  
  reviews_aspect_subset <- reviews_df %>% filter(aspect == input_aspect)
  reviews_aspect_subset <- reviews_aspect_subset %>% filter(as.POSIXlt(date, "%Y-%m-%d") <= as.POSIXlt(datemax, "%d-%m-%Y") , as.POSIXlt(date, "%Y-%m-%d") >=as.POSIXlt(datemin, "%d-%m-%Y"))
  max_date <- max(as.POSIXlt(reviews_aspect_subset$date, "%Y-%m-%d"))
  min_date <- min(as.POSIXlt(reviews_aspect_subset$date, "%Y-%m-%d"))
  monthly_dates <- seq(min_date, max_date, by = 'day')
  if (length(monthly_dates) > 24){
    breaks = '3 days'
  }
  else if(length(monthly_dates) < 13){
    
    breaks = '1 day'
  }
  else{
    breaks = '2 days'
  }
  count_monthly_aspect_df <- data.frame(day = rep(monthly_dates, 2), count_monthly_aspect = rep(0, 2 * length(monthly_dates)), aspect_polarity = c(rep(c('Positive'), length(monthly_dates)), rep(c('Negative'), length(monthly_dates))))
  count_monthly_aspect_df <- count_monthly_aspect_df %>% arrange(day)
  for(input_review in unique(reviews_aspect_subset$review)){
    for(input_polarity in c('Positive', 'Negative')){
      reviews_aspect_reviews_subset <- reviews_aspect_subset %>% filter(review == input_review, aspect_polarity == input_polarity)
      if(nrow(reviews_aspect_reviews_subset)!= 0){
        input_date <- as.POSIXlt(reviews_aspect_reviews_subset$date[1], "%Y-%m-%d")
        count_monthly_aspect_df[count_monthly_aspect_df$day == input_date & count_monthly_aspect_df$aspect_polarity == input_polarity , 'count_monthly_aspect'] <- count_monthly_aspect_df[count_monthly_aspect_df$day == input_date & count_monthly_aspect_df$aspect_polarity == input_polarity , 'count_monthly_aspect'][1] + 1
      }
    }
    
  }
  count_monthly_aspect_df <- count_monthly_aspect_df %>% mutate(day = as.Date(day, format = "%d-%m-%Y"))
  key_positive = row.names(count_monthly_aspect_df[count_monthly_aspect_df$aspect_polarity == 'Positive', ])
  key_negative = row.names(count_monthly_aspect_df[count_monthly_aspect_df$aspect_polarity == 'Negative', ])
  key_list_positive = list()
  for(i in (1:length(key_positive))){
    key_list_positive[[i]] <- c(key_positive[i], input_aspect)
  }
  key_list_negative = list()
  for(i in (1:length(key_negative))){
    key_list_negative[[i]] <- c(key_negative[i], input_aspect)
  }
  plot_aspect_evolution<- ggplot() +
    geom_line(data = count_monthly_aspect_df[count_monthly_aspect_df$aspect_polarity == 'Positive', ], aes(x=day, y = count_monthly_aspect, key = key_list_positive, color = "green", group = 1, text = paste('Sentiment: ' , aspect_polarity, '</br></br>Day: ', format(as.Date(day, format = "%d-%m-%Y"), "%d-%m-%Y"), '</br>Aspect %: ', count_monthly_aspect) )) +
    geom_line(data = count_monthly_aspect_df[count_monthly_aspect_df$aspect_polarity == 'Negative', ], aes(x=day, y = count_monthly_aspect, key = key_list_negative, color = "red", group = 1, text = paste('Sentiment: ' , aspect_polarity, '</br></br>Day: ', format(as.Date(day, format = "%d-%m-%Y"), "%d-%m-%Y"), '</br>Aspect %: ', count_monthly_aspect))) +
    labs(x = 'Day' , y='Aspect percentage') +
    scale_color_identity(name = "Sentiment",
                         breaks = c("green", "red"),
                         labels = c('Positive', 'Negative'),
                         guide = "legend") + theme(plot.title = element_text(face ='bold'), axis.text.x = element_text(face="bold", color="#993333",size=10, angle=90)) +
    scale_x_date(breaks = breaks, date_labels = "%d-%m")
  plot_aspect_evolution <- ggplotly(, tooltip = c('text'), source = 'C')
  plot_aspect_evolution$x$data[[1]]$name <- 'Positive'
  plot_aspect_evolution$x$data[[2]]$name <- 'Negative'
  return(plot_aspect_evolution)}

plot_aspect_specific_pos_neg_line <- function(input_aspect, datemin, datemax){
  
  reviews_aspect_subset <- reviews_df %>% filter(aspect == input_aspect)
  reviews_aspect_subset <- reviews_aspect_subset %>% filter(as.POSIXlt(date, "%Y-%m-%d") <= as.POSIXlt(datemax, "%d-%m-%Y") , as.POSIXlt(date, "%Y-%m-%d") >=as.POSIXlt(datemin, "%d-%m-%Y"))
  max_date <- max(as.POSIXlt(reviews_aspect_subset$date, "%Y-%m-%d"))
  min_date <- min(as.POSIXlt(reviews_aspect_subset$date, "%Y-%m-%d"))
  monthly_dates <- seq(min_date, max_date, by = 'month')
  if (length(monthly_dates) > 24){
    breaks = '3 months'
  }
  else if(length(monthly_dates) < 13){
    
    breaks = '1 month'
  }
  else{
    breaks = '2 months'
  }
  count_monthly_aspect_df <- data.frame(month = rep(monthly_dates, 2), count_monthly_aspect = rep(0, 2 * length(monthly_dates)), aspect_polarity = c(rep(c('Positive'), length(monthly_dates)), rep(c('Negative'), length(monthly_dates))))
  count_monthly_aspect_df <- count_monthly_aspect_df %>% arrange(month)
  count_monthly_aspect_df$month <-paste('01-', month(count_monthly_aspect_df$month), '-', year(count_monthly_aspect_df$month), sep= '')
  for(input_review in unique(reviews_aspect_subset$review)){
    for(input_polarity in c('Positive', 'Negative')){
      reviews_aspect_reviews_subset <- reviews_aspect_subset %>% filter(review == input_review, aspect_polarity == input_polarity)
      if(nrow(reviews_aspect_reviews_subset)!= 0){
        input_month <- month(as.POSIXlt(as.character(reviews_aspect_reviews_subset$date)[1]))
        input_year <- year(as.POSIXlt(as.character(reviews_aspect_reviews_subset$date)[1]))
        input_date <- paste('01-', input_month, '-', input_year, sep= '')
        count_monthly_aspect_df[count_monthly_aspect_df$month == input_date & count_monthly_aspect_df$aspect_polarity == input_polarity , 'count_monthly_aspect'] <- count_monthly_aspect_df[count_monthly_aspect_df$month == input_date & count_monthly_aspect_df$aspect_polarity == input_polarity , 'count_monthly_aspect'][1] + 1
      }
    }
    
  }
  count_monthly_aspect_df <- count_monthly_aspect_df %>% mutate(month = as.Date(month, format = "%d-%m-%Y"))
  key_positive = row.names(count_monthly_aspect_df[count_monthly_aspect_df$aspect_polarity == 'Positive', ])
  key_negative = row.names(count_monthly_aspect_df[count_monthly_aspect_df$aspect_polarity == 'Negative', ])
  key_list_positive = list()
  for(i in (1:length(key_positive))){
    key_list_positive[[i]] <- c(key_positive[i], input_aspect)
  }
  key_list_negative = list()
  for(i in (1:length(key_negative))){
    key_list_negative[[i]] <- c(key_negative[i], input_aspect)
  }
  plot_aspect_evolution<- ggplot() +
    geom_line(data = count_monthly_aspect_df[count_monthly_aspect_df$aspect_polarity == 'Positive', ], aes(x=month, y = count_monthly_aspect, key = key_list_positive, color = "green", group = 1, text = paste('Sentiment: ' , aspect_polarity, '</br></br>Month: ', format(as.Date(month, format = "%d-%m-%Y"), "%b %Y"), '</br>Aspect %: ', count_monthly_aspect) )) +
    geom_line(data = count_monthly_aspect_df[count_monthly_aspect_df$aspect_polarity == 'Negative', ], aes(x=month, y = count_monthly_aspect, key = key_list_negative, color = "red", group = 1, text = paste('Sentiment: ' , aspect_polarity, '</br></br>Month: ', format(as.Date(month, format = "%d-%m-%Y"), "%b %Y"), '</br>Aspect %: ', count_monthly_aspect))) +
    labs(x = 'Month' , y='Aspect percentage') +
    scale_color_identity(name = "Sentiment",
                         breaks = c("green", "red"),
                         labels = c('Positive', 'Negative'),
                         guide = "legend") + theme(plot.title = element_text(face ='bold'), axis.text.x = element_text(face="bold", color="#993333",size=10, angle=90)) +
    scale_x_date(breaks = breaks, date_labels = "%b %Y")
  plot_aspect_evolution <- ggplotly(, tooltip = c('text'), source = 'C')
  plot_aspect_evolution$x$data[[1]]$name <- 'Positive'
  plot_aspect_evolution$x$data[[2]]$name <- 'Negative'
  return(plot_aspect_evolution)}

retrieve_reviews_aspect <- function(reviews_df, input_polarity, input_aspect, datemin, datemax, n, response_status, input_category, activate_analysis, color_representation, input_source){
  if(input_source != 'All'){
    reviews_df <- reviews_df %>% filter(source == input_source)
  }
  if(input_category != 'N/A' & input_category != 'All'){
    
    
    reviews_df <- reviews_df %>% filter(category == input_category)
    
  }
  if(response_status == 'Not answered'){
    reviews_df <- reviews_df %>% filter(time_response == 'NO')
  }
  else if(response_status =='Answered'){
    reviews_df <- reviews_df %>% filter(time_response != 'NO')
  }
  id_button = 0
  reviews_top_polarity_aspect_df = data.frame()
  i = 1
  if(input_aspect!= 'All'){
  if(input_polarity!='All'){
    reviews_subset <- reviews_df %>% filter(aspect_polarity == input_polarity, aspect == input_aspect)
    reviews_subset <- reviews_subset %>% filter(as.POSIXlt(date, "%Y-%m-%d") <= as.POSIXlt(datemax, "%d-%m-%Y") , as.POSIXlt(date, "%Y-%m-%d") >=as.POSIXlt(datemin, "%d-%m-%Y"))}
  else{
    reviews_subset <- reviews_df %>% filter(aspect == input_aspect)
    reviews_subset <- reviews_subset %>% filter(as.POSIXlt(date, "%Y-%m-%d") <= as.POSIXlt(datemax, "%d-%m-%Y") , as.POSIXlt(date, "%Y-%m-%d") >=as.POSIXlt(datemin, "%d-%m-%Y"))
  }
  reviews_subset_ordered <- reviews_subset %>% arrange(desc(date))
  for(input_review in unique(reviews_subset_ordered$review)){
    review_extract <- as.character(reviews_subset_ordered[reviews_subset_ordered$review == input_review, 'review_extract'][1])
    review_date <- as.character(reviews_subset_ordered[reviews_subset_ordered$review == input_review, 'date'][1])
    review_file <- as.character(reviews_subset_ordered[reviews_subset_ordered$review == input_review, 'file'][1])
    review_time_response <- as.character(reviews_subset_ordered[reviews_subset_ordered$review == input_review, 'time_response'][1])
    review_response <- as.character(reviews_subset_ordered[reviews_subset_ordered$review == input_review, 'response'][1])
    review_category <- as.character(reviews_subset_ordered[reviews_subset_ordered$review == input_review, 'category'][1])
    review_source <- as.character(reviews_subset_ordered[reviews_subset_ordered$review == input_review, 'source'][1])
    review_user <- as.character(reviews_subset_ordered[reviews_subset_ordered$review == input_review, 'user'][1])
    
    if(i!=1){
      if(!input_review %in% reviews_top_polarity_aspect_df$review){
        reviews_top_polarity_aspect_df[i, 'review'] = input_review
        reviews_top_polarity_aspect_df[i, 'review_extract'] = review_extract
        reviews_top_polarity_aspect_df[i, 'review_date'] = review_date
        reviews_top_polarity_aspect_df[i, 'review_file'] = review_file
        reviews_top_polarity_aspect_df[i, 'time_response'] = review_time_response
        reviews_top_polarity_aspect_df[i, 'response'] = review_response
        reviews_top_polarity_aspect_df[i, 'category'] = review_category
        reviews_top_polarity_aspect_df[i, 'source'] = review_source
        reviews_top_polarity_aspect_df[i, 'user'] = review_user
        i = i + 1
      }}
    else{
      reviews_top_polarity_aspect_df[1, 'review'] = input_review
      reviews_top_polarity_aspect_df[1, 'review_extract'] = review_extract
      reviews_top_polarity_aspect_df[i, 'review_date'] = review_date
      reviews_top_polarity_aspect_df[1, 'review_file'] = review_file
      reviews_top_polarity_aspect_df[1, 'time_response'] = review_time_response
      reviews_top_polarity_aspect_df[1, 'response'] = review_response
      reviews_top_polarity_aspect_df[1, 'category'] = review_category
      reviews_top_polarity_aspect_df[1, 'source'] = review_source
      reviews_top_polarity_aspect_df[1, 'user'] = review_user
      i = i + 1
    }}
  if(nrow(reviews_top_polarity_aspect_df) == length(unique(reviews_top_polarity_aspect_df$review))){
    reviews_string <- ''
    for(final_review in unique(reviews_top_polarity_aspect_df$review)){
      id_button = id_button + 1
      review_date = as.character(reviews_top_polarity_aspect_df[reviews_top_polarity_aspect_df$review == final_review, 'review_date'][1])
      review_extract = as.character(reviews_top_polarity_aspect_df[reviews_top_polarity_aspect_df$review == final_review, 'review_extract'][1])
      review_file = as.character(reviews_top_polarity_aspect_df[reviews_top_polarity_aspect_df$review == final_review, 'review_file'][1])
      review_time_response <- as.character(reviews_top_polarity_aspect_df[reviews_top_polarity_aspect_df$review == final_review, 'time_response'][1])
      review_response <- as.character(reviews_top_polarity_aspect_df[reviews_top_polarity_aspect_df$review == final_review, 'response'][1])
      review_category <- as.character(reviews_top_polarity_aspect_df[reviews_top_polarity_aspect_df$review == final_review, 'category'][1])
      review_source <- as.character(reviews_top_polarity_aspect_df[reviews_top_polarity_aspect_df$review == final_review, 'source'][1])
      review_user <- as.character(reviews_top_polarity_aspect_df[reviews_top_polarity_aspect_df$review == final_review, 'user'][1])
      audio_url <- paste('gs://tu-vieja-en-recontra-tanga/', review_file, sep='')
      audio_save_path <- paste('www/', review_file, sep='')
      #if(!file.exists(audio_save_path)){
        #gcs_get_object(audio_url, saveToDisk = audio_save_path)}
      
      review_split <- str_split(final_review, review_extract, simplify = TRUE)
      if(length(review_split)>1){
        first_part <- review_split[1, 1]
        second_part <- review_split[1,2]
        if(review_time_response=='NO'){
          if(review_source == 'Voice Reviews App'){
          reviews_string <- paste(reviews_string, '<div style = "border-radius:8px; background-color: #d7fffe; border-style:solid; border-color:#b1d1fc;"><div style = "margin: 20px;">', '<img src="voice_logo.png" alt="logo" width="50" height="62.5" /></br></br><b>Date: ', format(as.Date(review_date, format = "%Y-%m-%d"), "%d-%m-%Y"), '</b><br/>','<b>Source: ', review_source,'</b><br/><b>User: ', review_user,'</b><br/>','<br/><b>Message: </b>',  first_part, '<a class="ui-tooltip" title = "Category: ', review_category, '&#013Aspect: ', input_aspect, '&#013Polarity: ', input_polarity,'"<span style="cursor: help;"><span style="background-color: #FFFF00">', review_extract, '</span></span></a>', second_part, '<br/><audio src="', review_file, '" type="audio/wav" controls></audio><br/><br/><button class="btn action-button" type="button" id="button_',id_button,'response"style=" border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #b1d1fc">Respond Review</button></div></div><br/><br/>', sep='')
          }
          else{
            reviews_string <- paste(reviews_string, '<div style = "border-radius:8px; background-color: #d7fffe; border-style:solid; border-color:#b1d1fc;"><div style = "margin: 20px;">',  '<img src="gmb_logo.png" alt="logo" width="125" height="75"/></br></br>', '<b>Date: ', format(as.Date(review_date, format = "%Y-%m-%d"), "%d-%m-%Y"), '</b><br/>','<b>Source: ', review_source,'</b><br/><b>User: ', review_user,'</b><br/>','<br/><b>Message: </b>',  first_part, '<a class="ui-tooltip" title = "Category: ', review_category, '&#013Aspect: ', input_aspect, '&#013Polarity: ', input_polarity,'"<span style="cursor: help;"><span style="background-color: #FFFF00">', review_extract, '</span></span></a>', second_part, '<br/><br/><button class="btn action-button" type="button" id="button_',id_button,'response" style=" border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #b1d1fc">Respond Review</button></div></div><br/><br/>', sep='')
          }
          }
        else{
          if(review_source == 'Voice Reviews App'){
          reviews_string <- paste(reviews_string, '<div style = "border-radius:8px; background-color: #d7fffe; border-style:solid; border-color:#b1d1fc;"><div style = "margin: 20px;">', '<img src="voice_logo.png" alt="logo" width="50" height="62.5" /></br></br><b>Date: ', format(as.Date(review_date, format = "%Y-%m-%d"), "%d-%m-%Y"), '</b><br/>', '<b>Source: ', review_source,'</b><br/><b>User: ', review_user,'</b><br/>', '<br/><b>Message: </b>', first_part, '<a class="ui-tooltip" title = "Category: ', review_category, '&#013Aspect: ', input_aspect, '&#013Polarity: ', input_polarity,'"<span style="cursor: help;"><span style="background-color: #FFFF00">', review_extract, '</span></span></a>', second_part, '<br/><audio src="', review_file, '" type="audio/wav" controls></audio><br/><span style="color:red">Review answered on ', review_time_response, '&emsp;</span><button class="btn action-button" type="button" id="button_',id_button,'history"style=" border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #b1d1fc">View history</button><br/><br/></div></div></br></br>', sep='')
          }
          else{
            reviews_string <- paste(reviews_string, '<div style = "border-radius:8px; background-color: #d7fffe; border-style:solid; border-color:#b1d1fc;"><div style = "margin: 20px;">',  '<img src="gmb_logo.png" alt="logo" width="125" height="75" /></br></br>', '<b>Date: ', format(as.Date(review_date, format = "%Y-%m-%d"), "%d-%m-%Y"), '</b><br/>', '<b>Source: ', review_source,'</b><br/><b>User: ', review_user,'</b><br/>', '<br/><b>Message: </b>', first_part, '<a class="ui-tooltip" title = "Category: ', review_category, '&#013Aspect: ', input_aspect, '&#013Polarity: ', input_polarity,'"<span style="cursor: help;"><span style="background-color: #FFFF00">', review_extract, '</span></span></a>', second_part, '<span style="color:red">Review answered on ', review_time_response, '&emsp;</span><button class="btn action-button" type="button" id="button_',id_button,'history"style=" border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #b1d1fc">View history</button></div></div><br/><br/>', sep='')
          }
          
          }
      }
      else{
        if(review_time_response=='NO'){
          if(review_source == 'Voice Reviews App'){
          reviews_string <- paste(reviews_string, '<div style = "border-radius:8px; background-color: #d7fffe; border-style:solid; border-color:#b1d1fc;"><div style = "margin: 20px;">', '<img src="voice_logo.png" alt="logo" width="50" height="62.5" /></br></br><b>Date: ', format(as.Date(review_date, format = "%Y-%m-%d"), "%d-%m-%Y"), '</b><br/>', '<b>Source: ', review_source,'</b><br/><b>User: ', review_user,'</b><br/>', '<br/><b>Message: </b>', final_review, '<br/><audio src="', review_file, '" type="audio/wav" controls></audio><br/><br/><button class="btn action-button" type="button" id="button_',id_button,'response"style=" border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #b1d1fc">Respond Review</button><br/><br/></div></div></br></br>', sep ='')
          }
          else{
            reviews_string <- paste(reviews_string, '<div style = "border-radius:8px; background-color: #d7fffe; border-style:solid; border-color:#b1d1fc;"><div style = "margin: 20px;">',  '<img src="gmb_logo.png" alt="logo" width="125" height="75"/></br></br>', '<b>Date: ', format(as.Date(review_date, format = "%Y-%m-%d"), "%d-%m-%Y"), '</b><br/>', '<b>Source: ', review_source,'</b><br/><b>User: ', review_user,'</b><br/>', '<br/><b>Message: </b>', final_review, '<br/><br/><button class="btn action-button" type="button" id="button_',id_button,'response"style=" border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #b1d1fc">Respond Review</button></div></div><br/><br/>', sep ='')
          }
        }
        else{
          if(review_source == 'Voice Reviews App'){
          reviews_string <- paste('<div style = "border-radius:8px; background-color: #d7fffe; border-style:solid; border-color:#b1d1fc;"><div style = "margin: 20px;">',reviews_string, '<img src="voice_logo.png" alt="logo" width="50" height="62.5" /></br></br><b>Date: ', format(as.Date(review_date, format = "%Y-%m-%d"), "%d-%m-%Y"), '</b><br/>', '<b>Source: ', review_source,'</b><br/><b>User: ', review_user,'</b><br/>', '<br/><b>Message: </b>', final_review, '<br/><audio src="', review_file, '" type="audio/wav" controls></audio><br/><span style="color:red">Review answered on ', review_time_response, '&emsp;</span><button class="btn action-button" type="button" id="button_',id_button,'history"style=" border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #b1d1fc">View history</button><br/><br/></div></div></br></br>', sep ='')
          }
          else{
            reviews_string <- paste(reviews_string, '<div style = "border-radius:8px; background-color: #d7fffe; border-style:solid; border-color:#b1d1fc;"><div style = "margin: 20px;">',  '<img src="gmb_logo.png" alt="logo" width="125" height="75"/></br></br>', '<b>Date: ', format(as.Date(review_date, format = "%Y-%m-%d"), "%d-%m-%Y"), '</b><br/>', '<b>Source: ', review_source,'</b><br/><b>User: ', review_user,'</b><br/>', '<br/><b>Message: </b>', final_review, '<span style="color:red">Review answered on ', review_time_response, '<br/><br/></span><button class="btn action-button" type="button" id="button_',id_button,'history" style=" border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #b1d1fc">View history</button></div></div><br/><br/>', sep ='')
          }
          
          }
      }
    }}}
  
  else{
    if(input_polarity!='All'){
      reviews_subset <- reviews_df %>% filter(aspect_polarity == input_polarity)
      reviews_subset <- reviews_subset %>% filter(as.POSIXlt(date, "%Y-%m-%d") <= as.POSIXlt(datemax, "%d-%m-%Y") , as.POSIXlt(date, "%Y-%m-%d") >=as.POSIXlt(datemin, "%d-%m-%Y"))}
    else{
      reviews_subset <- reviews_df
      reviews_subset <- reviews_subset %>% filter(as.POSIXlt(date, "%Y-%m-%d") <= as.POSIXlt(datemax, "%d-%m-%Y") , as.POSIXlt(date, "%Y-%m-%d") >=as.POSIXlt(datemin, "%d-%m-%Y"))
    }
    reviews_subset_ordered <- reviews_subset %>% arrange(desc(date))
    for(input_review in unique(reviews_subset_ordered$review)){
      review_date <- as.character(reviews_subset_ordered[reviews_subset_ordered$review == input_review, ][1, 'date'])
      review_file <- as.character(reviews_subset_ordered[reviews_subset_ordered$review == input_review, ][1, 'file'])
      review_time_response <- as.character(reviews_subset_ordered[reviews_subset_ordered$review == input_review, ][1, 'time_response'])
      review_response <- as.character(reviews_subset_ordered[reviews_subset_ordered$review == input_review, ][1, 'response'])
      review_category <- as.character(reviews_subset_ordered[reviews_subset_ordered$review == input_review, ][1, 'category'])
      review_source <- as.character(reviews_subset_ordered[reviews_subset_ordered$review == input_review, ][1, 'source'])
      review_user <- as.character(reviews_subset_ordered[reviews_subset_ordered$review == input_review, ][1, 'user'])
      if(i!=1){
        if(!input_review %in% reviews_top_polarity_aspect_df$review){
          reviews_top_polarity_aspect_df[i, 'review'] = input_review
          reviews_top_polarity_aspect_df[i, 'review_date'] = review_date
          reviews_top_polarity_aspect_df[i, 'review_file'] = review_file
          reviews_top_polarity_aspect_df[i, 'time_response'] = review_time_response
          reviews_top_polarity_aspect_df[i, 'response'] = review_response
          reviews_top_polarity_aspect_df[i, 'category'] = review_category
          reviews_top_polarity_aspect_df[i, 'source'] = review_source
          reviews_top_polarity_aspect_df[i, 'user'] = review_user
          i = i + 1
        }}
      else{
        reviews_top_polarity_aspect_df[1, 'review'] = input_review
        reviews_top_polarity_aspect_df[1, 'review_date'] = review_date
        reviews_top_polarity_aspect_df[1, 'review_file'] = review_file
        reviews_top_polarity_aspect_df[1, 'time_response'] = review_time_response
        reviews_top_polarity_aspect_df[1, 'response'] = review_response
        reviews_top_polarity_aspect_df[1, 'category'] = review_category
        reviews_top_polarity_aspect_df[1, 'source'] = review_source
        reviews_top_polarity_aspect_df[1, 'user'] = review_user
        i = i + 1
      }}
    if(activate_analysis == FALSE){
      reviews_string <- ''
      for(final_review in unique(reviews_top_polarity_aspect_df$review)){
        id_button = id_button + 1
        review_date = as.character(reviews_top_polarity_aspect_df[reviews_top_polarity_aspect_df$review == final_review, 'review_date'][1])
        review_file = as.character(reviews_top_polarity_aspect_df[reviews_top_polarity_aspect_df$review == final_review, 'review_file'][1])
        review_time_response <- as.character(reviews_top_polarity_aspect_df[reviews_top_polarity_aspect_df$review == final_review, 'time_response'][1])
        review_response <- as.character(reviews_top_polarity_aspect_df[reviews_top_polarity_aspect_df$review == final_review, 'response'][1])
        review_category <- as.character(reviews_top_polarity_aspect_df[reviews_top_polarity_aspect_df$review == final_review, 'category'][1])
        review_source <- as.character(reviews_top_polarity_aspect_df[reviews_top_polarity_aspect_df$review == final_review, 'source'][1])
        review_user <- as.character(reviews_top_polarity_aspect_df[reviews_top_polarity_aspect_df$review == final_review, 'user'][1])
        audio_url <- paste('gs://tu-vieja-en-recontra-tanga/', review_file, sep='')
        print(audio_url)
        audio_save_path <- paste('www/', review_file, sep='')
        print(audio_save_path)
        #if(!file.exists(audio_save_path)){
          #gcs_get_object(audio_url, saveToDisk = audio_save_path)}

          if(review_time_response=='NO'){
            if(review_source == 'Voice Reviews App'){
            reviews_string <- paste(reviews_string, '<div style = "border-radius:8px; background-color: #d7fffe; border-style:solid; border-color:#b1d1fc;"><div style = "margin: 20px;">', '<img src="voice_logo.png" alt="logo" width="50" height="62.5" /></br></br><b>Date: ', format(as.Date(review_date, format = "%Y-%m-%d"), "%d-%m-%Y"), '</b><br/>', '<b>Source: ', review_source,'</b><br/><b>User: ', review_user,'</b><br/>', '<br/><b>Message: </b>', final_review, '<br/><audio src="', review_file, '" type="audio/wav" controls></audio><br/><br/><button class="btn action-button" type="button" id="button_',id_button,'response" style=" border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #b1d1fc">Respond Review</button><br/><br/></div></div></br></br>', sep ='')
            }
            else{
              reviews_string <- paste(reviews_string, '<div style = "border-radius:8px; background-color: #d7fffe; border-style:solid; border-color:#b1d1fc;"><div style = "margin: 20px;">',  '<img src="gmb_logo.png" alt="logo" width="125" height="75" /></br></br>', '<b>Date: ', format(as.Date(review_date, format = "%Y-%m-%d"), "%d-%m-%Y"), '</b><br/>', '<b>Source: ', review_source,'</b><br/><b>User: ', review_user,'</b><br/>', '<br/><b>Message: </b>', final_review, '<br/><br/><button class="btn action-button" type="button" id="button_',id_button,'response" style=" border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #b1d1fc">Respond Review</button></div></div><br/><br/>', sep ='')
            }
            
            }
          else{
            if(review_source == 'Voice Reviews App'){
            reviews_string <- paste(reviews_string, '<div style = "border-radius:8px; background-color: #d7fffe; border-style:solid; border-color:#b1d1fc;"><div style = "margin: 20px;">', '<img src="voice_logo.png" alt="logo" width="50" height="62.5" /></br></br><b>Date: ', format(as.Date(review_date, format = "%Y-%m-%d"), "%d-%m-%Y"), '</b><br/>', '<b>Source: ', review_source,'</b><br/><b>User: ', review_user,'</b><br/>', '<br/><b>Message: </b>', final_review, '<br/><audio src="', review_file, '" type="audio/wav" controls></audio><br/><span style="color:red">Review answered on ', review_time_response, '&emsp;</span><button class="btn action-button" type="button" id="button_',id_button,'history"style=" border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #b1d1fc">View history</button><br/><br/></div></div></br></br>', sep ='')
            }
            else{
              reviews_string <- paste(reviews_string, '<div style = "border-radius:8px; background-color: #d7fffe; border-style:solid; border-color:#b1d1fc;"><div style = "margin: 20px;">',  '<img src="gmb_logo.png" alt="logo" width="125" height="75" /></br></br>', '<b>Date: ', format(as.Date(review_date, format = "%Y-%m-%d"), "%d-%m-%Y"), '</b><br/>', '<b>Source: ', review_source,'</b><br/><b>User: ', review_user,'</b><br/><br/><b>Message: </b>', final_review, '<br/><span style="color:red">Review answered on ', review_time_response, '<br/><br/></span><button class="btn action-button" type="button" id="button_',id_button,'history"style=" border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #b1d1fc">View history</button></div></div><br/><br/>', sep ='')
            }
            }
      }
    
    }
    
    else{
      reviews_string <- ''
      for(input_review in unique(reviews_subset_ordered$review)){
        id_button = id_button + 1
        reviews_extracts_dirty <- reviews_subset_ordered[reviews_subset_ordered$review == input_review, ]$review_extract
        reviews_extracts <- c()
        for (review_extract in reviews_extracts_dirty){
          if(grepl(review_extract, input_review, fixed=TRUE)){
            reviews_extracts <- c(reviews_extracts, review_extract)
          }
        }
        if(!is.null(reviews_extracts)){
        review_date <- as.character(reviews_subset_ordered[reviews_subset_ordered$review == input_review, ][1, 'date'])
        review_file <- as.character(reviews_subset_ordered[reviews_subset_ordered$review == input_review, ][1, 'file'])
        review_source <- as.character(reviews_subset_ordered[reviews_subset_ordered$review == input_review, ][1, 'source'])
        review_user <- as.character(reviews_subset_ordered[reviews_subset_ordered$review == input_review, ][1, 'user'])
        review_time_response <- as.character(reviews_subset_ordered[reviews_subset_ordered$review == input_review, ][1, 'time_response'])
        review_response <- as.character(reviews_subset_ordered[reviews_subset_ordered$review == input_review, ][1, 'response'])
        reviews_body <- colour_multiple_aspects(reviews_subset_ordered, '', input_review, reviews_extracts, 1, input_review, color_representation)
        audio_url <- paste('gs://tu-vieja-en-recontra-tanga/', review_file, sep='')
        print(audio_url)
        audio_save_path <- paste('www/', review_file, sep='')
        print(audio_save_path)
        # if(!file.exists(audio_save_path)){
          # gcs_get_object(audio_url, saveToDisk = audio_save_path)}
        
        if(review_time_response=='NO'){
          if(review_source == 'Voice Reviews App'){
          reviews_string <- paste(reviews_string, '<div style = "border-radius:8px; background-color: #d7fffe; border-style:solid; border-color:#b1d1fc;"><div style = "margin: 20px;">', '<img src="voice_logo.png" alt="logo" width="50" height="62.5" /></br></br><b>Date: ', format(as.Date(review_date, format = "%Y-%m-%d"), "%d-%m-%Y"), '</b><br/>', '<b>Source: ', review_source,'</b><br/><b>User: ', review_user,'</b><br/>', '<br/><b>Message: </b>', reviews_body, '<br/><audio src="', review_file, '" type="audio/wav" controls></audio><br/><br/><button class="btn action-button" type="button" id="button_',id_button,'response"style=" border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #b1d1fc">Respond Review</button><br/><br/></div></div></br></br>', sep ='')
          }
          else{
            reviews_string <- paste(reviews_string, '<div style = "border-radius:8px; background-color: #d7fffe; border-style:solid; border-color:#b1d1fc;"><div style = "margin: 20px;">',  '<img src="gmb_logo.png" alt="logo" width="125" height="75"/></br></br>', '<b>Date: ', format(as.Date(review_date, format = "%Y-%m-%d"), "%d-%m-%Y"), '</b><br/>', '<b>Source: ', review_source,'</b><br/><b>User: ', review_user,'</b><br/>', '<br/><b>Message: </b>', reviews_body, '<br/><br/><button class="btn action-button" type="button" id="button_',id_button,'response"style=" border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #b1d1fc">Respond Review</button></div></div><br/><br/>', sep ='')
          }
          }
        else{
          if(review_source == 'Voice Reviews App'){
          reviews_string <- paste(reviews_string, '<div style = "border-radius:8px; background-color: #d7fffe; border-style:solid; border-color:#b1d1fc;"><div style = "margin: 20px;">', '<img src="voice_logo.png" alt="logo" width="50" height="62.5" /></br></br><b>Date: ', format(as.Date(review_date, format = "%Y-%m-%d"), "%d-%m-%Y"), '</b><br/>', '<b>Source: ', review_source,'</b><br/><b>User: ', review_user,'</b><br/>', '<br/><b>Message: </b>', reviews_body, '<br/><audio src="', review_file, '" type="audio/wav" controls></audio><br/><span style="color:red">Review answered on ', review_time_response, '&emsp;</span><button class="btn action-button" type="button" id="button_',id_button,'history"style=" border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #b1d1fc">View history</button><br/><br/></div></div></br></br>', sep ='')
          }
          else{
            reviews_string <- paste(reviews_string, '<div style = "border-radius:8px; background-color: #d7fffe; border-style:solid; border-color:#b1d1fc;"><div style = "margin: 20px;">',  '<img src="gmb_logo.png" alt="logo" width="125" height="75"/></br></br>', '<b>Date: ', format(as.Date(review_date, format = "%Y-%m-%d"), "%d-%m-%Y"), '</b><br/>', '<b>Source: ', review_source,'</b><br/><b>User: ', review_user,'</b><br/>', '<br/><b>Message: </b>', reviews_body, '<br/><span style="color:red">Review answered on ', review_time_response, '<br/><br/></span><button class="btn action-button" type="button" id="button_',id_button,'history"style=" border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #b1d1fc">View history</button></div></div><br/><br/>', sep ='')
          }
          
          }
      }
      
    }
  }
  }
  if((n == 0 | length(unique(reviews_top_polarity_aspect_df$review))==0) & response_status == 'All' ){
    if(input_aspect != 'All'){
    reviews_string <- paste('There are no', input_polarity, 'reviews about', input_aspect, 'for the specified date.')
    }
    else{
      reviews_string <- paste('There are no', input_polarity, 'reviews about', input_category, 'for the specified date.')
    }
    }
  else if((n == 0 | length(unique(reviews_top_polarity_aspect_df$review))==0) & response_status == 'Answered'){
    if(input_aspect != 'All'){
    reviews_string <- paste('There are no answered', input_polarity, 'reviews about', input_aspect, 'for the specified date.')
    }
    else{
      reviews_string <- paste('There are no answered', input_polarity, 'reviews about', input_category, 'for the specified date.')
    }
    }
  else if((n == 0 | length(unique(reviews_top_polarity_aspect_df$review))==0) & response_status == 'Not answered'){
    if(input_aspect != 'All'){
    reviews_string <- paste('There are no ', input_polarity, 'reviews about', input_aspect, 'that have not been answered for the specified date.')
    }
    else{
      reviews_string <- paste('There are no ', input_polarity, 'reviews about', input_category, 'that have not been answered for the specified date.')
    }
    }
  return(list(reviews_string, reviews_top_polarity_aspect_df))
}


colour_multiple_aspects <- function(reviews_df, reviews_string, recursive_review, review_extracts, i, final_review, color_representation){
  print(review_extracts)
  review_extract <- review_extracts[i]
  input_aspect <- as.character(reviews_df[reviews_df$review==final_review & reviews_df$review_extract == review_extract, ][1, 'aspect'])
  review_category <- as.character(reviews_df[reviews_df$review==final_review & reviews_df$review_extract == review_extract, ][1, 'category'])
  input_polarity <- as.character(reviews_df[reviews_df$review==final_review & reviews_df$review_extract == review_extract, ][1, 'aspect_polarity'])
  if(color_representation == 'Category'){
  if(review_category == 'FOOD AND DRINKS'){
    aspect_colour <- '#FFFF00'
  }
  else if(review_category == 'SERVICE AND PRICING'){
    aspect_colour <- '#fed8b1'
  }
  else if(review_category == 'AMBIANCE AND DESIGN'){
    aspect_colour <- '#0080FF80'
  }
  }
  else{
    if(input_polarity == 'Positive'){
      aspect_colour <- '#00ff0080'
    }
    else if(input_polarity == 'Negative'){
      aspect_colour <- '#FF7F50'
    }
    
  }
  review_split <- str_split(recursive_review, review_extract, simplify = TRUE)
  print(review_split)
  if(length(review_split)>1){
    first_part <- review_split[1, 1]
    second_part <- review_split[1,2]
  }
  if (i == length(review_extracts)){
    return(paste(reviews_string, first_part, '<a class="ui-tooltip" title = "Category: ', review_category, '&#013Aspect: ', input_aspect, '&#013Polarity: ', input_polarity,'"<span style="cursor: help;"><span style="background-color: ', aspect_colour ,'">', review_extract,'</span></span></a>', second_part, sep = ''))
  }
  else{
    reviews_string <- paste(reviews_string, first_part, '<a class="ui-tooltip" title = "Category: ', review_category, '&#013Aspect: ', input_aspect, '&#013Polarity: ', input_polarity,'"<span style="cursor: help;"><span style="background-color: ', aspect_colour ,'">', review_extract,'</span></span></a>', colour_multiple_aspects(reviews_df, reviews_string, second_part, review_extracts, i+1, final_review, color_representation), sep ='')}}


count_reviews <- function(reviews_df, input_polarity, input_aspect, datemin, datemax, n, response_status, input_category, activate_analysis, color_representation, input_source){
  
  data_frame_to_count <- retrieve_reviews_aspect(reviews_df, input_polarity, input_aspect, datemin, datemax, n, response_status, input_category, activate_analysis, color_representation, input_source)[[2]]
  reviews_amount <- length(unique(data_frame_to_count$review))
  reviews_not_answered <- unique(length(data_frame_to_count[data_frame_to_count$time_response=='NO', ]$review))
  
  if(input_aspect == 'All'){
  html_count <- paste('<h5>', input_polarity, ' reviews for ', input_category, ' category from ', format(as.Date(datemin, format = "%Y-%m-%d"), "%d-%m-%Y"), ' to ', format(as.Date(datemax, format = "%Y-%m-%d"), "%d-%m-%Y"), '</h5></br><button class="btn action-button" type="button" id="buttonFilteredReviews" style=" border-radius: 8px; color: white; background-color: #4CAF50; border: 2px solid #555555"><b>TOTAL REVIEWS</b></button>&emsp;&emsp;&emsp;&emsp;&emsp;<span style="font-size:300%; color:green; position: relative; bottom:-10px">',reviews_amount, '</span></br></br><button class="btn action-button" type="button" id="buttonNotAnswered" style=" border-radius: 8px; color: white; background-color: #f44336; border: 2px solid #555555"><b>NOT ANSWERED</b> </button>&emsp;&emsp;&emsp;&emsp;&emsp;<span style="font-size:300%; color:red; position: relative; bottom:-10px">', reviews_not_answered, '</span></br></br>', sep ='')
  }
  else{
    html_count <- paste('<h5>', input_polarity, ' reviews for ', input_aspect, ' from ', format(as.Date(datemin, format = "%Y-%m-%d"), "%d-%m-%Y"), ' to ', format(as.Date(datemax, format = "%Y-%m-%d"), "%d-%m-%Y"), '</h5></br><button class="btn action-button" type="button" id="buttonFilteredReviews" style=" border-radius: 8px; color: white; background-color: #4CAF50; border: 2px solid #555555"><b>TOTAL REVIEWS</b></button>&emsp;&emsp;&emsp;&emsp;&emsp;<span style="font-size:300%; color:green; position: relative; bottom:-10px">',reviews_amount, '</span></br></br><button class="btn action-button" type="button" id="buttonNotAnswered" style=" border-radius: 8px; color: white; background-color: #f44336; border: 2px solid #555555"><b>NOT ANSWERED</b> </button>&emsp;&emsp;&emsp;&emsp;&emsp;<span style="font-size:300%; color:red; position: relative; bottom:-10px;">', reviews_not_answered, '</span></br></br>', sep ='')
  }
  return(html_count)
}

VIRTUALENV_NAME = '/home/ubuntu/env_2'

Sys.setenv(PYTHON_PATH = '/usr/bin/python3.8')
Sys.setenv(VIRTUALENV_NAME = paste0(VIRTUALENV_NAME, '/')) # include '/' => installs into rstudio-connect/apps/
Sys.setenv(RETICULATE_PYTHON = paste0(VIRTUALENV_NAME, '/bin/python3.8'))

server<-function(input, output, session) {
  
  virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
  python_path = Sys.getenv('PYTHON_PATH')
  # virtualenv_dir = 'nlp_trial'
  reticulate::use_python(python_path)
  
  # Create virtual env and install dependencies
  #reticulate::virtualenv_create(envname = 'nlp/trial')
  #reticulate::virtualenv_install(virtualenv_dir, packages = PYTHON_DEPENDENCIES, ignore_installed=TRUE)
  reticulate::use_virtualenv(virtualenv_dir, required = T)
  
  observeEvent(input$button_rev_ex, {
    if (input$review_2 != '') {
      reticulate::source_python("NLP_Project_2.py")
      show_modal_spinner(text = 'This might take a little while...') # show the modal window
      review_to_analyze <- input$review_2
      analyzed_review_df <- extract_sentiment_analysis(review_to_analyze)
      output$example_analysis <- renderText({print_analysis_2(analyzed_review_df, review_to_analyze)})
      remove_modal_spinner() }
  })
  
  observeEvent(input$review_example, {
    if (input$review_example == 'Hotel Review'){example = 'The hotel was outstanding! The decorations were amazing and the staff was extremely helpful. The prices were a little too much though but totally worth it! Completely recommendable experience!' }
    else if (input$review_example == 'Restaurant Review'){example = 'I went to Starbucks the other day and it was amazing! I had a latte which was exquisite! I also ordered a sandwich: the best I have ever tried! Highly recommend the place! The ambience was not the best though, it was really noisy.'}
    else if(input$review_example == 'Uni Review'){example = 'FIT5212 is a really good unit. The teachers were really helpful. I believe the students learned a lot, very useful Machine Learning tools. That being said, FIT5212 is pretty hard and has too much content so you need to stay focused the whole semester'}
    else if(input$review_example == 'App Review'){example = 'Voice Reviews works like a charm! It is the best customer service app on the market! It uses sophisticated Machine Learning techniques to provide a complete and accurate analysis of your reviews! Voice Reviews is still a little slow, but we are working on that. Bottomline: such a useful customer success tool to consider.'}
    else{example =''}
    updateTextAreaInput(session, 'review_2', paste(input$review_example, 'Example', sep=' '), example)
    output$example_analysis <- renderText({''})
  })
  
  observeEvent(input$review_2, {
    output$example_analysis <- renderText({''})
  })
  
  observeEvent(input$button_to_dashboard, {
    updateTabsetPanel(session,'displayedTab',selected = 'mainPanel')})
  
  output$text1 <- renderText(print_trends_general(1, input$polarity, input$category, input$dateRange[1], input$dateRange[2]))
  output$text2 <- renderText(print_trends_general(2, input$polarity, input$category, input$dateRange[1], input$dateRange[2]))
  output$text3 <- renderText(print_trends_general(3, input$polarity, input$category, input$dateRange[1], input$dateRange[2]))
  output$text4 <- renderText(print_trends_general(4, input$polarity, input$category, input$dateRange[1], input$dateRange[2]))
  output$text5 <- renderText(print_trends_general(5, input$polarity, input$category, input$dateRange[1], input$dateRange[2]))
  
  # Defining reactive element for mouse click in GDP markers
  observe({if(input$displayedTab == 'mainPanel'| input$displayedTab == 'reviewsPanel'|input$displayedTab == 'examplesPanel'){
    hideTab(inputId = 'displayedTab', target = 'responsePanel')
    hideTab(inputId = 'displayedTab', target = 'historyPanel')
    updateSelectInput(session, 'source', 'Select source of review', choices = c('All', 'Voice Reviews App', 'Google My Business'))
  }
  })
  # Defining reactive element that stores which of the functions we defined in PART 1 to call according 
  # to the dropdown menu selection
  plotCategories <- reactive({
    if(input$aspect_true=='No'){
      selected_category <- as.character(input$category)
      selected_polarity <- as.character(input$polarity)
      if (selected_category=='All'){
        function_to_plot <- plot_categories_all(selected_polarity, input$dateRange[1], input$dateRange[2])
      }
      else{
        function_to_plot <- plot_category_specific(selected_polarity, selected_category, input$dateRange[1], input$dateRange[2])
      }}
    else{
      selected_aspect <- as.character(input$aspect)
      if(interval(ymd(input$dateRange[1]), ymd(input$dateRange[2])) %/% months(1) >0){
        function_to_plot <- plot_aspect_specific_pos_neg_line(selected_aspect, input$dateRange[1], input$dateRange[2])
      }
      else{
        function_to_plot <- plot_aspect_daily_line(selected_aspect, input$dateRange[1], input$dateRange[2])
      }
    }
    return(function_to_plot)
  })
  
  observeEvent(input$category,{
    if(input$aspect_true=='Yes'){
      updateSelectInput(session, 'aspect','Select aspect',
                        unique(retrieve_aspect(input$polarity, input$category, input$dateRange[1], input$dateRange[2])$aspect))}
    updateSelectInput(session, "category2", "Select a category:",
                      # select input from the unique values of sensor names
                      c('All', unique(as.character(reviews_df$category))), input$category)
  })
  
  observeEvent(input$category2,{
    if(input$aspect_true2=='Yes'){
      updateSelectInput(session, 'aspect2','Select aspect',
                        unique(retrieve_aspect(input$polarity2, input$category2, input$dateRange2[1], input$dateRange2[2])$aspect))}
  updateSelectInput(session, "category", "Select a category:",
                              # select input from the unique values of sensor names
                              c('All', unique(as.character(reviews_df$category))), input$category2)
  trend_no(1)
  polarity_reactive(input$polarity)
  if(input$aspect_true=='Yes'){
    aspect_reactive(input$aspect)}
  else{
    aspect_reactive('All')
  }
  datemin_reactive(input$dateRange[1])
  datemax_reactive(input$dateRange[2])
  no_reviews_reactive(5)
  reviews_title_reactive(1)
  })
  
  observeEvent(input$polarity,{
    if(input$aspect_true=='Yes'){
      updateSelectInput(session, 'aspect', 'Select aspect',
                        unique(retrieve_aspect(input$polarity, input$category, input$dateRange[1], input$dateRange[2])$aspect))}
    updateSelectInput(session, 'polarity2', 'Select opinion sentiment', c('All', 'Positive','Negative'), input$polarity)
  })
  
  observeEvent(input$polarity2,{
    if(input$aspect_true2=='Yes'){
      updateSelectInput(session, 'aspect2', 'Select aspect',
                        unique(retrieve_aspect(input$polarity2, input$category2, input$dateRange2[1], input$dateRange2[2])$aspect))}
    updateSelectInput(session, 'polarity', 'Select opinion sentiment', c('All', 'Positive','Negative'), input$polarity2)
    trend_no(1)
    polarity_reactive(input$polarity)
    if(input$aspect_true=='Yes'){
      aspect_reactive(input$aspect)}
    else{
      aspect_reactive('All')
    }
    datemin_reactive(input$dateRange[1])
    datemax_reactive(input$dateRange[2])
    no_reviews_reactive(5)
    reviews_title_reactive(1)
  })
  
  check_aspect_true <- reactiveVal(value = 0)
  
  observeEvent(input$aspect_true,{
    if(check_aspect_true()==0){
      if(input$aspect_true=='Yes'){
        updateSelectInput(session, 'aspect', 'Select aspect',
                          unique(retrieve_aspect(input$polarity, input$category, input$dateRange[1], input$dateRange[2])$aspect))
      }}
    updateSelectInput(session, 'aspect_true2', "Display particular aspect?", c('Yes', 'No'), input$aspect_true)
    check_aspect_true(0)
  })
  
  observeEvent(input$aspect_true2,{
    updateSelectInput(session, 'aspect_true', "Display particular aspect?", c('Yes', 'No'), input$aspect_true2)
    trend_no(1)
    polarity_reactive(input$polarity)
    if(input$aspect_true=='Yes'){
      aspect_reactive(input$aspect)}
    else{
      aspect_reactive('All')
    }
    datemin_reactive(input$dateRange[1])
    datemax_reactive(input$dateRange[2])
    no_reviews_reactive(5)
    reviews_title_reactive(1)
  })
  
  observeEvent(input$aspect,{
    updateActionButton(session, 'button6', paste("View top Positive reviews about", input$aspect))
    updateActionButton(session, 'button7', paste("View top  Negative reviews about", input$aspect))
    updateActionButton(session, 'button8', paste("View top  General reviews about", input$aspect))
    updateSelectInput(session, 'aspect2', 'Select aspect',
                      unique(retrieve_aspect(input$polarity, input$category, input$dateRange[1], input$dateRange[2])$aspect), input$aspect)
  })
  
  observeEvent(input$aspect2,{
    updateActionButton(session, 'button6', paste("View top Positive reviews about", input$aspect))
    updateActionButton(session, 'button7', paste("View top  Negative reviews about", input$aspect))
    updateActionButton(session, 'button8', paste("View top  General reviews about", input$aspect))
    updateSelectInput(session, 'aspect', 'Select aspect',
                      unique(retrieve_aspect(input$polarity, input$category, input$dateRange[1], input$dateRange[2])$aspect), input$aspect2)
    trend_no(1)
    polarity_reactive(input$polarity)
    if(input$aspect_true=='Yes'){
      aspect_reactive(input$aspect)}
    else{
      aspect_reactive('All')
    }
    datemin_reactive(input$dateRange[1])
    datemax_reactive(input$dateRange[2])
    no_reviews_reactive(5)
    reviews_title_reactive(1)
  })
  
  observeEvent(input$dateRange,{
  updateDateRangeInput(session, 'dateRange2',
                 label = 'Choose reviews date range',
                 start = input$dateRange[1], end = input$dateRange[2],
                 min = min(as.POSIXlt(reviews_df$date, "%Y-%m-%d")), max = max(as.POSIXlt(reviews_df$date, "%Y-%m-%d"))
  )})
  
  observeEvent(input$dateRange2,{
    updateDateRangeInput(session, 'dateRange',
                         label = 'Choose reviews date range',
                         start = input$dateRange2[1], end = input$dateRange2[2],
                         min = min(as.POSIXlt(reviews_df$date, "%Y-%m-%d")), max = max(as.POSIXlt(reviews_df$date, "%Y-%m-%d"))
    )
    trend_no(1)
    polarity_reactive(input$polarity)
    if(input$aspect_true=='Yes'){
      aspect_reactive(input$aspect)}
    else{
      aspect_reactive('All')
    }
    datemin_reactive(input$dateRange[1])
    datemax_reactive(input$dateRange[2])
    no_reviews_reactive(5)
    reviews_title_reactive(1)
    })
  
  trend_no = reactiveVal()
  polarity_reactive = reactiveVal()
  aspect_reactive = reactiveVal()
  datemin_reactive = reactiveVal()
  datemax_reactive = reactiveVal()
  no_reviews_reactive = reactiveVal()
  data_frame_reactive = reactiveVal()
  review_response_string_reactive = reactiveVal()
  current_review = reactiveVal()
  reviews_df_reactive = reactiveVal(value = reviews_df)
  review_history_string_reactive = reactiveVal()
  reviews_title_reactive = reactiveVal()
  
  observeEvent(input$buttonFilteredReviews, {
    trend_no(1)
    updateTabsetPanel(session,'displayedTab',selected = 'reviewsPanel')
    polarity_reactive(input$polarity)
    if(input$aspect_true=='Yes'){
    aspect_reactive(input$aspect)}
    else{
      aspect_reactive('All')
    }
    datemin_reactive(input$dateRange[1])
    datemax_reactive(input$dateRange[2])
    no_reviews_reactive(5)
    reviews_title_reactive(1)
    updateSelectInput(session, 'source', 'Select source of review', choices = c('All', 'Voice Reviews App', 'Google My Business'), selected = 'All')
    
    updateSelectInput(session, 'responseFilter', 'Filter by response status', choices = c('All', 'Not answered', 'Answered'), selected = 'All')
    
  })
  
  observeEvent(input$buttonNotAnswered, {
    trend_no(1)
    updateTabsetPanel(session,'displayedTab',selected = 'reviewsPanel')
    polarity_reactive(input$polarity)
    if(input$aspect_true=='Yes'){
      aspect_reactive(input$aspect)}
    else{
      aspect_reactive('All')
    }
    datemin_reactive(input$dateRange[1])
    datemax_reactive(input$dateRange[2])
    no_reviews_reactive(5)
    reviews_title_reactive(1)
    updateSelectInput(session, 'responseFilter', 'Filter by response status', choices = c('All', 'Not answered', 'Answered'), selected = 'Not answered')
    updateSelectInput(session, 'source', 'Select source of review', choices = c('All', 'Voice Reviews App', 'Google My Business'), selected = 'All')
  })
  
  observeEvent(input$showOpinionAnalysis, {
    print(input$showOpinionAnalysis)
  })
  
  observeEvent(input$button1, {
    trend_no(1)
    updateTabsetPanel(session,'displayedTab',selected = 'reviewsPanel')
    polarity_reactive(input$polarity)
    aspect_reactive(aspect<- retrieve_trends_general(as.numeric(trend_no()), input$polarity, input$category, input$dateRange[1], input$dateRange[2])[as.numeric(trend_no()), 'aspect'])
    datemin_reactive(input$dateRange[1])
    datemax_reactive(input$dateRange[2])
    no_reviews_reactive(5)
    updateSelectInput(session, 'responseFilter', 'Filter by response status', choices = c('All', 'Not answered', 'Answered'), selected = 'All')
    reviews_title_reactive(0)
    updateSelectInput(session, 'source', 'Select source of review', choices = c('All', 'Voice Reviews App', 'Google My Business'), selected = 'All')
    
  })
  observeEvent(input$button2, {
    trend_no(2)
    updateTabsetPanel(session,'displayedTab',selected = 'reviewsPanel')
    polarity_reactive(input$polarity)
    datemin_reactive(input$dateRange[1])
    datemax_reactive(input$dateRange[2])
    no_reviews_reactive(5)
    aspect_reactive(aspect<- retrieve_trends_general(as.numeric(trend_no()), input$polarity, input$category, input$dateRange[1], input$dateRange[2])[as.numeric(trend_no()), 'aspect'])
    updateSelectInput(session, 'responseFilter', 'Filter by response status', choices = c('All', 'Not answered', 'Answered'), selected = 'All')
    reviews_title_reactive(0)
    updateSelectInput(session, 'source', 'Select source of review', choices = c('All', 'Voice Reviews App', 'Google My Business'), selected = 'All')
    
  })
  observeEvent(input$button3, {
    trend_no(3)
    updateTabsetPanel(session,'displayedTab',selected = 'reviewsPanel')
    polarity_reactive(input$polarity)
    datemin_reactive(input$dateRange[1])
    datemax_reactive(input$dateRange[2])
    no_reviews_reactive(5)
    updateSelectInput(session, 'responseFilter', 'Filter by response status', choices = c('All', 'Not answered', 'Answered'), selected = 'All')
    aspect_reactive(aspect<- retrieve_trends_general(as.numeric(trend_no()), input$polarity, input$category, input$dateRange[1], input$dateRange[2])[as.numeric(trend_no()), 'aspect'])
    reviews_title_reactive(0)
    updateSelectInput(session, 'source', 'Select source of review', choices = c('All', 'Voice Reviews App', 'Google My Business'), selected = 'All')
    
  })
  observeEvent(input$button4, {
    trend_no(4)
    updateTabsetPanel(session,'displayedTab',selected = 'reviewsPanel')
    polarity_reactive(input$polarity)
    datemin_reactive(input$dateRange[1])
    datemax_reactive(input$dateRange[2])
    no_reviews_reactive(5)
    updateSelectInput(session, 'responseFilter', 'Filter by response status', choices = c('All', 'Not answered', 'Answered'), selected = 'All')
    aspect_reactive(aspect<- retrieve_trends_general(as.numeric(trend_no()), input$polarity, input$category, input$dateRange[1], input$dateRange[2])[as.numeric(trend_no()), 'aspect'])
    reviews_title_reactive(0)
    updateSelectInput(session, 'source', 'Select source of review', choices = c('All', 'Voice Reviews App', 'Google My Business'), selected = 'All')
    
  })
  observeEvent(input$button5, {
    trend_no(5)
    updateTabsetPanel(session,'displayedTab',selected = 'reviewsPanel')
    polarity_reactive(input$polarity)
    datemin_reactive(input$dateRange[1])
    datemax_reactive(input$dateRange[2])
    no_reviews_reactive(5)
    updateSelectInput(session, 'responseFilter', 'Filter by response status', choices = c('All', 'Not answered', 'Answered'), selected = 'All')
    aspect_reactive(aspect<- retrieve_trends_general(as.numeric(trend_no()), input$polarity, input$category, input$dateRange[1], input$dateRange[2])[as.numeric(trend_no()), 'aspect'])
    reviews_title_reactive(0)
    updateSelectInput(session, 'source', 'Select source of review', choices = c('All', 'Voice Reviews App', 'Google My Business'), selected = 'All')
    
  })
  lapply(
    X=1:20,
    FUN = function(i){
      observeEvent(input[[paste('button_', i, 'response', sep ='')]], {
        review_string <- data_frame_reactive()[as.numeric(i), 'review']
        review_date <- data_frame_reactive()[as.numeric(i), 'review_date']
        response_source <- data_frame_reactive()[as.numeric(i), 'source']
        response_user <- data_frame_reactive()[as.numeric(i), 'user']
        if(response_source == 'Google My Business'){
          source_image <- 'gmb_logo.png'
          image_height<- 75
          image_width <- 125
          }
        else{
          image_height<- 62.5
          image_width <- 50
          source_image <- 'voice_logo.png'
        }
        review_response_string_reactive(paste('<div style = "border-radius:8px; background-color: #d7fffe; border-style:solid; border-color:#b1d1fc;"><div style = "margin: 20px;">',  '<img src="', source_image, '" alt="logo" width="', image_width, '" height="',image_height, '"/></br></br>', '<b>Date: ', format(as.Date(review_date, format = "%Y-%m-%d"), "%d-%m-%Y"), '</b><br/><b>Source: ', response_source, '</b></br><b>User: ', response_user,  '</b></br></br>', '<b>Message: </b>', review_string, '</div></div><br/>', sep=''))
        current_review(review_string)
        updateTabsetPanel(session,'displayedTab',selected = 'responsePanel')
        showTab(inputId = 'displayedTab', target = 'responsePanel')
      })
    }
  )
  
  lapply(
    X=1:20,
    FUN = function(i){
      observeEvent(input[[paste('button_', i, 'history', sep ='')]], {
        review_string <- data_frame_reactive()[as.numeric(i), 'review']
        review_date <- data_frame_reactive()[as.numeric(i), 'review_date']
        response_string <- data_frame_reactive()[as.numeric(i), 'response']
        response_time <- data_frame_reactive()[as.numeric(i), 'time_response']
        response_source <- data_frame_reactive()[as.numeric(i), 'source']
        response_user <- data_frame_reactive()[as.numeric(i), 'user']
        print(response_source)
        if(response_source == 'Google My Business'){
          source_image <- 'gmb_logo.png'
          image_height<- 75
          image_width <- 125
        }
        else{
          image_height<- 62.5
          image_width <- 50
          source_image <- 'voice_logo.png'
        }
        review_history_string_reactive(paste('<div style = "border-radius:8px; background-color: #d7fffe; border-style:solid; border-color:#b1d1fc;"><div style = "margin: 20px;">',  '<img src="', source_image, '" alt="logo" width="', image_width, '" height="',image_height, '"/></br></br>', '<b>Date: ', format(as.Date(review_date, format = "%Y-%m-%d"), "%d-%m-%Y"), '</b><br/><b>Source: ', response_source, '</b></br><b>User: ', response_user,  '</b></br></br>', '<b>Message: </b>', review_string, '</div></div><br/><div style = "border-radius:8px; background-color: #d7fffe; border-style:solid; border-color:#b1d1fc;"><div style = "margin: 20px;"><b><span style="color:red"> Business response</span></b><br/><b>Date: ',response_time, '</b><br/><br/><b>Message: </b>', response_string, '</div></div>', sep=''))
        updateTabsetPanel(session,'displayedTab',selected = 'historyPanel')
        showTab(inputId = 'displayedTab', target = 'historyPanel')
      })
    }
  )
  
  output$reviewsPlot <- renderPlotly({plotCategories()})
  
  
  plotTitle <- reactive({
    
    if(input$aspect_true == 'Yes'){
      text_to_render <- paste('<h4>Distribution of aspect', input$aspect, format(as.Date(input$dateRange[1], format = "%Y-%m-%d"), "%d-%m-%Y"), 'to', format(as.Date(input$dateRange[2], format = "%Y-%m-%d"), "%d-%m-%Y"), '</h4>')
    }
    else{
      if(input$category=='All'){
        text_to_render <- paste('<h4>', input$polarity, 'reviews by category from', format(as.Date(input$dateRange[1], format = "%Y-%m-%d"), "%d-%m-%Y"), 'to', format(as.Date(input$dateRange[2], format = "%Y-%m-%d"), "%d-%m-%Y"), '</h4>')
      }
      else{
        text_to_render <- paste('<h4>', input$polarity, 'top aspects', input$category, format(as.Date(input$dateRange[1], format = "%Y-%m-%d"), "%d-%m-%Y"), 'to', format(as.Date(input$dateRange[2], format = "%Y-%m-%d"), "%d-%m-%Y"), '</h4>')
      }
    }
    return(text_to_render)
  })
  
  reviewsTitle <- reactive({
    if (reviews_title_reactive() == 0){
    if(polarity_reactive() != 'All'){
      text_to_render <- paste('<h4 align="center">', polarity_reactive(), 'reviews about', aspect_reactive(), 'from', format(as.Date(datemin_reactive(), format = "%Y-%m-%d"), "%d-%m-%Y"), 'to', format(as.Date(datemax_reactive(), format = "%Y-%m-%d"), "%d-%m-%Y"), '</h4>')
    }
    else{
      text_to_render <- paste('<h4 align="center">', 'Reviews about', aspect_reactive(), 'from', format(as.Date(datemin_reactive(), format = "%Y-%m-%d"), "%d-%m-%Y"), 'to', format(as.Date(datemax_reactive(), format = "%Y-%m-%d"), "%d-%m-%Y"), '</h4>')
    }}
    else{
      if(input$polarity!='All' | input$category!='All'  ){
      text_to_render <- paste('<h4 align="center">', input$polarity, 'reviews about', input$category, 'from', format(as.Date(datemin_reactive(), format = "%Y-%m-%d"), "%d-%m-%Y"), 'to', format(as.Date(datemax_reactive(), format = "%Y-%m-%d"), "%d-%m-%Y"), '</h4>')
      }
      else{
        text_to_render <- paste('<h4 align="center">', 'All reviews from', format(as.Date(datemin_reactive(), format = "%Y-%m-%d"), "%d-%m-%Y"), 'to', format(as.Date(datemax_reactive(), format = "%Y-%m-%d"), "%d-%m-%Y"), '</h4>')
      }
      }
    return(text_to_render)
  })
  
  printReviews <- reactive({
    if(input$aspect_true=='No'){
      if(aspect_reactive()!='All'){
      aspect<- retrieve_trends_general(as.numeric(trend_no()), input$polarity, input$category, input$dateRange[1], input$dateRange[2])[as.numeric(trend_no()), 'aspect']
      polarity <- retrieve_trends_general(as.numeric(trend_no()), input$polarity, input$category, input$dateRange[1], input$dateRange[2])[as.numeric(trend_no()), 'aspect_polarity']
      category <- 'N/A'
      }
      else{
        aspect <- 'All'
        polarity <- input$polarity
        category <- input$category
      }
      function_to_return <- retrieve_reviews_aspect(reviews_df_reactive(), polarity, aspect, input$dateRange[1], input$dateRange[2], no_reviews_reactive(), input$responseFilter, category, input$showOpinionAnalysis, input$colour, input$source)
      data_frame_reactive(function_to_return[[2]])
      return(function_to_return[[1]])}
    else{
      function_to_return <- retrieve_reviews_aspect(reviews_df_reactive(), polarity_reactive(), input$aspect, datemin_reactive(), datemax_reactive(), no_reviews_reactive(), input$responseFilter, 'N/A', input$showOpinionAnalysis, input$colour, input$source)
      data_frame_reactive(function_to_return[[2]])
      return(function_to_return[[1]])
    }
  })
  
  counterReviews <- reactive({
    if(input$aspect_true == 'Yes'){
      aspect <- input$aspect
    }
    else{
      aspect <- 'All'
    }
    string_to_return <- count_reviews(reviews_df, input$polarity, aspect, input$dateRange[1], input$dateRange[2], 1, 'All', input$category, input$showOpinionAnalysis, input$colour, input$source)
    return(string_to_return)
  })
  
  observeEvent(input$button6, {
    polarity_reactive('Positive')
    updateTabsetPanel(session,'displayedTab',selected = 'reviewsPanel')
    aspect_reactive(input$aspect)
    datemin_reactive(input$dateRange[1])
    datemax_reactive(input$dateRange[2])
    no_reviews_reactive(5)
    updateSelectInput(session, 'responseFilter', 'Filter by response status', choices = c('All', 'Not answered', 'Answered'), selected = 'All')
    reviews_title_reactive(0)
    updateSelectInput(session, 'source', 'Select source of review', choices = c('All', 'Voice Reviews App', 'Google My Business'), selected = 'All')
    
  })
  
  observeEvent(input$button7, {
    polarity_reactive('Negative')
    updateTabsetPanel(session,'displayedTab',selected = 'reviewsPanel')
    aspect_reactive(input$aspect)
    datemin_reactive(input$dateRange[1])
    datemax_reactive(input$dateRange[2])
    no_reviews_reactive(5)
    updateSelectInput(session, 'responseFilter', 'Filter by response status', choices = c('All', 'Not answered', 'Answered'), selected = 'All')
    reviews_title_reactive(0)
    updateSelectInput(session, 'source', 'Select source of review', choices = c('All', 'Voice Reviews App', 'Google My Business'), selected = 'All')
  })
  
  observeEvent(input$button8, {
    polarity_reactive('All')
    updateTabsetPanel(session,'displayedTab',selected = 'reviewsPanel')
    aspect_reactive(input$aspect)
    datemin_reactive(input$dateRange[1])
    datemax_reactive(input$dateRange[2])
    no_reviews_reactive(5)
    updateSelectInput(session, 'responseFilter', 'Filter by response status', choices = c('All', 'Not answered', 'Answered'), selected = 'All')
    reviews_title_reactive(0)
    updateSelectInput(session, 'source', 'Select source of review', choices = c('All', 'Voice Reviews App', 'Google My Business'), selected = 'All')
    
  })
  
  observeEvent(input$responseButton, {
    # python_path = '/Users/jgordyn/opt/anaconda3/envs/nlp_new/bin/python3.7'
    # reticulate::use_virtualenv('/Users/jgordyn/opt/anaconda3/envs/nlp_new', required = T)
    review_response_text <- input$reviewResponse
    if(review_response_text!=''){
      # source_python("response_reviews_2.py")
      # response_review(review_id, review_response_text)
      updateTextAreaInput(session, 'reviewResponse', 'Response', value = '')
      reviews_df[reviews_df$review==current_review(), 'time_response'] <- as.character(format(now(), '%d-%m-%Y %H:%M:%S'))
      reviews_df[reviews_df$review==current_review(), 'response'] <- review_response_text
      write.csv(reviews_df, file = 'aspects_reviews_final.csv', row.names = FALSE)
      # gcs_global_bucket("tu-vieja-en-recontra-tanga")
      #gcs_upload(reviews_df, name = 'aspects_reviews_final.csv')
      shinyalert(title = "Response submitted", type = "success")
      reviews_df_reactive(reviews_df)
      updateTabsetPanel(session,'displayedTab',selected = 'reviewsPanel')
      
      }
    else{
      shinyalert(title = "Response is empty", type = "error")
    }
    
  })
  
  observe({
    event_plot_category_specific <- event_data("plotly_click", source = 'A')
    event_plot_category_all <- event_data("plotly_click", source = 'B')
    event_aspect_specific <- event_data("plotly_click", source = 'C')
    if(input$aspect_true == 'No' && !is.null(event_plot_category_specific)){
      if(input$category!='All'){
        clicked_data_df <- retrieve_data_plot_category_specific(event_plot_category_specific$key[[1]][2], event_plot_category_specific$key[[1]][3], input$dateRange[1], input$dateRange[2])
        clicked_data <- clicked_data_df[rownames(clicked_data_df) == event_plot_category_specific$key[[1]][1], ]
        aspect_aspect <- clicked_data$aspect
        new_value <- aspect_aspect
        if(new_value != data_click_global){
          updateSelectInput(session, 'aspect_true', "Select aspect:",
                            c('Yes', 'No'), 'Yes')
          updateSelectInput(session, 'aspect', "Select aspect:",
                            unique(reviews_df[reviews_df$category == event_plot_category_specific$key[[1]][3], ]$aspect), aspect_aspect)
          check_aspect_true(1)
          data_click_global <<- new_value
        }
      }
    }
    if(input$aspect_true == 'No' && !is.null(event_plot_category_all)){
      if(input$category =='All'){
        clicked_data_df_all <- retrieve_data_plot_categories_all(event_plot_category_all$key[[1]][2], input$dateRange[1], input$dateRange[2])
        clicked_data_all <- clicked_data_df_all[rownames(clicked_data_df_all) == event_plot_category_all$key[[1]][1], ]
        clicked_category_all <- clicked_data_all$category
        clicked_polarity_all <- clicked_data_all$aspect_polarity
        new_value_all <- paste(clicked_category_all, clicked_polarity_all)
        if(new_value_all != data_click_global_all){
          updateSelectInput(session, "category", "Select a category:",
                            # select input from the unique values of sensor names
                            c('All', unique(as.character(reviews_df$category))), clicked_category_all)
          updateSelectInput(session, 'polarity', 'Select opinion sentiment', c('All', 'Positive','Negative'), clicked_polarity_all)
          data_click_global_all <<- new_value_all
        }
      }
    }
    
    if(input$aspect_true == 'Yes' && !is.null(event_aspect_specific)){
      if(interval(ymd(input$dateRange[1]), ymd(input$dateRange[2])) %/% months(1) >0){
        clicked_data_df_aspect <- retrieve_aspect_df_month(event_aspect_specific$key[[1]][2], input$dateRange[1], input$dateRange[2])
        clicked_data_aspect <- clicked_data_df_aspect[rownames(clicked_data_df_aspect) == event_aspect_specific$key[[1]][1], ]
        clicked_date <- clicked_data_aspect$month
      }
      else{
        clicked_data_df_aspect <- retrieve_aspect_df_day(event_aspect_specific$key[[1]][2], input$dateRange[1], input$dateRange[2])
        clicked_data_aspect <- clicked_data_df_aspect[rownames(clicked_data_df_aspect) == event_aspect_specific$key[[1]][1], ]
        clicked_date <- clicked_data_aspect$day
      }
      clicked_polarity_aspect <- clicked_data_aspect$aspect_polarity
      clicked_count <- clicked_data_aspect$count_monthly_aspect
      new_value_aspect <- paste(clicked_date, clicked_polarity_aspect)
      if(new_value_aspect != data_click_global_aspect){
        polarity_reactive(clicked_polarity_aspect)
        aspect_reactive(input$aspect)
        datemin_reactive(as.Date(clicked_date, format = "%d-%m-%Y"))
        if(interval(ymd(input$dateRange[1]), ymd(input$dateRange[2])) %/% months(1) >0){
          datemax_reactive(ceiling_date(clicked_date, "month") - days(1))}
        else{
          datemax_reactive(datemin_reactive())
        }
        no_reviews_reactive(as.numeric(clicked_count))
        updateTabsetPanel(session,'displayedTab',selected = 'reviewsPanel')
        updateSelectInput(session, 'responseFilter', 'Filter by response status', choices = c('All', 'Not answered', 'Answered'), selected = 'All')
        data_click_global_aspect <<- new_value_aspect
        reviews_title_reactive(0)
      }
    }
    
  })
  
  output$reviewsText<-renderText(printReviews())
  output$trendsText <- renderText(paste('<h4>', input$polarity, 'trends,', 'category:', input$category, '</h4>'))
  output$trendsAspectText <- renderText(paste('<h4>', 'Opinions about', input$aspect, '</h4>'))
  output$titleTextPlot <- renderText(plotTitle())
  output$reviewsAspectTitle <- renderText(reviewsTitle())
  output$value <- renderText({input$responsePanel})
  output$reviewToRespond <- renderText({review_response_string_reactive()})
  output$reviewHistory <- renderText({review_history_string_reactive()})
  output$reviewsCount <-renderText({counterReviews()})
}
