# importing the required libraries
library('tidyverse')
library('shiny')
library('ggplot2')
library('gridExtra')
library('shinythemes')
library('bsplus')
library('lubridate')
library('plotly')
library('shinyWidgets')
library('shinyalert')

# gcs_auth(json_file = "/Users/jgordyn/Google Drive/DATA SCIENCE MASTER/NLP/API01-e2eb0b73f5c2.json")
# reviews_df <- gcs_get_object('gs://tu-vieja-en-recontra-tanga/aspects_reviews_final.csv')
reviews_df <- read_csv('aspects_reviews_final_trial.csv')
reviews_df$date<- as.character(reviews_df$date)

# defining the user interface
ui<- fluidPage(
      navbarPage("WhatUThinkin'",id ='displayedTab',
                  tabPanel("Introduction", value = 'introPanel',
                           titlePanel("WhatUThinkin' Demo App"),
                           # sidebarLayout will contain student's data
                           sidebarLayout(
                              sidebarPanel(width = 3, img(src = 'voice_logo.png', width = "50", height="62.5")),
                              
                              # mainPanel will contain the actual Introduction
                              mainPanel(width = 7,
                                        br(),
                                        p("The following is a Demo interactive App to get a sense of how the product will work and understand the potential applications it has. You will find the following tabs:"),
                                        br(),
                                        p(strong("Examples Tab:"), "Here you will get to test the review analysis algorithm in different pre-defined examples, having also the possibility of writing and analysing live your own written review.
The Demo only supports written reviews as input. The final product will also support voice reviews which will be transcribed to text and then analysed as shown in this Demo App."),
                                        br(),
                                       p(strong("Dashboard Tab:"), "interactive basic dashboard to understand how the information will be presented to the client once analysed. The dashboard is constructed using 2 different sources of reviews:"),
                                        p("Synthetic voice reviews transcribed with Google Speech API and then analysed using the same program that returns the results in the Examples Tab, mainly to show that the App is able to capture voice reviews and analyse them as easily as text."),
                                        p("Real Google Reviews corresponding to the retail Company GNC AR in Argentina", tags$a(href="https://www.gnc.com.ar", "www.gnc.com.ar"), "to demonstrate that it can also be simply integrated with other reviews services, such as Google My Business in this case."),
                                        p("You will be able to identify the reviews main trends and filter them according to different criteria. All the plots generated in this Tab are interactive and clickable."),
                                        br(),
                                        p(strong("Reviews Tab:"), "to access the original reviews (written or voice) according to the applied filters. You will also be able to show the explicit analysis of each review, subject to pre-defined filtering, by using the “Show Analysis” button."),
                                        br(),
                                        p(strong("Response Tab:"), "allowing to answer each individual review (or a set of reviews that meet the filtering criteria). This tab will be only accessible via the “Respond Review” button under each review in the Reviews Tab."),
                                        br(),
                                        p(strong("History Tab:"), "to see the response history of each review. Only accessible by clicking on the View History button under each already answered review in the Reviews Tab."),
                                        br(),
                                        br(),
                                        actionButton("introButton", "Go to Examples", style = "border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #b1d1fc"),
                                       br(),
                                       br(),
                                       br()
                              ))),
                  tabPanel(value = 'examplesPanel', 'Examples',
                           fluidRow(
                              column(3,
                                     fluidRow(
                                     img(src = 'voice_logo.png', width = "50", height="62.5"),
                                     style = "border-radius:8px; background-color: #ededed; border-style:solid; border-color:#b1d1fc; margin: 5px; padding: 10px",
                                     br(),
                                     selectInput("review_example", "Select Review Example:",
                                                 # select input from the unique values of sensor names
                                                 c('Hotel Review', 'Restaurant Review', 'Uni Review', 'App Review', 'Write your own'))%>% shinyInput_label_embed(
                                                    icon("info") %>%
                                                       bs_embed_tooltip(title = 'Select among pre-defined examples to analyse reviews or "Write your own" to create and analyse your own review. Once the results are displayed, hover over the highlighted text.')))),
                              column(8, align = 'center',
                                     style = "border-radius:8px; background-color: #d7fffe; border-style:solid; border-color:#b1d1fc; margin: 5px; padding: 10px",
                                     textAreaInput('review_2', 'Review', value = 'The hotel was outstanding! The decorations were amazing and the staff was extremely helpful. The prices were a little too much though but totally worth it! Completely recommendable experience!' ,
                                                   width = '400px', height = '200px'),
                                     actionButton("button_rev_ex", "Analyse Review", style = "border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #b1d1fc"),
                                     br(),
                                     br())),
                           br(),
                           fluidRow(
                              column(3, ''),
                              column(8, align = 'center', htmlOutput("example_analysis")),
                              br(),
                              br()
                           )
                  ),
                  tabPanel(value = 'mainPanel', 'Dashboard',
      column(5, style= "padding-right:0px",
             fluidRow(
                style = "border-radius:8px; background-color: #d7fffe; border-style:solid; border-color:#b1d1fc; margin: 5px; padding: 10px",
                      # title
               titlePanel("Reviews Dashboard"),
               br(),
                selectInput("category", "Select a category:",
                            # select input from the unique values of sensor names
                            c('All', unique(as.character(reviews_df$category))))%>% shinyInput_label_embed(
                              icon("info") %>%
                                bs_embed_tooltip(title = "Filter the reviews, taking into account if the client has expressed an opinion about any aspect belonging to any of the following pre-defined categories.")),
                # list with the different options
                # adding tool tips with bs_embed_tooltip when relevant
                             selectInput('polarity', 'Select opinion sentiment', c('All', 'Positive','Negative'))%>% shinyInput_label_embed(
                               icon("info") %>%
                                 bs_embed_tooltip(title = "Filter the Reviews according to the sentiment expressed by the client about each particular aspect."))
                             ,
                             selectInput("aspect_true","Display particular aspect?", c('No', 'Yes'), ) %>% shinyInput_label_embed(
                               icon("info") %>%
                                 bs_embed_tooltip(title = "If yes, you will be able to choose among the top aspects mentioned in the Reviews that match the pre-selected filtering criteria, in order to display information about that particular aspect (the aspects will be ordered according to popularity among the customers reviews, from more to less popular) If no, general information about all/top aspects will be displayed.")),
                             conditionalPanel(condition = "input.aspect_true == 'Yes'",selectInput("aspect", "Select aspect:",
                                                  unique(reviews_df$aspect))),
                dateRangeInput('dateRange',
                               label = 'Choose reviews date range',
                               start = min(as.POSIXlt(reviews_df$date, "%Y-%m-%d")), end = max(as.POSIXlt(reviews_df$date, "%Y-%m-%d")),
                               min = min(as.POSIXlt(reviews_df$date, "%Y-%m-%d")), max = max(as.POSIXlt(reviews_df$date, "%Y-%m-%d")),
                               format = "dd/mm/yyyy"
                ),
       
                             ),
             fluidRow(
                style = "border-radius:8px; background-color: #d7fffe; border-style:solid; border-color:#b1d1fc; margin: 5px; padding: 10px",
                htmlOutput('reviewsCount')
             )
             ),
                     
                              column(7,style= "padding-left:0px; padding-bottom:10px",
                                     fluidRow( style = "border-radius:8px; background-color: #d7fffe; border-style:solid; border-color:#b1d1fc; margin: 5px; padding: 10px",
                                     conditionalPanel(condition = "input.aspect_true=='No'", # defining the outputs: map title, map and graph
                                      htmlOutput('trendsText'),
                                      br(),
                                     strong(htmlOutput('text1')),
                                     actionButton("button1", "View reviews", style = " border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #555555"),
                                     strong(htmlOutput('text2')),
                                     actionButton("button2", "View reviews", style = " border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #555555"),
                                     strong(htmlOutput('text3')),
                                     actionButton("button3", "View reviews", style = " border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #555555"),
                                     strong(htmlOutput('text4')),
                                     actionButton("button4", "View reviews", style = " border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #555555"),
                                     strong(htmlOutput('text5')),
                                     actionButton("button5", "View reviews", style = " border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #555555"),
                                     br(),
                                     br()),
                                     
                                     
                                     conditionalPanel(
                                                      condition = "input.aspect_true=='Yes'", # defining the outputs: map title, map and graph
                                                      htmlOutput('trendsAspectText'),
                                                      br(),
                                                      actionButton("button6", "View top Positive reviews about", style = " border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #555555"),
                                                      br(),
                                                      br(),
                                                      actionButton("button7", "View top Negative reviews about", style = " border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #555555"),
                                                      br(),
                                                      br(),
                                                      actionButton("button8", "View top General reviews about", style = " border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #555555"),
                                                      br(),
                                                      br(),
                                                      br())),
                                     fluidRow(style = "border-radius:8px; border-style:solid; border-color:#b1d1fc; margin: 5px; padding: 10px",
                                              htmlOutput('titleTextPlot'),
                                              plotlyOutput("reviewsPlot")
                                              )
                                     
                                     
                              ),
                            ),
      tabPanel(value = 'reviewsPanel', 'Reviews',
               fluidRow(style = "background-color: #ededed; border-radius:8px; border-style:solid; border-color:#b1d1fc; margin: 5px; padding: 10px",
               htmlOutput('reviewsAspectTitle'),
               br(),
                  column(6, 
               selectInput("category2", "Select a category:",
                           # select input from the unique values of sensor names
                           c('All', unique(as.character(reviews_df$category))))%>% shinyInput_label_embed(
                              icon("info") %>%
                                 bs_embed_tooltip(title = "Filter the reviews, taking into account if the client has expressed an opinion about any aspect belonging to any of the following pre-defined categories.")),
               # list with the different options
               # adding tool tips with bs_embed_tooltip when relevant
               selectInput('polarity2', 'Select opinion sentiment', c('All', 'Positive','Negative'))%>% shinyInput_label_embed(
                  icon("info") %>%
                     bs_embed_tooltip(title = "Filter the Reviews according to the sentiment expressed by the client about each particular aspect."))
               ,
               selectInput("aspect_true2","Display particular aspect?", c('No', 'Yes'), ) %>% shinyInput_label_embed(
                  icon("info") %>%
                     bs_embed_tooltip(title = "If yes, you will be able to choose among the top aspects mentioned in the Reviews that match the pre-selected filtering criteria, in order to display information about that particular aspect (the aspects will be ordered according to popularity among the customers reviews, from more to less popular) If no, general information about all/top aspects will be displayed."))),
               column(6,
               dateRangeInput('dateRange2',
                              label = 'Choose reviews date range',
                              start = min(as.POSIXlt(reviews_df$date, "%Y-%m-%d")), end = max(as.POSIXlt(reviews_df$date, "%Y-%m-%d")),
                              min = min(as.POSIXlt(reviews_df$date, "%Y-%m-%d")), max = max(as.POSIXlt(reviews_df$date, "%Y-%m-%d")),
                              format = "dd/mm/yyyy"
               ),
               selectInput('source', 'Select source of review', choices = c('All', 'Voice Reviews App', 'Google My Business')),
               selectInput('responseFilter', 'Filter by response status', choices = c('All', 'Not answered', 'Answered')),
               conditionalPanel(condition = "input.aspect_true2 == 'Yes'",selectInput("aspect2", "Select aspect:",
                                                                                     unique(reviews_df$aspect)))),
               prettyCheckbox('showOpinionAnalysis', label = strong('Show opinion analysis'), fill = TRUE, thick = TRUE, bigger = TRUE, animation = 'smooth', value = FALSE),
               conditionalPanel(condition = "input.showOpinionAnalysis == 1",selectInput("colour", "What would you like the color to represent?",
                                                                                      c('Category', 'Opinion Polarity')))),
               br(),
               br(),
               htmlOutput('reviewsText')
               ),
      tabPanel(value = 'responsePanel', 'Response',
               titlePanel("Respond to the Review"),
               br(),
               htmlOutput('reviewToRespond'),
               br(),
               fluidRow(style = "border-radius:8px; background-color: #d7fffe; border-style:solid; border-color:#b1d1fc; margin: 5px; padding: 10px",
               textAreaInput('reviewResponse', 'Write your response:', width = '100%', height = '100%') %>%
                  shiny::tagAppendAttributes(style = 'width: 100%;'),
               verbatimTextOutput("value"),
               actionButton("responseButton", "Submit response", style = " border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #555555")),
               useShinyalert()
               ),
      tabPanel(value = 'historyPanel', 'History',
               titlePanel("Review's History"),
               br(),
               htmlOutput('reviewHistory')
                            )
      
)
)
