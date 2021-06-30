library(shiny)
library(shinythemes)
library(bsplus)

options(shiny.sanitize.errors = FALSE)

ui<- fluidPage(fluidRow(
  column(3,
         style = "border-radius:8px; background-color: #ededed; border-style:solid; border-color:#b1d1fc; margin: 5px; padding: 10px",
         br(),
         selectInput("review_example", "Select Review Example:",
                     # select input from the unique values of sensor names
                     c('Hotel Review', 'Restaurant Review', 'Uni Review', 'App Review', 'Write your own'))%>% shinyInput_label_embed(
                       icon("info") %>%
                         bs_embed_tooltip(title = 'Select among pre-defined examples to analyse reviews or "Write your own" to create and analyse your own review. Once the results are displayed, hover over the highlighted text.'))),
  column(8, align = 'center',
  style = "border-radius:8px; background-color: #d7fffe; border-style:solid; border-color:#b1d1fc; margin: 5px; padding: 10px",
  textAreaInput('review', 'Review', value = 'The hotel was outstanding! The decorations were amazing and the staff was extremely helpful. The prices were a little too much though but totally worth it! Completely recommendable experience!' ,
                width = '400px', height = '200px'),
  actionButton("button", "Analyse Review", style = " border-radius: 8px; color: black; background-color: #e7e7e7; border: 2px solid #555555 ; padding: 10px"),
  br(),
  br())),
  br(),
  br(),
  fluidRow(
  column(3, ''),
  column(8, align = 'center', htmlOutput("value"))
  ))
