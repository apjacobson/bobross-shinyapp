#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(png)
library(shinyWidgets)
library(shinyjs)
library(tm)
dat <- read.csv("./bob-ross.csv")
source("funcs.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
  headerPanel("Bob Ross - painting by the numbers"),
  tabsetPanel(
    tabPanel("Painting Creator",
  titlePanel("Create your own Bob Ross painting"),
  setBackgroundImage(
    src = "www/bob.png"
  ),
  
  fluidRow(
    column(4, wellPanel(
      selectInput("sky", "Sky:",
                   c("clouds", "sunset", "mountains")),
      selectInput("ground", "Ground:",
                   c("grass", "ocean", "river", "lake")),

      checkboxInput("tree", "Tree", value = FALSE),
      actionButton("button", "Generate title!")
    )),
    
           imageOutput("image1", height = "3px", width = "3px")
    ,
    
           imageOutput("image2", height = "3px", width = "3px")
    ,
          imageOutput("image3", height = "3px", width = "3px"),
    
    column(4, hidden(
      div(id='text_div',
          verbatimTextOutput("text")
      )
    ))
  )

), tabPanel("some other stuff i'm gonna do",
  titlePanel("Oh baby")
)))

# Define server logic required to draw a histogram
server <- function(input, output) {
  observeEvent(input$button, {
    toggle('text_div')
    output$text <- renderText({paste(title_word(input$sky), title_word(input$ground))})
  })
   
  output$image1 <- renderImage({
    if (is.null(input$ground))
      return(NULL)
    
    if (input$ground == "ocean") {
      return(list(
        src = "www/ocean.png",
        contentType = "image/png",
        alt = "ocean"
      ))
    } else if (input$ground == "grass") {
      return(list(
        src = "www/grass.png",
        filetype = "image/png",
        alt = "grass"
      ))
    } else if (input$ground == "river") {
      return(list(
        src = "www/river.png",
        filetype = "image/png",
        alt = "river"
      ))
    }else if (input$ground == "lake") {
      return(list(
        src = "www/lake.png",
        filetype = "image/png",
        alt = "lake"
      ))
    }
    
  }, deleteFile = FALSE)
  
  output$image2 <- renderImage({
    if (is.null(input$sky))
      return(NULL)
    
    if (input$sky == "clouds") {
      return(list(
        src = "www/clouds.png",
        contentType = "image/png",
        alt = "clouds"
      ))
    } else if (input$sky == "sunset") {
      return(list(
        src = "www/sunset.png",
        filetype = "image/png",
        alt = "sunset"
      ))
    } else if (input$sky == "mountains") {
      return(list(
        src = "www/mountains.png",
        filetype = "image/png",
        alt = "mountains"
      ))
    }
    
  }, deleteFile = FALSE)
  
  
  output$image3 <- renderImage({
    if (is.null(input$tree))
      return(NULL)
    
    if (input$tree == TRUE) {
      return(list(
        src = "www/tree.png",
        contentType = "image/png",
        alt = "tree"
      ))
    } else {
      return(list(
        src = "www/none.png",
        contentType = "image/png",
        alt = "none"
      ))
    }  }, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)

