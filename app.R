library(shiny)
library(png)
library(shinyWidgets)
library(shinyjs)
library(tm)
library(arules)
library(arulesViz)
library(stringr)

dat <- read.csv("./bob-ross.csv")
source("funcs.R")

# Define UI for application
ui <- fluidPage(
  headerPanel("Bob Ross - painting by the numbers"),
  tabsetPanel(
    tabPanel("Painting Creator",
  titlePanel("Create your own Bob Ross painting"),
  setBackgroundImage(
    src = "www/bob.png"
  ),
  
  fluidRow(
    column(2, wellPanel(
      selectInput("sky", "Sky:",
                   c("clouds", "sunset", "mountains")),
      selectInput("ground", "Ground:",
                   c("grass", "ocean", "river", "lake")),
      selectInput("frame", "Frame:",
                   c("rectangular", "circle", "oval", "seashell", "window", "wood")),

      checkboxInput("tree", "Tree", value = FALSE),
      checkboxInput("flowers", "Flowers", value = FALSE),
      checkboxInput("moon", "Moon", value = FALSE),
      checkboxInput("bob", "Bob Ross", value = FALSE),
      actionButton("button", "Generate title!")
    )),
    column(3,
    
           imageOutput("image1", height = "3px", width = "3px")
    ,
    
           imageOutput("image2", height = "3px", width = "3px")
    ,
          imageOutput("image3", height = "3px", width = "3px"),
          imageOutput("image4", height = "3px", width = "3px"),
          imageOutput("image5", height = "3px", width = "3px"),
          imageOutput("image7", height = "3px", width = "3px"),
          imageOutput("image6", height = "3px", width = "3px")
    ),
    column(2, offset=4,
      h1("WWBRD"),
      h6("(What Would Bob Ross Do?)"),
      h4("Bob Ross would call this..."),
      p(id='text_div',
          verbatimTextOutput("text")
      ),
      h4("Bob Ross would add..."),
      p(id='text_div',
        verbatimTextOutput("text2")
      ),
    )
  )

), tabPanel("some other stuff i'm gonna do",
  titlePanel("Oh baby")
)))

# Define server logic required to draw the image
server <- function(input, output) {
  output$text <- renderText({paste(title_word(input$sky), title_word(input$ground))})
  observeEvent(input$button, {
    output$text <- renderText({paste(title_word(input$sky), title_word(input$ground))})
  })
  output$text2 <- renderText({
    sky <- input$sky
    if (input$sky == "sunset") {
      sky <- "SUN"
    }
    inputs <- c(str_to_upper(sky),str_to_upper(input$ground))
    if (input$sky == "mountains") {
      inputs <- c(inputs,"TREES","SNOWY_MOUNTAIN","MOUNTAIN","CLOUDS","CONIFER")
    }
    if (input$tree == TRUE) {
      inputs <- c(inputs,"TREE")
    }
    if (input$flowers == TRUE) {
      inputs <- c(inputs,"FLOWERS")
    }
    if (input$moon == TRUE) {
      inputs <- c(inputs, "MOON")
    }
    if (!is.null(input$frame)) {
      if (input$frame == "wood") {
        inputs <- c(inputs, "WOOD_FRAMED")
      } else {
        new <- paste(str_to_upper(input$frame), "_FRAME",sep="")
        inputs <- c(inputs, new)
      }
    }
    return(paste(run_apriori(inputs),collapse="\n"))
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
  
  output$image4 <- renderImage({
    if (is.null(input$flowers))
      return(NULL)
    
    if (input$flowers == TRUE) {
      return(list(
        src = "www/flowers.png",
        contentType = "image/png",
        alt = "flowers"
      ))
    } else {
      return(list(
        src = "www/none.png",
        contentType = "image/png",
        alt = "none"
      ))
    }  }, deleteFile = FALSE)
  
  
  output$image5 <- renderImage({
    if (is.null(input$moon))
      return(NULL)
    
    if (input$moon == TRUE) {
      return(list(
        src = "www/moon.png",
        contentType = "image/png",
        alt = "moon"
      ))
    } else {
      return(list(
        src = "www/none.png",
        contentType = "image/png",
        alt = "none"
      ))
    }  }, deleteFile = FALSE)
  
  
  output$image6 <- renderImage({
    if (is.null(input$bob))
      return(NULL)
    
    if (input$bob == TRUE) {
      return(list(
        src = "www/bob.png",
        contentType = "image/png",
        alt = "bob"
      ))
    } else {
      return(list(
        src = "www/none.png",
        contentType = "image/png",
        alt = "none"
      ))
    }  }, deleteFile = FALSE)
  
  
  
  output$image7 <- renderImage({
    if (is.null(input$frame))
      return(NULL)
    
    if (input$frame == "rectangular") {
      return(list(
        src = "www/none.png",
        contentType = "image/png",
        alt = "none"
      ))
    } else if (input$frame == "circle") {
      return(list(
        src = "www/circle_frame.png",
        filetype = "image/png",
        alt = "circle frame"
      ))
    } else if (input$frame == "oval") {
      return(list(
        src = "www/oval_frame.png",
        filetype = "image/png",
        alt = "oval frame"
      ))
    } else if (input$frame == "seashell") {
      return(list(
        src = "www/seashell_frame.png",
        filetype = "image/png",
        alt = "seashell frame"
      ))
    } else if (input$frame == "window") {
      return(list(
        src = "www/window_frame.png",
        filetype = "image/png",
        alt = "window frame"
      ))
    } else if (input$frame == "wood") {
      return(list(
        src = "www/wood_frame.png",
        filetype = "image/png",
        alt = "wood frame"
      ))
    }
    
  }, deleteFile = FALSE)
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

