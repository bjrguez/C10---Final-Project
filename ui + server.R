library(shiny)

####################################################
# Define UI for application
####################################################
load("./nGramDF.RData")

ui <- fluidPage(
   
   # Application title
   titlePanel("Next Word Prediction Application: Data Science Capstone Project"),
   hr(),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       h3("Introduction"),
       p("It contains a predictive text model that 
         may guess the most likely word following a given text message 
         and, in addition, also provide several alternatives for what 
         the next word might be."), 
       p("For example, when we type: \"It was nice\", the next word 
         might be most likely to be \"to\"; or it might be something 
         like \"meeting\", \"seeing\", ..."),
       p("In this app, the predictions are based on machine learning 
         from written text data sourced from Internet blogs, news and 
         twitter."),
       
       hr(), 
       
       h3("Instructions"), 
       p("- To begin a new prediction task, simply delete any 
                existing texts in the provided textbox below, 
                Type English text below."),
       p("- For any prediction tasks, type English texts 
                (at least a single word) in the textbox; Note that 
                the app might not be able to make a prediction for 
                misspelled input."), 
       p("- As we type, the most-likely next word will 
                show and update at the same time below"), 
       p("- In addition, more alternative words will also be 
                available below."),

       width= 4
       ),
      
      # Show a plot of the generated distribution
     mainPanel(
       h3("Type English text below:"),
       textInput(inputId="textInput", 
                 label="", 
                 value="Data science capstone"),
       tags$head(tags$style(type="text/css", "#textInput {width: 650px}")),

       hr(),
       h3("The next word might be:"),
       textOutput(outputId = "nextWord"),
       h3("More alternatives:"),
       textOutput(outputId="nextWord2"), 
       textOutput(outputId="nextWord3"), 
       textOutput(outputId="nextWord4"), 
       textOutput(outputId="nextWord5"), 
       textOutput(outputId="nextWord6"),
       textOutput(outputId="nextWord7"),
       br()
      )
   )
)

####################################################
# Define server 
####################################################

source("./functions.R")

server <- function(input, output) {

  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  }) 
  
  withProgress(message="App initializing ...", value=NULL, {
    withProgress(message="Loading data", value=0, {
      load("nGramDF.RData", .GlobalEnv)
      for (i in 1:20) {
        incProgress(0.05, detail=paste(":", i*5, "%"))
        Sys.sleep(0.15)
      }
    })
    
    # Increment the top-level progress indicator
    incProgress(0.1)
  })
  
  preds <- reactive(predictNextWord(input$textInput))
  
  topPreds <- reactive(preds()[[1]])
  
  output$nextWord <- renderText({topPreds()[1]})
  output$nextWord2 <- renderText({topPreds()[2]})
  output$nextWord3 <- renderText({topPreds()[3]})
  output$nextWord4 <- renderText({topPreds()[4]})
  output$nextWord5 <- renderText({topPreds()[5]})
  output$nextWord6 <- renderText({topPreds()[6]})
  output$nextWord7 <- renderText({"..."})
}

# Run the application 
shinyApp(ui = ui, server = server)
