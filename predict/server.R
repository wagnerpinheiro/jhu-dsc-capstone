library(shiny)

source("predict.R")

shinyServer(function(input, output) {
   
  output$predict_txt <- renderText({
    
    phrase <- input$phrase
    
    paste0(predictnextword(phrase), sep="<hr>")
    
    
  })
  
  
})
