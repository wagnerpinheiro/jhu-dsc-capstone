library(shiny)

shinyUI(
  navbarPage(
    "Text Prediction (JHU Data Science Specialization Capstone Project)",
    tabPanel("App",
             splitLayout(
               textInput("phrase", "Start typing your text here:", width = 500),
               htmlOutput("predict_txt", style = "text-align: left; padding-top: 40px; line-height: 5px;", width =
                            100)
             )),
    tabPanel(
      "Documentation",
      p(
        "This is a text prediction application for the JHU Data Science Specialization Capstone Project. Type in the phrase, the application will return a maximum of best 5 predicted words."
      ),
      p(
        "View ", a("here", href='http://rpubs.com/wagnerpinheiro/jhu-dsc-cap-final', target='_blank'), " the final project presentation."
      )
    )
    
  )
)
