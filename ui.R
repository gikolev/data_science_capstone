shinyUI(fluidPage(
        
        titlePanel("Language Modeling with R"),
        
        sidebarLayout(
                sidebarPanel(
                        textInput("text", 
                                  h4("Enter text here"),
                                  value = "I would like a cup of")
                ),
                mainPanel(
                        h4("Prediction"),
                        textOutput("prediction")     
                )
        )
))
        
        

