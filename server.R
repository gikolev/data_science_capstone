df_list <- readRDS("data/df_list.rds")
source("predict.R", local = TRUE)

shinyServer(function(input, output) {
        
        prediction <- reactive( {
                predict(input$text)
        })
        
        output$prediction <- renderPrint(prediction())
        
})

