library(shiny)
library(ggplot2)

ui <- fluidPage(


    titlePanel("AppAAAAAAAAA"),
    sliderInput("slider1", "Slide sth", min = 1, max = 100, 
                value = c(4, 5)),
    plotOutput("plot1")

    
)


server <- function(input, output) {

    output[["plot1"]] <- renderPlot({
        df <- data.frame(x = input[["slider1"]][1]:input[["slider1"]][2],
                         y = 4)
        ggplot(df, aes(x = x, y = y)) +
            geom_point()
    })
    
    
}

shinyApp(ui = ui, server = server)
