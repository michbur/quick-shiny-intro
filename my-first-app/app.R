library(shiny)
library(ggplot2)

# https://github.com/rstudio/reactlog

ui <- fluidPage(


    titlePanel("AppAAAAAAAAA"),
    textInput("text1", "Enter title", value = "title"),
    sliderInput("slider1", "Slide sth", min = 1, max = 100, 
                value = c(4, 5)),
    textOutput("text_value"),
    plotOutput("plot1", click = "plot1_click"),
    verbatimTextOutput("click_value")

    
)


server <- function(input, output) {

    
    df <- reactive({
        data.frame(x = input[["slider1"]][1]:input[["slider1"]][2],
                   y = 4)
    })
    
    df_clicked <- reactive({
        nearPoints(df = df(), coordinfo = input[["plot1_click"]], 
                   maxpoints = 1, allRows = TRUE) 
    })
    
    output[["plot1"]] <- renderPlot({
        ggplot(df_clicked(), aes(x = x, y = y, 
                                 size = selected_, color = selected_)) +
            geom_point() +
            ggtitle(input[["text1"]])
    })
    
    output[["text_value"]] <- renderText({
        paste0("Title is ", input[["text1"]])
    })
    
    output[["click_value"]] <- renderPrint({
        nearPoints(df = df(), coordinfo = input[["plot1_click"]], 
                   maxpoints = 1, allRows = TRUE) 
    })
}

shinyApp(ui = ui, server = server)
