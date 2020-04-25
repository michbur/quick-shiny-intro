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
    
    rv <- reactiveValues(clicked_id = c())
    
    observeEvent(input[["plot1_click"]], {
        df_clicked <- nearPoints(df = df(), 
                                 coordinfo = input[["plot1_click"]], 
                                 maxpoints = 1, allRows = TRUE)
        
        selected_point <- which(df_clicked[["selected_"]])
        if(length(selected_point) > 0) {
            if(selected_point %in% rv[["clicked_id"]]) {
                rv[["clicked_id"]] <- setdiff(rv[["clicked_id"]], selected_point)
            } else {
                rv[["clicked_id"]] <- c(rv[["clicked_id"]], selected_point)
            }
        }
        
    })
    
    
    df <- reactive({
        data.frame(x = input[["slider1"]][1]:input[["slider1"]][2],
                   y = 4)
    })
    
    
    
    output[["plot1"]] <- renderPlot({
        plot_df <- df()
        plot_df[["selected"]] <- FALSE
        plot_df[rv[["clicked_id"]], "selected"] <- TRUE 
        
        ggplot(plot_df, aes(x = x, y = y, size = selected, color = selected)) +
            geom_point() +
            ggtitle(input[["text1"]])
    })
    
    output[["text_value"]] <- renderText({
        paste0("Title is ", input[["text1"]])
    })
    
    output[["click_value"]] <- renderPrint({
        rv[["clicked_id"]]
    })
}

shinyApp(ui = ui, server = server)
