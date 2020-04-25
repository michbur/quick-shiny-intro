library(shiny)
library(ggplot2)

# https://github.com/rstudio/reactlog
# renderUI albo zamiast updateSliderInput

ui <- fluidPage(
    
    
    titlePanel("AppAAAAAAAAA"),
    textInput("text1", "Enter title", value = "title"),
    uiOutput("long_text_box"),
    numericInput("max_slider", "Input max slider length", min = 10, max = 20, 
                 value = 10),
    sliderInput("slider1", "Slide sth", min = 1, max = 100, 
                value = c(4, 5)),
    textOutput("text_value"),
    plotOutput("plot1", click = "plot1_click"),
    verbatimTextOutput("click_value")
    
    
)


server <- function(input, output, session) {
    
    rv <- reactiveValues(clicked_id = c())
    
    observeEvent(input[["max_slider"]], {
        #renderUI
        updateSliderInput(session, "slider1", "Slide sth", min = 1, 
                          max = input[["max_slider"]], 
                          value = c(4, 5))
    })
    
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
    
    output[["long_text_box"]] <- renderUI({
        if(nchar(input[["text1"]]) > 10) {
            textOutput("long_text")  
        } else {
            NULL
        }
    })
    
    output[["long_text"]] <- renderText({
        "Congrats, you have written a long text."
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
