
# load packages
library(shiny)
library(shinyWidgets)
library(tidyverse)

#source
# source("model.R")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),

    # Application title
    titlePanel("OSMI data explorer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h4("Step 1: Select variables"),
            htmlOutput("datapoints"),
            lapply(names(topics)[names(topics)!="Comment"], function(name)
                   pickerInput(
                       name,
                       name,
                       topics[[name]],
                       multiple = T,
                       options = list(`actions-box` = TRUE)
                   )),
            h4("Step 2: Update variables"),
            actionButton("update", "Update variables")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            lapply(c("X","Y","Group"), function(var)
                div(style="display:inline-block",
                    selectInput(
                        var,
                        paste0(var," variable"),
                        "",
                        multiple = F,
                        selectize = T))),
            htmlOutput("datapoints_chart"),
            dropdownButton(
                selectInput("chart_type_options",
                            "Chart type options",
                            "",
                            multiple = F,
                            selectize = T),
                circle = TRUE, status = "info", icon = icon("gear"), width = "300px",
                tooltip = tooltipOptions(title = "Click to see inputs !")),
            # uiOutput("chartUI"),
            # plotlyOutput("chart")
            leafletOutput("chart")
                    
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$blank <- renderUI({"blank"})
    
    observeEvent(input$update, {
        chosen <- lapply(names(topics), function(name) input[[name]]) %>% 
            unlist(.) %>% 
            c("None",.)
        lapply(c("X","Y","Group"), function(var)
            updateSelectInput(session = session,
                              inputId = var,
                              choices = chosen,
                              selected = NULL))
    })
    
    observe({
        chosen <- lapply(names(topics), function(name) input[[name]]) %>% 
            unlist(.)
        
        datapoints <- qualitative %>% 
            select(all_of(chosen)) %>% 
            drop_na(.) %>% 
            nrow(.) %>% 
            as.character(.)
        
        output$datapoints <- renderUI({HTML(paste0("<h5> <em> Number of complete datapoints: ",datapoints,"</em> </h5>"))})
        
    })
    observe({
        chosen_var_chart <- lapply(c("X","Y","Group"), function(var) input[[var]]) %>% 
            unlist(.)
        
        datapoints_chart <- qualitative %>% 
            select(any_of(chosen_var_chart)) %>% 
            drop_na(.) %>% 
            nrow(.) %>% 
            as.character(.)
        
        output$datapoints_chart <- renderUI({HTML(paste0("<h5> <em> Number of datapoints for chart: ",datapoints_chart,"</em> </h5>"))})
        
        chart_types <- chart_options(input$X,input$Y)
        updateSelectInput(session = session,
            inputId = "chart_type_options",
            choices = chart_types)
    })
    observe({
        # chart <- charting_wrapper(input$chart_type_options, input$X, input$Y, input$Group)
        # chart <- charting_functions[["scatterplot"]](input$X, input$Y, input$Group)
        # build <- reactive({
        #     charting_wrapper("scatterplot", input$X, input$Y, input$Group)
        # })
        # chart <- charting_wrapper(input$chart_type_options, input$X, input$Y, input$Group)
        # output$chart <-render_chart(chart, input$chart_type_options)
        # output$chartUI <- renderUI({
        #     output_chart("chart",input$chart_type_options)
        #     # plotlyOutput("chart")
        # })
        chart <- charting_wrapper(input$chart_type_options, input$X, input$Y, input$Group)
        output$chart <- renderLeaflet(chart)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
