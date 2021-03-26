
# load packages
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(kableExtra)
library(shinythemes)

#source
source("model.R")

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
            tabsetPanel(
                tabPanel("Chart",
                         dropdownButton(
                             selectInput("chartTypeOptions",
                                         "Chart type options",
                                         choices = "scatterplot",
                                         selected = "scatterplot", 
                                         multiple = F,
                                         selectize = T),
                             circle = TRUE, status = "info", icon = icon("gear"), width = "300px",
                             tooltip = tooltipOptions(title = "Click to see inputs!")),
                         # uiOutput("chartUI"),
                         plotlyOutput("chart")),
                         # conditionalPanel(condition = "input.chartTypeOptions %in% c('scatterplot','boxplot','violin','bar')",
                         #                  plotlyOutput("chart"))),
                        # conditionalPanel(condition = "input.chartTypeOptions == 'heatmap'",
                        #                  d3tree3Output("chart")),
                        # conditionalPanel(condition = "input.chartTypeOptions %in% c('us_map','world_map')",
                        #                  leafletOutput("chart")),
                        # try(d3tree3Output("d3tree")),
                        # try(plotlyOutput("plotly")),
                        # try(leafletOutput("leaflet"))
                        # d3tree3Output("chart")
                tabPanel("Variable_info", 
                         tableOutput("tbl"))
            )
        )
    )
)

# Define server logic
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
        if (chart_types == "") {
            chart_types <- "scatterplot"
        }
        chart_types <- chart_types[chart_types %in% chart_types_full] #only show plotly chart types
        updateSelectInput(session = session,
            inputId = "chartTypeOptions",
            choices = chart_types,
            selected = chart_types[1])
        
        output$tbl <- function() {
            req(input$X,input$Y,input$Group)
            column_info %>% 
                filter(Short_name %in% c(input$X,input$Y,input$Group)) %>% 
                .[,colSums(is.na(.))<nrow(.)] -> df
            numeric_cols <- ncol(df) - 4 
            df %>% 
                knitr::kable("html") %>%
                kable_styling("striped", full_width = F) %>%
                add_header_above(c("Question description" = 4, "Value map" = numeric_cols))
        }
        
    })
    observe({
        # chart <- charting_wrapper(input$chartTypeOptions, input$X, input$Y, input$Group)
        # chart <- charting_functions[["scatterplot"]](input$X, input$Y, input$Group)
        #     charting_wrapper("scatterplot", input$X, input$Y, input$Group)
        # })
        
        chart <- charting_wrapper(input$chartTypeOptions, input$X, input$Y, input$Group)
        # output$chart <- render_chart(chart, input$chartTypeOptions)
        # output$plotly <- tryCatch(
        #     expr = {renderPlotly({chart})},
        #     error = function(e) renderPlotly(""))
        # output$d3tree <- renderD3tree2({chart})
        # output$leaflet <- renderLeaflet({chart})
        # output$chartUI <- renderUI({
            # if (input$chartTypeOptions %in% c("scatterplot","boxplot","violin","bar")) {
            #     plotlyOutput("chart")
            # } else if (input$chartTypeOptions == "heatmap") {
            #     d3tree3Output("chart")
            # } else {
            #     leafletOutput("chart")
            # }
        output$chart <- renderPlotly({chart})
        # } else if (input$chartTypeOptions == "heatmap") {
        #     output$chart <- renderD3tree2({chart})
        # } else {
        #     output$chart <- renderLeaflet({chart})
        # }
        # })
        # chart <- charting_wrapper("heatmap", input$X, input$Y, input$Group)
        # output$chart <- renderD3tree3(chart)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
