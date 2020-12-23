
# load packages
library(shiny)
library(shinyWidgets)
library(tidyverse)

#source("functions.R")



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("OSMI data explorer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h4("Step 1: Select variables"),
            htmlOutput("datapoints"),
            lapply(names(topics), function(name)
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
                        selectize = T)))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observeEvent(input$update, {
        chosen <- lapply(names(topics), function(name) input[[name]]) %>% 
            unlist(.) %>% 
            c(NA,.)
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
}

# Run the application 
shinyApp(ui = ui, server = server)
