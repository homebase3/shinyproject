
# load packages
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(kableExtra)
library(shinythemes)
library(shinyalert)
library(cicerone)
library(rintrojs)

# source
source("model.R")

# define tour
tour_list <- list()
tour_list[['#tour']] <- '<b> Welcome to the tour! </b> <br> Click through for a full introduction of the workflow!'
tour_list[[paste0("#",gsub(" ","_",names(topics)[1]))]] <- " <b> Select your variables. </b> <br> The OSMI data has over 80 variables. In this step, you'll select the variables that you will chart. Variables are grouped by topic. If you are interested in a given topic, click on it and select the relevant variables."
tour_list[['#datapoints']] <- 'Remember to check the number of complete datapoints for your selected variables to ensure that there are enough datapoints before you move on.'
tour_list[['#update']] <- "<b> Update your variables in the charting interface. </b> <br> Once you've selected your variables, click this button to migrate your variables to the charting interface. You can always come back and reselect your variables at any time. If you do, just click this button again and they will update."
tour_list[['#step_3']] <- "<b> Select your X, Y, and Group variables for charting </b> <br> With your smaller sample of variables, select the X, Y, and Group variables you'd like to chart. You only need to choose and X variable and a Y variable or Group Variable to chart. More on this next."
tour_list[['#step_4']] <- "<b> Select your chart type. </b> <br> Now that you've selected your variables to chart, you can choose from a menu of charting options for these choices by clicking on the blue gear below. The menu updates dynamically based on the types of variables (e.g. categorical, continuous) you have chosen in the prior step. If no variables have been selected or there is a charting error, the chart will appear blank. In case you're curious on more specific info on the variables you've chosen to chart -- like the exact question that was asked, click over to the \"Variable info\" tab anytime. You won't lose your charting progress."

tour_list %>% 
  do.call(rbind,.) %>% 
  as.data.frame(.) %>% 
  rownames_to_column(.,var= 'element') %>% 
  rename(.,intro = V1) -> tour_df

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("flatly"),
  # use shiny alert
  useShinyalert(),

  #use introjs
  introjsUI(),

  # Application title
  titlePanel("OSMI data explorer"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      actionButton("tour", "Take the tour"),
      h4("Step 1: Select variables"),
      htmlOutput("datapoints"),
      lapply(names(topics)[names(topics) != "Comment"], function(name) {
        pickerInput(
          gsub(" ", "_", name),
          name,
          topics[[name]],
          multiple = T,
          options = list(`actions-box` = TRUE)
        )
      }),
      h4("Step 2: Update variables"),
      actionButton("update", "Update variables")
    ),

    # build main panel for plots
    mainPanel(
      h4("Step 3: Select variables for charting",id = "step_3"),
      lapply(c("X_var", "Y_var", "Group_var"), function(var) {
        div(
          style = "display:inline-block",
          selectInput(
            var,
            paste0(var, " variable"),
            "",
            multiple = F,
            selectize = T
          )
        )
      }),
      htmlOutput("datapoints_chart"),
      h4("Step 4: Explore charts and variable information", id = "step_4"),
      tabsetPanel(id = "tabz",
        tabPanel(
          "Chart",
          dropdownButton(
            selectInput("chartTypeOptions",
              "Chart type options",
              choices = "scatterplot",
              selected = "scatterplot",
              multiple = F,
              selectize = T
            ),
            circle = TRUE, status = "info", icon = icon("gear"), width = "300px",
            tooltip = tooltipOptions(title = "Click to see inputs!")
          ),
          plotlyOutput("chart")
        ),

        tabPanel(
          id = "Variable_info",
          title = "Variable info",
          tableOutput("tbl")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # initial alert upon opening app
  shinyalert(
    title = "Welcome to the OSMI explorer",
    text = "This is explorer is meant to help users make sense of Open Sourcing Mental Illness' survey data on the tech industry from 2019. It allows you to identify questions of interest, find questions with non-sparse responses quickly, and then build visualizations based on variables in the survey that can help you make sense of the data. If this is your first time using the explorer, please click tour on the top left for more information on the workflow."
  )

  # guided app onboarding tour
  observeEvent(input$tour,{
    introjs(session,
            options = list(steps = tour_df))
  })
  
  # # start tour when tour button is clicked
  # guide$init()
  # observeEvent(input$tour, {
  #   guide$start()
  # })

  # topic update observer
  observeEvent(input$update, {
    chosen <- lapply(names(topics), function(name) input[[name]]) %>%
      unlist(.) %>%
      c("None", .)
    lapply(c("X_var", "Y_var", "Group_var"), function(var) {
      updateSelectInput(
        session = session,
        inputId = var,
        choices = chosen,
        selected = NULL
      )
    })
  })

  observe({
    chosen <- lapply(names(topics), function(name) input[[name]]) %>%
      unlist(.)

    datapoints <- qualitative %>%
      select(all_of(chosen)) %>%
      drop_na(.) %>%
      nrow(.) %>%
      as.character(.)

    output$datapoints <- renderUI({
      HTML(paste0("<h5> <em> Number of complete datapoints: ", datapoints, "</em> </h5>"))
    })
  })
  observe({
    chosen_var_chart <- lapply(c("X_var", "Y_var", "Group_var"), function(var) input[[var]]) %>%
      unlist(.)

    datapoints_chart <- qualitative %>%
      select(any_of(chosen_var_chart)) %>%
      drop_na(.) %>%
      nrow(.) %>%
      as.character(.)

    output$datapoints_chart <- renderUI({
      HTML(paste0("<h5> <em> Number of datapoints for chart: ", datapoints_chart, "</em> </h5>"))
    })

    chart_types <- chart_options(input$X_var, input$Y_var)
    if (chart_types == "") {
      chart_types <- "scatterplot"
    }
    chart_types <- chart_types[chart_types %in% chart_types_full] # only show plotly chart types
    updateSelectInput(
      session = session,
      inputId = "chartTypeOptions",
      choices = chart_types,
      selected = chart_types[1]
    )

    output$tbl <- function() {
      req(input$X_var, input$Y_var, input$Group_var)
      column_info %>%
        filter(Short_name %in% c(input$X_var, input$Y_var, input$Group_var)) %>%
        .[, colSums(is.na(.)) < nrow(.)] -> df
      numeric_cols <- ncol(df) - 4
      df %>%
        knitr::kable("html") %>%
        kable_styling("striped", full_width = F) %>%
        add_header_above(c("Question description" = 4, "Value map" = numeric_cols))
    }
  })
  observe({
    chart <- charting_wrapper(input$chartTypeOptions, input$X_var, input$Y_var, input$Group_var)
    output$chart <- renderPlotly({
      chart
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
