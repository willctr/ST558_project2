# app.R
# NFL play-by-play
# shiny app for subsetting, summarizing, and plotting NFL play-by-play data.

library(shiny)
library(dplyr)
library(ggplot2)

# data
nfl <- readRDS("nfl_pbp.rds")

# numeric variables the user can filter on
numeric_vars <- c(
  "Yards gained" = "yards_gained",
  "Yards to go" = "ydstogo",
  "Yards from opponent's end zone" = "yrdline100",
  "Score differential" = "score_diff",
  "Seconds left in regulation" = "time_secs",
  "Air yards (passes only)" = "air_yards",
  "Expected points added (EPA)" = "epa",
  "Win probability" = "win_prob"
)

#  build a range slider for a numeric variable
make_range_slider <- function(id, var) {
  rng <- range(nfl[[var]], na.rm = TRUE)
  label <- names(numeric_vars)[numeric_vars == var]
  sliderInput(id, label = paste("Range of", label),
              min = floor(rng[1]), max = ceiling(rng[2]),
              value = c(floor(rng[1]), ceiling(rng[2])))
}

# UI
ui <- fluidPage(
  titlePanel("NFL Play-by-Play Explorer (2009-2016)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Subset the data"),
      
      # categorical filter: play type
      checkboxGroupInput(
        "play_type", "Play type",
        choices = levels(nfl$play_type),
        selected = levels(nfl$play_type)
      ),
      
      # categorical filter: down
      radioButtons(
        "down", "Down",
        choices = c("All", levels(nfl$down)),
        selected = "All",
        inline = TRUE
      ),
      
      # categorical filter: conference of the offense
      radioButtons(
        "conference", "Offense's conference",
        choices = c("All", levels(nfl$conference)),
        selected = "All",
        inline = TRUE
      ),
      
      hr(),
      
      # numeric filter: choose a variable, then a slider appears
      selectInput(
        "num_var1", "Numeric variable 1",
        choices = c("None" = "none", numeric_vars),
        selected = "none"
      ),
      uiOutput("num_slider1"),
      
      # numeric filter 
      selectInput(
        "num_var2", "Numeric variable 2",
        choices = c("None" = "none", numeric_vars),
        selected = "none"
      ),
      uiOutput("num_slider2"),
      
      helpText("Filtering on a numeric variable drops plays where it is missing",
               "(for example, air yards on runs)."),
      
      actionButton("apply", "Apply filters", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        selected = "About",
        # about tab
        tabPanel(
          "About",
          h3("About this app"),
          p("This app lets you explore every pass, run, and sack from the",
            "2009-2016 NFL seasons. You can subset the",
            "data to the scenarios of interest, download it, and build",
            "your own tables and plots."),
          
          h4("The data"),
          p("The data was scraped from the NFL's play-by-play feed with the",
            tags$code("nflscrapR"), "package. Each row is one play, with the",
            "game situation (down, distance, field position, score, time),",
            "what happened (play type, yards gained, pass details), and",
            "advanced metrics like expected points added (EPA) and win probability.",
            "More information: ",
            a("Kaggle", href = "https://www.kaggle.com/datasets/maxhorowitz/nflplaybyplay2009to2016",
              target = "_blank"), "."),
          
          h4("How to use the app"),
          tags$ul(
            tags$li(strong("Sidebar:"), "choose play types, a down, and a conference,",
                    "and optionally pick up to two numeric variables to filter by range.",
                    "Click", strong("Apply filters"), "to update the data. Nothing",
                    "changes until you click Apply filters."),
            tags$li(strong("Data download:"), "view the filtered data in a table and",
                    "save it as a csv file."),
            tags$li(strong("Data exploration:"), "build contingency tables, numeric",
                    "summaries, and plots of the filtered data, choosing which",
                    "variables to summarize, color, and facet by.")
          ),
          
          img(src = "nfl_logo.png", width = "100%",
              style = "max-width: 600px; margin-top: 10px;")
        ),
        # Data download tab
        tabPanel(
          "Data Download",
          br(),
          textOutput("n_rows"),
          br(),
          downloadButton("download_data", "Download filtered data (CSV)"),
          br(), br(),
          DT::dataTableOutput("data_table")
        ),
        tabPanel("Data Exploration",
                 p("Data Exploration tab."))
      )
    )
  )
)

# server
server <- function(input, output, session) {
  
  # dynamic sliders: only shown once a numeric variable is chosen
  output$num_slider1 <- renderUI({
    req(input$num_var1 != "none")
    make_range_slider("num_range1", input$num_var1)
  })
  
  output$num_slider2 <- renderUI({
    req(input$num_var2 != "none")
    make_range_slider("num_range2", input$num_var2)
  })
  
  # subset data: only updates when the Apply button is pressed
  subset_data <- eventReactive(input$apply, {
    df <- nfl |>
      filter(play_type %in% input$play_type)
    
    if (input$down != "All") {
      df <- filter(df, down == input$down)
    }
    if (input$conference != "All") {
      df <- filter(df, conference == input$conference)
    }
    
    if (input$num_var1 != "none" && !is.null(input$num_range1)) {
      df <- filter(df, between(.data[[input$num_var1]],
                               input$num_range1[1], input$num_range1[2]))
    }
    if (input$num_var2 != "none" && !is.null(input$num_range2)) {
      df <- filter(df, between(.data[[input$num_var2]],
                               input$num_range2[1], input$num_range2[2]))
    }
    
    df
  }, ignoreNULL = FALSE)
  
  # Data download tab
  # number of plays in the current subset
  output$n_rows <- renderText({
    paste(format(nrow(subset_data()), big.mark = ","), "plays selected")
  })
  
  # table of the subset data
  output$data_table <- DT::renderDataTable({
    DT::datatable(subset_data(),
                  options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # save the subset as a csv
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("nfl_pbp_subset_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(subset_data(), file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui, server)