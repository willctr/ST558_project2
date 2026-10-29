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

# numeric variables for the exploration tab
explore_num_vars <- c(
  numeric_vars,
  "Yards after catch (passes only)" = "yac",
  "Win probability added (WPA)" = "wpa"
)

#  build a range slider for a numeric variable
make_range_slider <- function(id, var) {
  rng <- range(nfl[[var]], na.rm = TRUE)
  label <- names(numeric_vars)[numeric_vars == var]
  sliderInput(id, label = paste("Range of", label),
              min = floor(rng[1]), max = ceiling(rng[2]),
              value = c(floor(rng[1]), ceiling(rng[2])))
}

# categorical variables for tables, bar charts, coloring, and grouping
cat_vars <- c(
  "Play type" = "play_type",
  "Down" = "down",
  "Quarter" = "quarter",
  "Season" = "season",
  "Field zone" = "field_zone",
  "Game situation (score)" = "game_situation",
  "Conference (offense)" = "conference",
  "Division (offense)" = "division",
  "Pass length" = "pass_length",
  "Pass location" = "pass_location",
  "Pass outcome" = "pass_outcome",
  "Run location" = "run_location",
  "First down gained" = "first_down",
  "Touchdown" = "touchdown",
  "Interception" = "interception",
  "Fumble" = "fumble",
  "Offense team" = "posteam"
)

# variables with few enough levels to facet by (no team, 32 levels)
facet_vars <- cat_vars[cat_vars != "posteam"]

# readable label for any variable name
var_label <- function(var) {
  all_vars <- c(explore_num_vars, cat_vars)
  names(all_vars)[all_vars == var][1]
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
        # data download tab
        tabPanel(
          "Data Download",
          br(),
          textOutput("n_rows"),
          br(),
          downloadButton("download_data", "Download filtered data (CSV)"),
          br(), br(),
          DT::dataTableOutput("data_table")
        ),
        
        # data exploration tab
        tabPanel(
          "Data Exploration",
          br(),
          radioButtons(
            "summary_type", "Type of summary",
            choices = c("Categorical", "Numeric"),
            selected = "Categorical",
            inline = TRUE
          ),
          
          # categorical summaries
          conditionalPanel(
            condition = "input.summary_type == 'Categorical'",
            fluidRow(
              column(4, selectInput("cat_var1", "Variable",
                                    choices = cat_vars, selected = "play_type")),
              column(4, selectInput("cat_var2", "Second variable (optional)",
                                    choices = c("None" = "none", cat_vars),
                                    selected = "down")),
              column(4, selectInput("cat_facet", "Facet by (optional)",
                                    choices = c("None" = "none", facet_vars),
                                    selected = "none"))
            ),
            radioButtons(
              "bar_position", "Bar style (when a second variable is chosen)",
              choices = c("Side-by-side" = "dodge",
                          "Stacked" = "stack",
                          "Proportions" = "fill"),
              selected = "dodge",
              inline = TRUE
            ),
            h4("Contingency table"),
            tableOutput("cat_table"),
            h4("Bar chart"),
            plotOutput("cat_plot", height = "450px")
          ),
          
          # numeric summaries
          conditionalPanel(
            condition = "input.summary_type == 'Numeric'",
            fluidRow(
              column(4, selectInput("num_var", "Numeric variable",
                                    choices = explore_num_vars,
                                    selected = "yards_gained")),
              column(4, selectInput("num_group", "Group / color by",
                                    choices = c("None" = "none", cat_vars),
                                    selected = "play_type")),
              column(4, selectInput("num_facet", "Facet by (optional)",
                                    choices = c("None" = "none", facet_vars),
                                    selected = "none"))
            ),
            h4("Summary statistics"),
            tableOutput("num_table"),
            h4("Plot"),
            radioButtons(
              "plot_type", "Plot type",
              choices = c("Histogram", "Density", "Box plot",
                          "Scatter plot", "Heatmap"),
              selected = "Histogram",
              inline = TRUE
            ),
            # extra inputs only needed by some plot types
            conditionalPanel(
              condition = "input.plot_type == 'Scatter plot'",
              selectInput("num_var_y", "Y-axis variable",
                          choices = explore_num_vars, selected = "epa"),
              helpText("Scatter plots show a random sample of up to 5,000 plays.")
            ),
            conditionalPanel(
              condition = "input.plot_type == 'Heatmap'",
              selectInput("heat_var", "Second categorical variable (heatmap rows)",
                          choices = facet_vars, selected = "down"),
              helpText("Each tile shows the mean of the numeric variable.",
                       "The group variable sets the columns.")
            ),
            plotOutput("num_plot", height = "450px")
          )
        )
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
  
  # data download tab
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
  
  # data exploration tab: categorical
  # subset data with missing values removed for the chosen variables
  cat_data <- reactive({
    vars <- c(input$cat_var1, input$cat_var2, input$cat_facet)
    vars <- vars[vars != "none"]
    subset_data() |>
      filter(if_all(all_of(vars), ~ !is.na(.x))) |>
      droplevels()
  })
  
  # checks shared by the table and the plot
  check_cat_inputs <- function() {
    validate(
      need(input$cat_var1 != input$cat_var2,
           "Choose two different variables."),
      need(input$cat_facet == "none" ||
             !(input$cat_facet %in% c(input$cat_var1, input$cat_var2)),
           "The facet variable must be different from the plotted variables."),
      need(nrow(cat_data()) > 0,
           "No plays match these filters. Change the sidebar or variables.")
    )
  }
  
  output$cat_table <- renderTable({
    check_cat_inputs()
    df <- cat_data()
    
    if (input$cat_var2 == "none") {
      # one-way table with proportions
      df |>
        count(.data[[input$cat_var1]], name = "Count") |>
        mutate(Proportion = round(Count / sum(Count), 3)) |>
        rename(!!var_label(input$cat_var1) := 1)
    } else {
      # two-way table: rows = variable 1, columns = variable 2
      tab <- as.data.frame.matrix(table(df[[input$cat_var1]], df[[input$cat_var2]]))
      row_name <- paste(var_label(input$cat_var1), "/", var_label(input$cat_var2))
      cbind(setNames(data.frame(rownames(tab)), row_name), tab)
    }
  }, digits = 3)
  
  output$cat_plot <- renderPlot({
    check_cat_inputs()
    df <- cat_data()
    
    if (input$cat_var2 == "none") {
      p <- ggplot(df, aes(x = .data[[input$cat_var1]])) +
        geom_bar(fill = "#2c7fb8") +
        labs(title = paste("Number of plays by", var_label(input$cat_var1)),
             x = var_label(input$cat_var1), y = "Number of plays")
    } else {
      y_label <- if (input$bar_position == "fill") "Proportion of plays" else "Number of plays"
      p <- ggplot(df, aes(x = .data[[input$cat_var1]],
                          fill = .data[[input$cat_var2]])) +
        geom_bar(position = input$bar_position) +
        labs(title = paste(var_label(input$cat_var1), "by",
                           var_label(input$cat_var2)),
             x = var_label(input$cat_var1), y = y_label,
             fill = var_label(input$cat_var2))
    }
    
    if (input$cat_facet != "none") {
      p <- p + facet_wrap(vars(.data[[input$cat_facet]])) +
        labs(subtitle = paste("Faceted by", var_label(input$cat_facet)))
    }
    
    p + theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # data exploration tab
  num_data <- reactive({
    vars <- c(input$num_var, input$num_group, input$num_facet)
    if (input$plot_type == "Scatter plot") vars <- c(vars, input$num_var_y)
    if (input$plot_type == "Heatmap") vars <- c(vars, input$heat_var)
    vars <- unique(vars[vars != "none"])
    subset_data() |>
      filter(if_all(all_of(vars), ~ !is.na(.x))) |>
      droplevels()
  })
  
  output$num_table <- renderTable({
    validate(need(nrow(num_data()) > 0,
                  "No plays match these filters. Try changing the sidebar or variables."))
    df <- num_data()
    if (input$num_group != "none") {
      df <- group_by(df, .data[[input$num_group]])
    }
    stats <- df |>
      summarize(
        n = n(),
        Mean = mean(.data[[input$num_var]]),
        Median = median(.data[[input$num_var]]),
        SD = sd(.data[[input$num_var]]),
        Min = min(.data[[input$num_var]]),
        Max = max(.data[[input$num_var]]),
        .groups = "drop"
      ) |>
      mutate(n = as.integer(n))
    
    # use the readable label for the group column
    if (input$num_group != "none") {
      names(stats)[1] <- var_label(input$num_group)
    }
    stats
  }, digits = 2)
  
  output$num_plot <- renderPlot({
    group <- input$num_group
    x_lab <- var_label(input$num_var)
    validate(
      need(nrow(num_data()) > 0,
           "No plays match these filters. Try changing the sidebar or variables."),
      need(input$num_facet == "none" || input$num_facet != group,
           "The facet variable must be different from the group variable."),
      need(input$plot_type != "Scatter plot" || input$num_var != input$num_var_y,
           "Choose two different numeric variables for the scatter plot."),
      need(input$plot_type != "Heatmap" || group != "none",
           "The heatmap needs a group variable for its columns."),
      need(input$plot_type != "Heatmap" || input$heat_var != group,
           "Choose two different categorical variables for the heatmap.")
    )
    df <- num_data()
    has_group <- group != "none"
    
    # base plot for the chosen plot type; color/fill by the group variable if chosen
    if (input$plot_type == "Histogram") {
      p <- if (has_group) {
        ggplot(df, aes(x = .data[[input$num_var]], fill = .data[[group]])) +
          geom_histogram(bins = 40, alpha = 0.6, position = "identity")
      } else {
        ggplot(df, aes(x = .data[[input$num_var]])) +
          geom_histogram(bins = 40, fill = "#2c7fb8")
      }
      p <- p + labs(title = paste("Distribution of", x_lab), x = x_lab,
                    y = "Number of plays", fill = var_label(group))
      
    } else if (input$plot_type == "Density") {
      p <- if (has_group) {
        ggplot(df, aes(x = .data[[input$num_var]], fill = .data[[group]])) +
          geom_density(alpha = 0.4)
      } else {
        ggplot(df, aes(x = .data[[input$num_var]])) +
          geom_density(fill = "#2c7fb8", alpha = 0.6)
      }
      p <- p + labs(title = paste("Density of", x_lab), x = x_lab,
                    y = "Density", fill = var_label(group))
      
    } else if (input$plot_type == "Box plot") {
      p <- if (has_group) {
        ggplot(df, aes(x = .data[[group]], y = .data[[input$num_var]],
                       fill = .data[[group]])) +
          geom_boxplot(outlier.alpha = 0.1, show.legend = FALSE) +
          labs(title = paste(x_lab, "by", var_label(group)), x = var_label(group))
      } else {
        ggplot(df, aes(x = "", y = .data[[input$num_var]])) +
          geom_boxplot(fill = "#2c7fb8", outlier.alpha = 0.1) +
          labs(title = x_lab, x = NULL)
      }
      p <- p + labs(y = x_lab)
      
    } else if (input$plot_type == "Scatter plot") {
      # sample so the plot renders quickly
      set.seed(558)
      df_plot <- slice_sample(df, n = min(5000, nrow(df)))
      y_lab <- var_label(input$num_var_y)
      p <- if (has_group) {
        ggplot(df_plot, aes(x = .data[[input$num_var]], y = .data[[input$num_var_y]],
                            color = .data[[group]])) +
          geom_point(alpha = 0.4)
      } else {
        ggplot(df_plot, aes(x = .data[[input$num_var]], y = .data[[input$num_var_y]])) +
          geom_point(alpha = 0.4, color = "#2c7fb8")
      }
      p <- p + labs(title = paste(y_lab, "vs.", x_lab),
                    subtitle = paste("Random sample of", nrow(df_plot), "plays"),
                    x = x_lab, y = y_lab, color = var_label(group))
      
    } else {
      # heatmap: mean of the numeric variable for each pair of categories
      facet_cols <- if (input$num_facet != "none") input$num_facet else character(0)
      p <- df |>
        group_by(across(all_of(c(group, input$heat_var, facet_cols)))) |>
        summarize(mean_value = mean(.data[[input$num_var]]), .groups = "drop") |>
        ggplot(aes(x = .data[[group]], y = .data[[input$heat_var]], fill = mean_value)) +
        geom_tile(color = "white") +
        geom_text(aes(label = round(mean_value, 2)), size = 3.5) +
        scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac",
                             midpoint = mean(df[[input$num_var]])) +
        labs(title = paste("Mean", x_lab, "by", var_label(group), "and",
                           var_label(input$heat_var)),
             x = var_label(group), y = var_label(input$heat_var),
             fill = paste("Mean", x_lab))
    }
    
    if (input$num_facet != "none") {
      p <- p + facet_wrap(vars(.data[[input$num_facet]])) +
        labs(caption = paste("Faceted by", var_label(input$num_facet)))
    }
    
    p + theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
}


shinyApp(ui, server)