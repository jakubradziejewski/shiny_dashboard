library(shiny)
library(shinydashboard)
library(plotly)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "Boredom Level & Music Habits"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data Explorer", tabName = "data_explorer", icon = icon("database")),
      menuItem("Genre & Mental Health", tabName = "genre_mental_health", icon = icon("heartbeat")),
      menuItem("Frequency Analysis", tabName = "frequency_analysis", icon = icon("music")),
      menuItem("Hours vs. Health", tabName = "hours_vs_health", icon = icon("clock-o")),
      menuItem("Mental Health Clusters", tabName = "mental_health_clusters", icon = icon("brain"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              h2("Music Habits Overview"),
              fluidRow(
                box(title = "Boredom Level Input", status = "primary", solidHeader = TRUE,
                    sliderInput("testRange", "Boredom level:", min = 0, max = 100, value = c(20, 80)),
                    textOutput("testText"),
                    width = 12
                )
              )
      ),
      
      tabItem(tabName = "data_explorer",
              h2("Explore the Raw Data"),
              p("This table shows the processed data. You can scroll horizontally to see all columns."),
              dataTableOutput("data")
      ),
      
      tabItem(tabName = "genre_mental_health",
              h2("Mental Health Metrics by Favorite Genre"),
              fluidRow(
                box(title = "Select Music Genre", status = "info", solidHeader = TRUE,
                    selectInput("selected_genre", "Choose a Favorite Genre:",
                                choices = NULL),
                    width = 12
                )
              ),
              fluidRow(
                box(title = "Anxiety Distribution", status = "warning", solidHeader = TRUE,
                    plotlyOutput("plot_anxiety_dist"),
                    width = 6
                ),
                box(title = "Depression Distribution", status = "warning", solidHeader = TRUE,
                    plotlyOutput("plot_depression_dist"),
                    width = 6
                )
              ),
              fluidRow(
                box(title = "Insomnia Distribution", status = "warning", solidHeader = TRUE,
                    plotlyOutput("plot_insomnia_dist"),
                    width = 6
                ),
                box(title = "OCD Distribution", status = "warning", solidHeader = TRUE,
                    plotlyOutput("plot_ocd_dist"),
                    width = 6
                )
              ),
              fluidRow(
                box(title = "Music Effects Distribution", status = "warning", solidHeader = TRUE,
                    plotlyOutput("plot_music_effects_dist"),
                    width = 12
                )
              )
      ),
      
      tabItem(tabName = "frequency_analysis",
              h2("Mental Health Metrics by Listening Frequency of Specific Genres and Characteristics"), # Updated title
              fluidRow(
                box(title = "Select Genre Frequency and Level", status = "info", solidHeader = TRUE,
                    column(6,
                           selectInput("selected_freq_column", "Choose a Genre Frequency:",
                                       choices = NULL)
                    ),
                    column(6,
                           selectInput("selected_frequency_level", "Choose a Frequency Level:",
                                       choices = NULL)
                    ),
                    width = 12
                )
              ),
              fluidRow(
                box(title = "Filter by Characteristics", status = "info", solidHeader = TRUE,
                    # Added characteristic filter dropdowns to this tab
                    column(4, selectInput("filter_while_working_freq", "While Working:", choices = c("All", "Yes", "No"), selected = "All")), # New input ID
                    column(4, selectInput("filter_instrumentalist_freq", "Instrumentalist:", choices = c("All", "Yes", "No"), selected = "All")), # New input ID
                    column(4, selectInput("filter_composer_freq", "Composer:", choices = c("All", "Yes", "No"), selected = "All")), # New input ID
                    column(4, selectInput("filter_exploratory_freq", "Exploratory:", choices = c("All", "Yes", "No"), selected = "All")), # New input ID
                    column(4, selectInput("filter_foreign_languages_freq", "Foreign Languages:", choices = c("All", "Yes", "No"), selected = "All")), # New input ID
                    width = 12
                )
              ),
              fluidRow(
                box(title = "Anxiety Distribution", status = "warning", solidHeader = TRUE,
                    plotlyOutput("plot_anxiety_freq_dist"),
                    width = 6
                ),
                box(title = "Depression Distribution", status = "warning", solidHeader = TRUE,
                    plotlyOutput("plot_depression_freq_dist"),
                    width = 6
                )
              ),
              fluidRow(
                box(title = "Insomnia Distribution", status = "warning", solidHeader = TRUE,
                    plotlyOutput("plot_insomnia_freq_dist"),
                    width = 6
                ),
                box(title = "OCD Distribution", status = "warning", solidHeader = TRUE,
                    plotlyOutput("plot_ocd_freq_dist"),
                    width = 6
                )
              ),
              fluidRow(
                box(title = "Music Effects Distribution", status = "warning", solidHeader = TRUE,
                    plotlyOutput("plot_music_effects_freq_dist"),
                    width = 12
                )
              )
      ),
      
      tabItem(tabName = "hours_vs_health",
              h2("Hours per Day vs. Mental Health by Genre Frequency and Characteristics"),
              fluidRow(
                box(title = "Select Genre Frequency and Level", status = "info", solidHeader = TRUE,
                    column(6,
                           selectInput("selected_freq_column_scatter", "Choose a Genre Frequency:",
                                       choices = NULL)
                    ),
                    column(6,
                           selectInput("selected_frequency_level_scatter", "Choose a Frequency Level:",
                                       choices = NULL)
                    ),
                    width = 12
                )
              ),
              fluidRow(
                box(title = "Filter by Characteristics", status = "info", solidHeader = TRUE,
                    column(4, selectInput("filter_while_working", "While Working:", choices = c("All", "Yes", "No"), selected = "All")),
                    column(4, selectInput("filter_instrumentalist", "Instrumentalist:", choices = c("All", "Yes", "No"), selected = "All")),
                    column(4, selectInput("filter_composer", "Composer:", choices = c("All", "Yes", "No"), selected = "All")),
                    column(4, selectInput("filter_exploratory", "Exploratory:", choices = c("All", "Yes", "No"), selected = "All")),
                    column(4, selectInput("filter_foreign_languages", "Foreign Languages:", choices = c("All", "Yes", "No"), selected = "All")),
                    width = 12
                )
              ),
              fluidRow(
                box(title = "Hours per Day vs. Anxiety", status = "danger", solidHeader = TRUE,
                    plotlyOutput("plot_hours_anxiety_scatter"),
                    width = 6
                ),
                box(title = "Hours per Day vs. Depression", status = "danger", solidHeader = TRUE,
                    plotlyOutput("plot_hours_depression_scatter"),
                    width = 6
                )
              ),
              fluidRow(
                box(title = "Hours per Day vs. Insomnia", status = "danger", solidHeader = TRUE,
                    plotlyOutput("plot_hours_insomnia_scatter"),
                    width = 6
                ),
                box(title = "OCD Distribution", status = "danger", solidHeader = TRUE,
                    plotlyOutput("plot_hours_ocd_scatter"),
                    width = 6
                )
              ),
              fluidRow(
                box(title = "Hours per Day vs. Music Effects", status = "danger", solidHeader = TRUE,
                    plotlyOutput("plot_hours_music_effects_scatter"),
                    width = 12
                )
              )
      ),
      tabItem(tabName = "mental_health_clusters",
              h2("Mental Health Profile Clusters"),
              fluidRow(
                box(title = "Cluster Controls", status = "primary", solidHeader = TRUE,
                    column(6, 
                           selectInput("cluster_var1", "X-Axis Variable:", 
                                       choices = c("anxiety", "depression", "insomnia", "ocd"),
                                       selected = "anxiety")
                    ),
                    column(6, 
                           selectInput("cluster_var2", "Y-Axis Variable:", 
                                       choices = c("anxiety", "depression", "insomnia", "ocd"),
                                       selected = "depression")
                    ),
                    column(6,
                           numericInput("num_clusters", "Number of Clusters:", 
                                        value = 3, min = 2, max = 6)
                    ),
                    column(6,
                           checkboxInput("show_cluster_labels", "Show Cluster Labels", 
                                         value = TRUE)
                    ),
                    width = 4
                ),
                box(title = "Mental Health Profile Clusters", status = "warning", solidHeader = TRUE,
                    plotlyOutput("mental_health_cluster_plot", height = "500px"),
                    width = 8
                )
              ),
              fluidRow(
                box(title = "Selected Cluster Music Profile", status = "success", solidHeader = TRUE,
                    plotlyOutput("selected_cluster_radar", height = "400px"),
                    width = 6
                ),
                box(title = "Selected Cluster Details", status = "info", solidHeader = TRUE,
                    plotlyOutput("selected_cluster_barchart", height = "400px"),
                    width = 6
                )
              ),
              fluidRow(
                box(title = "Cluster Data Table", status = "primary", solidHeader = TRUE,
                    dataTableOutput("cluster_data_table"),
                    width = 12
                )
              )
      )
    )
  )
)
