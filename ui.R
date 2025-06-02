library(shiny)
library(shinydashboard)
library(plotly)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "MxMH"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About & Help", tabName = "about_help", icon = icon("info-circle")),
      menuItem("Data Explorer", tabName = "data_explorer", icon = icon("database")),
      menuItem("Correlation Heatmap", tabName = "correlation_heatmap", icon = icon("chart-line")),
      menuItem("Frequency Analysis", tabName = "frequency_analysis", icon = icon("music")),
      menuItem("Hours vs. Health", tabName = "hours_vs_health", icon = icon("clock")),
      menuItem("Mental Health Clusters", tabName = "mental_health_clusters", icon = icon("brain"))
    ),
    div(
      style = "position: absolute; bottom: 10px; left: 50%; transform: translateX(-50%); text-align: center; width: 90%;",
      tags$img(src = "logo.svg", 
               alt = "Logo", 
               style = "max-width: 100%; max-height: 80px; height: auto;")
    )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "data_explorer",
              h2("Explore the Raw Data"),
              p("This table shows the processed data. You can scroll horizontally to see all columns."),
              dataTableOutput("data")
      ),
      
      tabItem(tabName = "frequency_analysis",
              h2("Mental Health Metrics by Listening Frequency of Specific Genres and Characteristics"),
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
                    column(4, selectInput("filter_while_working_freq", "While Working:", choices = c("All", "Yes", "No"), selected = "All")),
                    column(4, selectInput("filter_instrumentalist_freq", "Instrumentalist:", choices = c("All", "Yes", "No"), selected = "All")),
                    column(4, selectInput("filter_composer_freq", "Composer:", choices = c("All", "Yes", "No"), selected = "All")),
                    column(4, selectInput("filter_exploratory_freq", "Exploratory:", choices = c("All", "Yes", "No"), selected = "All")),
                    column(4, selectInput("filter_foreign_languages_freq", "Foreign Languages:", choices = c("All", "Yes", "No"), selected = "All")),
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
                box(title = "OCD Distribution", status = "danger", solidHeader = TRUE, # Typo in original, assuming it meant Hours per Day vs OCD
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
              h2("Clustering variables to find mental health profiles"),
              fluidRow(
                box(title = "Choose variables to make clusters", status = "primary", solidHeader = TRUE,
                    column(6, 
                           selectInput("cluster_var1", "X-Axis Variable:", 
                                       choices = c("anxiety", "depression", "insomnia", "ocd","hours_per_day", "age", "bpm"),
                                       selected = "hours_per_day")
                    ),
                    column(6, 
                           selectInput("cluster_var2", "Y-Axis Variable:", 
                                       choices = c("anxiety", "depression", "insomnia", "ocd", "hours_per_day", "age", "bpm"),
                                       selected = "depression")
                    ),
                    column(6,
                           numericInput("num_clusters", "Number of Clusters:", 
                                        value = 3, min = 2, max = 6)
                    ),
                    column(6,
                           checkboxInput("show_cluster_labels", "Show Cluster Labels", 
                                         value = FALSE)
                    ),
                    width = 4
                ),
                box(title = "Mental Health Profile Clusters", status = "info", solidHeader = TRUE,
                    plotlyOutput("mental_health_cluster_plot", height = "500px"),
                    p("Click on any point to select a cluster and see detailed analysis below."),
                    width = 8
                )
              ),
              
              fluidRow(
                box(title = "Music Profile", status = "primary", solidHeader = TRUE,
                    plotlyOutput("selected_cluster_radar", height = "600px"),
                    width = 6
                ),
                box(title = "Mental Health Profile", status = "info", solidHeader = TRUE,
                    plotlyOutput("selected_cluster_boxplots", height = "600px"),
                    width = 6
                )
              ),
              fluidRow(
                box(title = "Mental Health Metrics Comparison", status = "success", solidHeader = TRUE,
                    plotlyOutput("cluster_mental_health_comparison", height = "400px"),
                    p("Compare average mental health scores across all clusters. Selected cluster is highlighted if clicked on the main plot."),
                    width = 6
                ),
                box(title = "Age Demographics by Cluster", status = "info", solidHeader = TRUE,
                    plotlyOutput("cluster_demographics", height = "400px"),
                    p("Age group distribution reveals demographic patterns within each mental health cluster. Selected cluster is highlighted."),
                    width = 6
                )
              ),
              fluidRow(
                box(title = "Data Table", status = "info", solidHeader = TRUE,
                    dataTableOutput("cluster_data_table"),
                    width = 12
                )
              )
      ),
      
      # Enhanced UI for the Correlation Heatmap Tab
      tabItem(tabName = "correlation_heatmap",
              h2("Correlation Analysis of Music and Mental Health Variables"),
              
              # Category filtering controls
              fluidRow(
                box(title = "Category Selection", status = "primary", solidHeader = TRUE,
                    checkboxGroupInput("selected_category_groups", 
                                       "Select Variable Categories to Include:",
                                       choices = list(
                                         "Music Genres (Frequency)" = "genres",
                                         "Mental Health Scores" = "mental_health", 
                                         "Music Engagement/Background" = "music_background",
                                         "Music Listening Habits" = "music_characteristics",
                                         "Demographics (Age)" = "demographics"
                                       ),
                                       selected = c("genres", "mental_health", "music_background", 
                                                    "music_characteristics", "demographics"),
                                       inline = TRUE),
                    p("Select which categories of variables to include in the correlation analysis."),
                    width = 12
                )
              ),
              
              # Correlation controls
              fluidRow(
                box(title = "Correlation Controls", status = "info", solidHeader = TRUE,
                    column(6,
                           sliderInput("correlation_threshold", 
                                       "Highlight Correlations Above (Absolute Value):", 
                                       min = 0, max = 1, value = 0.3, step = 0.05)
                    ),
                    column(6,
                           checkboxInput("show_values", "Show Correlation Values on Heatmap", value = TRUE)
                    ),
                    p("Use the slider to highlight correlations above your selected threshold. Higher values indicate stronger relationships between variables."),
                    width = 12
                )
              ),
              
              # Correlation heatmap
              fluidRow(
                box(title = "Correlation Heatmap", status = "info", solidHeader = TRUE,
                    plotlyOutput("correlation_heatmap_plot", height = "700px"),
                    p("This heatmap shows correlations between selected variable categories. Darker/brighter colors indicate stronger correlations. Cells with correlations below the threshold (and not on the diagonal) are muted. Click and drag to zoom, double-click to reset."),
                    width = 12
                )
              ),
              
              # Strong correlations table
              fluidRow(
                box(title = "Strong Correlations Summary Table", status = "warning", solidHeader = TRUE,
                    dataTableOutput("strong_correlations_table"),
                    p("This table lists all variable pairs with absolute correlations at or above your selected threshold."),
                    width = 12
                )
              )
      ), # End of correlation_heatmap tabItem
      
      # New About & Help Tab
      tabItem(tabName = "about_help",
              h2("About This Application & Using Correlations for Guidance"),
              fluidRow(
                box(title = "Welcome!", status = "primary", solidHeader = TRUE, width = 12,
                    p("This application is designed to help you explore potential relationships between music listening habits, music preferences, and self-reported mental health indicators from a survey dataset."),
                    p("One of the key tools for initial exploration is the 'Correlation Heatmap' tab.")
                )
              ),
              fluidRow(
                box(title = "Understanding the Correlation Heatmap Tab", status = "info", solidHeader = TRUE, width = 12,
                    h3("What is a Correlation Heatmap?"),
                    p("The Correlation Heatmap visualizes the statistical relationships (correlations) between different variables in the dataset. It's a powerful way to quickly identify which variables tend to change in a similar or opposite fashion."),
                    
                    h3("How to Interpret Correlations:"),
                    tags$ul(
                      tags$li(HTML("<strong>Correlation Coefficient:</strong> A value between -1 and +1 that measures the strength and direction of a linear relationship between two variables.")),
                      tags$li(HTML("<strong>Positive Correlation (e.g., +0.7):</strong> As one variable increases, the other tends to increase. On this heatmap, these are typically represented by warmer/brighter colors in the positive range (e.g., oranges, reds).")),
                      tags$li(HTML("<strong>Negative Correlation (e.g., -0.7):</strong> As one variable increases, the other tends to decrease. On this heatmap, these are typically represented by cooler/darker colors in the negative range (e.g., blues).")),
                      tags$li(HTML("<strong>Strength:</strong> The closer the absolute value of the coefficient is to 1 (i.e., towards +1 or -1), the stronger the linear relationship. Values closer to 0 indicate a weaker or no linear relationship. More intense colors generally indicate stronger correlations.")),
                      tags$li(HTML("<strong>Important Note:</strong> Correlation does NOT imply causation! It only suggests an association or that two variables tend to co-vary."))
                    ),
                    
                    h3("Using the Heatmap Controls:"),
                    tags$ul(
                      tags$li(HTML("<strong>Category Selection:</strong> Checkboxes allow you to choose which groups of variables (e.g., 'Music Genres', 'Mental Health') you want to include in the heatmap. This helps focus your analysis on specific areas of interest and can make the heatmap less cluttered and easier to interpret.")),
                      tags$li(HTML("<strong>Highlight Correlations Above (Threshold Slider):</strong> This slider allows you to set a minimum absolute correlation value (e.g., 0.3). Any correlation stronger than this threshold (either positive or negative) will be visually highlighted with a more vibrant color on the heatmap. Weaker correlations (below the threshold and not on the main diagonal) will appear muted (grayed out), making significant relationships stand out more clearly.")),
                      tags$li(HTML("<strong>Show Correlation Values on Heatmap:</strong> If checked, the numerical correlation values will be displayed directly on the cells of the heatmap that meet the highlighting threshold, providing precise information."))
                    )
                )
              ),
              fluidRow(
                box(title = "Using Correlations to Guide Your Exploration", status = "success", solidHeader = TRUE, width = 12,
                    p("The heatmap is an excellent starting point for discovering patterns. Here's how to use its insights to guide further investigation in other tabs of this application:"),
                    tags$ul(
                      tags$li(HTML("<strong>1. Identify Strong Relationships:</strong> Look for the brightly colored cells (those meeting your threshold criteria) on the heatmap. These indicate pairs of variables that have a notable linear association.")),
                      tags$li(HTML("<strong>2. Form Hypotheses:</strong> For example, if 'Frequency of Lofi Music' shows a strong positive correlation with 'Anxiety' levels (hypothetically), you might hypothesize that frequent Lofi listeners in this dataset tend to report higher anxiety. Or, if it's negative, that they report lower anxiety.")),
                      tags$li(HTML("<strong>3. Dive Deeper in Other Tabs:</strong> Based on what you observe in the heatmap:")),
                      tags$ul(
                        tags$li(HTML("If a <strong>music genre frequency</strong> (e.g., 'Frequency - Rock') correlates with a <strong>mental health metric</strong> (e.g., 'Depression'):")),
                        tags$ul(
                          tags$li(HTML("Navigate to the <strong>'Frequency Analysis'</strong> tab. Select 'Frequency - Rock' from the 'Choose a Genre Frequency' dropdown. Then, observe the 'Depression Distribution' plot. Does the distribution of depression scores change across different listening frequency levels for Rock music? Compare this to the global average (red line) for all respondents."))
                        ),
                        tags$li(HTML("If <strong>'Hours per Day'</strong> listening to music shows a correlation with a <strong>mental health metric</strong> (e.g., 'Insomnia'):")),
                        tags$ul(
                          tags$li(HTML("Go to the <strong>'Hours vs. Health'</strong> tab. Examine the 'Hours per Day vs. Insomnia' scatter plot. You can further filter this view by specific genre listening frequencies or other respondent characteristics using the dropdowns on that page to see if the relationship holds for specific subgroups."))
                        ),
                        tags$li(HTML("If you see interesting correlations between two numeric variables that can be used for clustering (e.g., 'BPM' and 'Anxiety', or 'Hours per Day' and 'Depression'):")),
                        tags$ul(
                          tags$li(HTML("Visit the <strong>'Mental Health Clusters'</strong> tab. Select these two variables for the X and Y axes of the main cluster plot ('Mental Health Profile Clusters'). Adjust the number of clusters. Do distinct groups of individuals emerge based on these variables? Then, examine the characteristics of these clusters (their music preferences via the radar plot, average mental health scores via boxplots and line graphs, and demographics) using the other plots on that page. Clicking a cluster on the main plot will update the detailed views."))
                        )
                      ),
                      tags$li(HTML("<strong>4. Look for Patterns:</strong> Are certain categories of variables (e.g., all 'Frequency - ...' variables for upbeat genres) consistently correlated with specific mental health outcomes or other behaviors? Do demographic factors like 'Age' correlate with music preferences or reported mental health scores?")),
                      tags$li(HTML("<strong>5. Refine Your Understanding:</strong> The correlation heatmap provides a broad overview of linear associations. The other tabs allow you to dissect these relationships, look at distributions, compare subgroups, and explore multivariate patterns (like in clustering), providing a more nuanced understanding than correlation alone can offer."))
                    ),
                    p(HTML("By using the correlation heatmap to identify potential areas of interest, you can more effectively navigate the other visualization tabs to explore these relationships in more detail. Happy exploring!"))
                )
              )
      )
    )
  )
)