library(shiny)
library(DT)
library(dplyr)
library(janitor)
library(readr)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(rlang)
library(tidyr)
library(viridis) # Added for viridis color scales
library(scales) # Added for number formatting

# --- Load and Preprocess Data ---
# This block runs once when the Shiny application starts.
# It loads the data, cleans column names, removes unnecessary columns,
# recodes streaming service values, creates interval factors for mental health metrics,
# converts frequency columns to ordered factors, converts characteristic columns to factors,
# and filters out rows with missing favorite genre.

# Assuming your data is in a file named "music.csv"
# show_col_types = FALSE suppresses the column specification output in the console.
music_raw <- read_csv("music.csv", show_col_types = FALSE)

# Clean column names to be R-friendly (lowercase, underscores instead of periods/spaces)
music_clean_names <- music_raw %>%
  clean_names()

# Identify columns for creating intervals
interval_cols <- music_clean_names %>%
  select(anxiety, depression, insomnia, ocd)

# Determine the overall minimum and maximum values across the interval columns
# This ensures consistent breaks for the intervals across these metrics.
overall_min <- min(interval_cols, na.rm = TRUE)
overall_max <- max(interval_cols, na.rm = TRUE)

# Define the number of equal intervals to create
num_intervals <- 5

# Create the break points for the intervals
interval_breaks <- seq(overall_min, overall_max, length.out = num_intervals + 1)

# Define the ordered levels for frequency columns
# IMPORTANT: Replace these with the actual unique values and their desired order
# from your frequency columns (e.g., frequency_classical, etc.) if they differ.
frequency_levels <- c("Never", "Rarely", "Sometimes", "Very frequently")

# Define the levels for the characteristic columns (e.g., While Working, Instrumentalist)
# IMPORTANT: Ensure these levels match the actual values in your data (likely "Yes" and "No").
characteristic_levels <- c("No", "Yes")

# Perform the main data processing steps
music_processed <- music_clean_names %>%
  select(-timestamp, -permissions) %>% # Remove timestamp and permissions columns
  mutate(
    # Recode the specific value in primary_streaming_service to "None"
    primary_streaming_service = recode(primary_streaming_service,
                                       "I do not use a streaming service." = "None"),
    
    # Convert mental health columns (Anxiety, Depression, Insomnia, OCD) to interval factors
    # cut() creates factors based on the defined breaks. include.lowest = TRUE includes the minimum value.
    # dig.lab = 4 controls the number of digits in the interval labels.
    anxiety_interval = cut(anxiety, breaks = interval_breaks, include.lowest = TRUE, dig.lab = 4),
    depression_interval = cut(depression, breaks = interval_breaks, include.lowest = TRUE, dig.lab = 4),
    insomnia_interval = cut(insomnia, breaks = interval_breaks, include.lowest = TRUE, dig.lab = 4),
    ocd_interval = cut(ocd, breaks = interval_breaks, include.lowest = TRUE, dig.lab = 4),
    
    # Convert music_effects to a simple factor
    music_effects = factor(music_effects),
    
    # Convert frequency columns (those starting with "frequency_") to ordered factors
    # across() applies the factor conversion to multiple columns.
    across(starts_with("frequency_"), ~factor(., levels = frequency_levels, ordered = TRUE)),
    
    # Convert characteristic columns to factors with defined levels
    while_working = factor(while_working, levels = characteristic_levels),
    instrumentalist = factor(instrumentalist, levels = characteristic_levels),
    composer = factor(composer, levels = characteristic_levels),
    exploratory = factor(exploratory, levels = characteristic_levels),
    foreign_languages = factor(foreign_languages, levels = characteristic_levels)
  ) %>%
  # Filter out rows where fav_genre is missing or empty, as it's used for filtering in one tab.
  filter(!is.na(fav_genre) & fav_genre != "")

# The 'music_processed' data frame is now ready to be used by the server functions.
# --- End Data Preprocessing ---


# Define the server logic for the Shiny application
server <- function(input, output, session) {
  
  # Observe block to update the choices in the 'selected_genre' dropdown (first tab)
  # This runs once when the app starts.
  observe({
    updateSelectInput(session, "selected_genre",
                      choices = unique(music_processed$fav_genre), # Use unique favorite genres from processed data
                      selected = unique(music_processed$fav_genre)[1]) # Select the first unique genre by default
  })
  
  # Observe block to update the choices in the frequency column dropdowns (frequency analysis and scatter plot tabs)
  # This runs once when the app starts.
  observe({
    # Get the column names that start with "frequency_"
    freq_cols <- colnames(music_processed %>% select(starts_with("frequency_")))
    # Create user-friendly labels for the dropdown choices
    freq_col_labels <- gsub("frequency_", "Frequency - ", freq_cols)
    freq_col_labels <- gsub("_", " ", freq_col_labels)
    
    # Update the dropdown for the frequency analysis tab
    updateSelectInput(session, "selected_freq_column",
                      choices = setNames(freq_cols, freq_col_labels), # Use column names as values, user-friendly labels for display
                      selected = freq_cols[1]) # Select the first frequency column by default
    
    # Update the dropdown for the scatter plot tab
    updateSelectInput(session, "selected_freq_column_scatter",
                      choices = setNames(freq_cols, freq_col_labels),
                      selected = freq_cols[1]) # Select the first frequency column by default
  })
  
  # ObserveEvent to update the frequency level dropdown based on the selected frequency column (frequency analysis tab)
  # This runs whenever the selected frequency column changes.
  observeEvent(input$selected_freq_column, {
    req(input$selected_freq_column) # Ensure a frequency column is selected
    
    # Get the levels (e.g., "Never", "Rarely") from the selected frequency column
    freq_levels <- levels(music_processed[[input$selected_freq_column]])
    # Update the choices in the frequency level dropdown
    updateSelectInput(session, "selected_frequency_level",
                      choices = freq_levels,
                      selected = freq_levels[1]) # Select the first level by default
  })
  
  # ObserveEvent to update the frequency level dropdown based on the selected frequency column (scatter plot tab)
  # This runs whenever the selected frequency column in the scatter plot tab changes.
  observeEvent(input$selected_freq_column_scatter, {
    req(input$selected_freq_column_scatter) # Ensure a frequency column is selected
    
    # Get the levels from the selected frequency column
    freq_levels <- levels(music_processed[[input$selected_freq_column_scatter]])
    # Update the choices in the frequency level dropdown for the scatter plot tab
    updateSelectInput(session, "selected_frequency_level_scatter",
                      choices = freq_levels,
                      selected = freq_levels[1]) # Select the first level by default
  })
  
  
  # Reactive expression to filter data based on selected favorite genre (first tab)
  # This reactive depends on input$selected_genre.
  filtered_music_data_genre <- reactive({
    req(input$selected_genre) # Ensure a genre is selected
    
    music_processed %>%
      filter(fav_genre == input$selected_genre)
  })
  
  # Reactive expression to filter data based on selected frequency column AND level (frequency analysis tab)
  # This reactive depends on input$selected_freq_column, input$selected_frequency_level, and the characteristic filter dropdowns for this tab.
  filtered_subset_data_freq <- reactive({
    req(input$selected_freq_column, input$selected_frequency_level) # Ensure both are selected
    
    # Start with the full processed data
    data <- music_processed %>%
      # Filter by the selected frequency column AND level
      filter(!!sym(input$selected_freq_column) == input$selected_frequency_level)
    
    # Apply characteristic filters from the dropdowns for this tab
    filter_while_working_val <- input$filter_while_working_freq
    filter_instrumentalist_val <- input$filter_instrumentalist_freq
    filter_composer_val <- input$filter_composer_freq
    filter_exploratory_val <- input$filter_exploratory_freq
    filter_foreign_languages_val <- input$filter_foreign_languages_freq
    
    # Apply filtering for each characteristic if the selection is not "All"
    if (filter_while_working_val != "All") {
      data <- data %>% filter(while_working == filter_while_working_val)
    }
    if (filter_instrumentalist_val != "All") {
      data <- data %>% filter(instrumentalist == filter_instrumentalist_val)
    }
    if (filter_composer_val != "All") {
      data <- data %>% filter(composer == filter_composer_val)
    }
    if (filter_exploratory_val != "All") {
      data <- data %>% filter(exploratory == filter_exploratory_val)
    }
    if (filter_foreign_languages_val != "All") {
      data <- data %>% filter(foreign_languages == filter_foreign_languages_val)
    }
    
    # Return the filtered data
    data
  })
  
  # Reactive expression to filter data for the scatter plot tab
  # This reactive depends on input$selected_freq_column_scatter, input$selected_frequency_level_scatter, and the characteristic filter dropdowns.
  filtered_subset_data_scatter <- reactive({
    req(input$selected_freq_column_scatter, input$selected_frequency_level_scatter) # Ensure frequency column and level are selected
    
    # Start with the full processed data
    data <- music_processed %>%
      # Filter by the selected frequency column AND level
      filter(!!sym(input$selected_freq_column_scatter) == input$selected_frequency_level_scatter)
    
    # Apply characteristic filters from the dropdowns
    # Get the values from the characteristic filter dropdowns
    filter_while_working_val <- input$filter_while_working
    filter_instrumentalist_val <- input$filter_instrumentalist
    filter_composer_val <- input$filter_composer
    filter_exploratory_val <- input$filter_exploratory
    filter_foreign_languages_val <- input$filter_foreign_languages
    
    # Apply filtering for each characteristic if the selection is not "All"
    if (filter_while_working_val != "All") {
      data <- data %>% filter(while_working == filter_while_working_val)
    }
    if (filter_instrumentalist_val != "All") {
      data <- data %>% filter(instrumentalist == filter_instrumentalist_val)
    }
    if (filter_composer_val != "All") {
      data <- data %>% filter(composer == filter_composer_val)
    }
    if (filter_exploratory_val != "All") {
      data <- data %>% filter(exploratory == filter_exploratory_val)
    }
    if (filter_foreign_languages_val != "All") {
      data <- data %>% filter(foreign_languages == filter_foreign_languages_val)
    }
    
    # Return the filtered data
    data
  })
  
  
  # Reactive expression for the global data line (all records for the selected frequency column)
  # This reactive depends on input$selected_freq_column.
  # This should NOT be filtered by characteristics, as it represents the global comparison.
  global_data_for_freq_plot <- reactive({
    req(input$selected_freq_column) # Ensure a frequency column is selected
    
    music_processed %>%
      filter(!is.na(!!sym(input$selected_freq_column))) # Filter out NA values in the selected frequency column
  })
  
  
  # Function to create a distribution plot with global line comparison (for bar/line plots)
  # This function is used for the "Genre & Mental Health" and "Frequency Analysis" tabs.
  create_distribution_plot <- function(subset_data, global_comparison_data, metric_col, base_title, subset_label, global_label) {
    metric_sym <- enquo(metric_col) # Capture the column name as a quosure
    metric_name <- as_label(metric_sym) # Get the string name of the column
    
    # Ensure the metric column is a factor in the data subsets being plotted
    # This is a safety check; ideally, preprocessing handles this.
    if (!is.factor(subset_data[[metric_name]])) {
      subset_data[[metric_name]] <- factor(subset_data[[metric_name]])
    }
    if (!is.factor(global_comparison_data[[metric_name]])) {
      global_comparison_data[[metric_name]] <- factor(global_comparison_data[[metric_name]])
    }
    
    # Get the levels of the factor from the full processed data to ensure consistency on the x-axis
    metric_levels <- levels(music_processed[[metric_name]])
    
    # Calculate proportions for the subset data (bars)
    subset_plot_data <- subset_data %>%
      count(!!metric_sym, .drop = FALSE) %>% # Count occurrences of each level, keep levels with 0 counts
      filter(!is.na(!!metric_sym)) %>% # Filter out NA values for the metric
      mutate(proportion = n / sum(n)) %>% # Calculate proportion within the subset
      complete(!!metric_sym, fill = list(n = 0, proportion = 0)) %>% # Ensure all levels are present, even with 0 count
      mutate(!!metric_sym := factor(!!metric_sym, levels = metric_levels)) # Ensure the factor levels are in the correct order
    
    
    # Calculate proportions for the global comparison data (line)
    global_plot_data <- global_comparison_data %>%
      count(!!metric_sym, .drop = FALSE) %>% # Count occurrences, keep levels with 0 counts
      filter(!is.na(!!metric_sym)) %>% # Filter out NA values
      mutate(proportion = n / sum(n)) %>% # Calculate proportion within the global comparison data
      complete(!!metric_sym, fill = list(n = 0, proportion = 0)) %>% # Ensure all levels are present
      mutate(!!metric_sym := factor(!!metric_sym, levels = metric_levels)) # Ensure the factor levels are in the correct order
    
    
    # Create the dynamic plot title
    plot_title <- paste(base_title, "for", subset_label, "vs.", global_label)
    
    # Create the ggplot object
    p <- ggplot() +
      # Add bars for the subset data
      geom_col(data = subset_plot_data, aes(x = !!metric_sym, y = proportion, fill = !!metric_sym),
               width = 0.7) + # Bar width
      # Add a line for the global comparison data
      geom_line(data = global_plot_data, aes(x = !!metric_sym, y = proportion, group = 1),
                color = "red", size = 1) + # Global line color and size
      # Add points on the global line
      geom_point(data = global_plot_data, aes(x = !!metric_sym, y = proportion, group = 1),
                 color = "red", size = 2) + # Points color and size
      theme_minimal() + # Minimal theme
      labs(title = plot_title, # Plot title
           x = metric_name, # X-axis label
           y = "Proportion of Respondents", # Y-axis label
           fill = metric_name) + # Legend title for bar colors
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels
      scale_fill_brewer(palette = "Set2") + # Color palette for bars
      scale_y_continuous(limits = c(0, 1)) # Ensure y-axis goes from 0 to 1 for proportions
    
    # Convert the ggplot object to an interactive plotly object
    ggplotly(p, tooltip = c("x", "y")) # Tooltip shows x-value (level) and y-value (proportion)
  }
  
  # Function to create a scatter plot (Hours per Day vs. Mental Health)
  # This function is used for the "Hours vs. Health" tab.
  # It colors points by Age.
  # Handles both numerical and factor y-axis variables.
  # Uses a continuous color scale for Age with numeric labels.
  create_scatter_plot <- function(data, metric_col, base_title, subset_label) {
    metric_sym <- enquo(metric_col) # Capture the mental health metric column
    metric_name <- as_label(metric_sym) # Get the string name of the mental health metric column
    
    # Ensure the metric column is a factor for mapping to y-axis categories if it's not numeric
    if (!is.numeric(data[[metric_name]]) && !is.factor(data[[metric_name]])) {
      warning(paste("Metric column", metric_name, "is not numeric or factor. Attempting factor conversion."))
      data[[metric_name]] <- factor(data[[metric_name]])
    }
    
    plot_title <- paste(base_title, "for", subset_label)
    
    # Check if the metric column is numeric or a factor
    if (is.numeric(data[[metric_name]])) {
      # If numeric, plot directly on the y-axis
      p <- ggplot(data, aes(x = hours_per_day, y = !!metric_sym, color = age)) + # Color by Age
        geom_point(alpha = 0.6) + # No jitter needed for continuous y
        theme_minimal() +
        labs(title = plot_title,
             x = "Hours per Day",
             y = metric_name, # Y-axis label is the metric name
             color = "Age") + # Legend title is Age
        # Use a continuous color scale for Age with numeric labels
        scale_color_viridis_c(option = "plasma", labels = scales::number_format())
    } else if (is.factor(data[[metric_name]])) {
      # If factor, use jitter on the y-axis
      p <- ggplot(data, aes(x = hours_per_day, y = !!metric_sym, color = age)) + # Color by Age
        geom_point(alpha = 0.6, position = position_jitter(height = 0.1)) + # Jitter for factor y
        theme_minimal() +
        labs(title = plot_title,
             x = "Hours per Day",
             y = metric_name, # Y-axis label is the metric name
             color = "Age") + # Legend title is Age
        # Use a continuous color scale for Age with numeric labels
        scale_color_viridis_c(option = "plasma", labels = scales::number_format())
    } else {
      # Handle unexpected data types for the metric column
      warning(paste("Metric column", metric_name, "is not numeric or factor. Cannot plot."))
      return(NULL) # Return NULL if plotting is not possible
    }
    
    
    # Convert the ggplot object to an interactive plotly object
    # Include 'age' in the tooltip to show the exact Age value on hover
    ggplotly(p, tooltip = c("x", "y", "color", "age")) # Tooltip shows hours_per_day, mental health category/value, color mapping, and Age
  }
  
  
  # --- Output Definitions ---
  
  # Output for the test text (on the Overview Dashboard tab)
  output$testText <- renderText({
    paste("Boredom level ",
          input$testRange[1], " - ",
          input$testRange[2], "%")
  })
  
  # Output for the data table (on the Data Explorer tab)
  output$data <- renderDataTable({
    datatable(music_processed, options = list(scrollX = TRUE)) # Display the processed data table with horizontal scrolling
  })
  
  # Outputs for the "Genre & Mental Health" tab (bar/line plots)
  output$plot_anxiety_dist <- renderPlotly({
    create_distribution_plot(filtered_music_data_genre(), music_processed, anxiety_interval, "Anxiety Distribution", input$selected_genre, "Global")
  })
  
  output$plot_depression_dist <- renderPlotly({
    create_distribution_plot(filtered_music_data_genre(), music_processed, depression_interval, "Depression Distribution", input$selected_genre, "Global")
  })
  
  output$plot_insomnia_dist <- renderPlotly({
    create_distribution_plot(filtered_music_data_genre(), music_processed, insomnia_interval, "Insomnia Distribution", input$selected_genre, "Global")
  })
  
  output$plot_ocd_dist <- renderPlotly({
    create_distribution_plot(filtered_music_data_genre(), music_processed, ocd_interval, "OCD Distribution", input$selected_genre, "Global")
  })
  
  output$plot_music_effects_dist <- renderPlotly({
    create_distribution_plot(filtered_music_data_genre(), music_processed, music_effects, "Music Effects Distribution", input$selected_genre, "Global")
  })
  
  # Outputs for the "Frequency Analysis" tab (bar/line plots)
  output$plot_anxiety_freq_dist <- renderPlotly({
    # Construct subset label including frequency and characteristics for this tab
    subset_label_parts <- c(gsub("frequency_", "", input$selected_freq_column), input$selected_frequency_level)
    char_filters <- c(input$filter_while_working_freq, input$filter_instrumentalist_freq, input$filter_composer_freq, input$filter_exploratory_freq, input$filter_foreign_languages_freq)
    char_filter_labels <- c("Working", "Instrumentalist", "Composer", "Exploratory", "Foreign Lang.")
    selected_chars_labels <- char_filter_labels[char_filters != "All"]
    selected_chars_values <- char_filters[char_filters != "All"]
    
    if (length(selected_chars_labels) > 0) {
      char_summary <- paste(paste0(selected_chars_labels, ": ", selected_chars_values), collapse = ", ")
      subset_label_parts <- c(subset_label_parts, paste0("(", char_summary, ")"))
    }
    
    subset_label <- paste(subset_label_parts, collapse = " - ")
    
    global_label <- paste0(gsub("frequency_", "", input$selected_freq_column), " - Global") # Global label remains based only on frequency column
    create_distribution_plot(filtered_subset_data_freq(), global_data_for_freq_plot(), anxiety_interval, "Anxiety Distribution", subset_label, global_label)
  })
  
  output$plot_depression_freq_dist <- renderPlotly({
    # Construct subset label including frequency and characteristics for this tab
    subset_label_parts <- c(gsub("frequency_", "", input$selected_freq_column), input$selected_frequency_level)
    char_filters <- c(input$filter_while_working_freq, input$filter_instrumentalist_freq, input$filter_composer_freq, input$filter_exploratory_freq, input$filter_foreign_languages_freq)
    char_filter_labels <- c("Working", "Instrumentalist", "Composer", "Exploratory", "Foreign Lang.")
    selected_chars_labels <- char_filter_labels[char_filters != "All"]
    selected_chars_values <- char_filters[char_filters != "All"]
    
    if (length(selected_chars_labels) > 0) {
      char_summary <- paste(paste0(selected_chars_labels, ": ", selected_chars_values), collapse = ", ")
      subset_label_parts <- c(subset_label_parts, paste0("(", char_summary, ")"))
    }
    
    subset_label <- paste(subset_label_parts, collapse = " - ")
    
    global_label <- paste0(gsub("frequency_", "", input$selected_freq_column), " - Global")
    create_distribution_plot(filtered_subset_data_freq(), global_data_for_freq_plot(), depression_interval, "Depression Distribution", subset_label, global_label)
  })
  
  output$plot_insomnia_freq_dist <- renderPlotly({
    # Construct subset label including frequency and characteristics for this tab
    subset_label_parts <- c(gsub("frequency_", "", input$selected_freq_column), input$selected_frequency_level)
    char_filters <- c(input$filter_while_working_freq, input$filter_instrumentalist_freq, input$filter_composer_freq, input$filter_exploratory_freq, input$filter_foreign_languages_freq)
    char_filter_labels <- c("Working", "Instrumentalist", "Composer", "Exploratory", "Foreign Lang.")
    selected_chars_labels <- char_filter_labels[char_filters != "All"]
    selected_chars_values <- char_filters[char_filters != "All"]
    
    if (length(selected_chars_labels) > 0) {
      char_summary <- paste(paste0(selected_chars_labels, ": ", selected_chars_values), collapse = ", ")
      subset_label_parts <- c(subset_label_parts, paste0("(", char_summary, ")"))
    }
    
    subset_label <- paste(subset_label_parts, collapse = " - ")
    
    global_label <- paste0(gsub("frequency_", "", input$selected_freq_column), " - Global")
    create_distribution_plot(filtered_subset_data_freq(), global_data_for_freq_plot(), insomnia_interval, "Insomnia Distribution", subset_label, global_label)
  })
  
  output$plot_ocd_freq_dist <- renderPlotly({
    # Construct subset label including frequency and characteristics for this tab
    subset_label_parts <- c(gsub("frequency_", "", input$selected_freq_column), input$selected_frequency_level)
    char_filters <- c(input$filter_while_working_freq, input$filter_instrumentalist_freq, input$filter_composer_freq, input$filter_exploratory_freq, input$filter_foreign_languages_freq)
    char_filter_labels <- c("Working", "Instrumentalist", "Composer", "Exploratory", "Foreign Lang.")
    selected_chars_labels <- char_filter_labels[char_filters != "All"]
    selected_chars_values <- char_filters[char_filters != "All"]
    
    if (length(selected_chars_labels) > 0) {
      char_summary <- paste(paste0(selected_chars_labels, ": ", selected_chars_values), collapse = ", ")
      subset_label_parts <- c(subset_label_parts, paste0("(", char_summary, ")"))
    }
    
    subset_label <- paste(subset_label_parts, collapse = " - ")
    
    global_label <- paste0(gsub("frequency_", "", input$selected_freq_column), " - Global")
    create_distribution_plot(filtered_subset_data_freq(), global_data_for_freq_plot(), ocd_interval, "OCD Distribution", subset_label, global_label)
  })
  
  output$plot_music_effects_freq_dist <- renderPlotly({
    # Construct subset label including frequency and characteristics for this tab
    subset_label_parts <- c(gsub("frequency_", "", input$selected_freq_column), input$selected_frequency_level)
    char_filters <- c(input$filter_while_working_freq, input$filter_instrumentalist_freq, input$filter_composer_freq, input$filter_exploratory_freq, input$filter_foreign_languages_freq)
    char_filter_labels <- c("Working", "Instrumentalist", "Composer", "Exploratory", "Foreign Lang.")
    selected_chars_labels <- char_filter_labels[char_filters != "All"]
    selected_chars_values <- char_filters[char_filters != "All"]
    
    if (length(selected_chars_labels) > 0) {
      char_summary <- paste(paste0(selected_chars_labels, ": ", selected_chars_values), collapse = ", ")
      subset_label_parts <- c(subset_label_parts, paste0("(", char_summary, ")"))
    }
    
    subset_label <- paste(subset_label_parts, collapse = " - ")
    
    global_label <- paste0(gsub("frequency_", "", input$selected_freq_column), " - Global")
    create_distribution_plot(filtered_subset_data_freq(), global_data_for_freq_plot(), music_effects, "Music Effects Distribution", subset_label, global_label)
  })
  
  # Outputs for the "Hours vs. Health" tab (scatter plots)
  output$plot_hours_anxiety_scatter <- renderPlotly({
    # Construct subset label based on selected frequency and characteristics
    subset_label_parts <- c(gsub("frequency_", "", input$selected_freq_column_scatter), input$selected_frequency_level_scatter)
    char_filters <- c(input$filter_while_working, input$filter_instrumentalist, input$filter_composer, input$filter_exploratory, input$filter_foreign_languages)
    char_filter_labels <- c("Working", "Instrumentalist", "Composer", "Exploratory", "Foreign Lang.")
    selected_chars_labels <- char_filter_labels[char_filters != "All"]
    selected_chars_values <- char_filters[char_filters != "All"]
    
    if (length(selected_chars_labels) > 0) {
      char_summary <- paste(paste0(selected_chars_labels, ": ", selected_chars_values), collapse = ", ")
      subset_label_parts <- c(subset_label_parts, paste0("(", char_summary, ")"))
    }
    
    subset_label <- paste(subset_label_parts, collapse = " - ")
    create_scatter_plot(filtered_subset_data_scatter(), anxiety, "Hours per Day vs. Anxiety", subset_label)
  })
  
  output$plot_hours_depression_scatter <- renderPlotly({
    subset_label_parts <- c(gsub("frequency_", "", input$selected_freq_column_scatter), input$selected_frequency_level_scatter)
    char_filters <- c(input$filter_while_working, input$filter_instrumentalist, input$filter_composer, input$filter_exploratory, input$filter_foreign_languages)
    char_filter_labels <- c("Working", "Instrumentalist", "Composer", "Exploratory", "Foreign Lang.")
    selected_chars_labels <- char_filter_labels[char_filters != "All"]
    selected_chars_values <- char_filters[char_filters != "All"]
    
    if (length(selected_chars_labels) > 0) {
      char_summary <- paste(paste0(selected_chars_labels, ": ", selected_chars_values), collapse = ", ")
      subset_label_parts <- c(subset_label_parts, paste0("(", char_summary, ")"))
    }
    
    subset_label <- paste(subset_label_parts, collapse = " - ")
    create_scatter_plot(filtered_subset_data_scatter(), depression, "Hours per Day vs. Depression", subset_label)
  })
  
  output$plot_hours_insomnia_scatter <- renderPlotly({
    subset_label_parts <- c(gsub("frequency_", "", input$selected_freq_column_scatter), input$selected_frequency_level_scatter)
    char_filters <- c(input$filter_while_working, input$filter_instrumentalist, input$filter_composer, input$filter_exploratory, input$filter_foreign_languages)
    char_filter_labels <- c("Working", "Instrumentalist", "Composer", "Exploratory", "Foreign Lang.")
    selected_chars_labels <- char_filter_labels[char_filters != "All"]
    selected_chars_values <- char_filters[char_filters != "All"]
    
    if (length(selected_chars_labels) > 0) {
      char_summary <- paste(paste0(selected_chars_labels, ": ", selected_chars_values), collapse = ", ")
      subset_label_parts <- c(subset_label_parts, paste0("(", char_summary, ")"))
    }
    
    subset_label <- paste(subset_label_parts, collapse = " - ")
    create_scatter_plot(filtered_subset_data_scatter(), insomnia, "Hours per Day vs. Insomnia", subset_label)
  })
  
  output$plot_hours_ocd_scatter <- renderPlotly({
    subset_label_parts <- c(gsub("frequency_", "", input$selected_freq_column_scatter), input$selected_frequency_level_scatter)
    char_filters <- c(input$filter_while_working, input$filter_instrumentalist, input$filter_composer, input$filter_exploratory, input$filter_foreign_languages)
    char_filter_labels <- c("Working", "Instrumentalist", "Composer", "Exploratory", "Foreign Lang.")
    selected_chars_labels <- char_filter_labels[char_filters != "All"]
    selected_chars_values <- char_filters[char_filters != "All"]
    
    if (length(selected_chars_labels) > 0) {
      char_summary <- paste(paste0(selected_chars_labels, ": ", selected_chars_values), collapse = ", ")
      subset_label_parts <- c(subset_label_parts, paste0("(", char_summary, ")"))
    }
    
    subset_label <- paste(subset_label_parts, collapse = " - ")
    create_scatter_plot(filtered_subset_data_scatter(), ocd, "Hours per Day vs. OCD", subset_label)
  })
  
  output$plot_hours_music_effects_scatter <- renderPlotly({
    subset_label_parts <- c(gsub("frequency_", "", input$selected_freq_column_scatter), input$selected_frequency_level_scatter)
    char_filters <- c(input$filter_while_working, input$filter_instrumentalist, input$filter_composer, input$filter_exploratory, input$filter_foreign_languages)
    char_filter_labels <- c("Working", "Instrumentalist", "Composer", "Exploratory", "Foreign Lang.")
    selected_chars_labels <- char_filter_labels[char_filters != "All"]
    selected_chars_values <- char_filters[char_filters != "All"]
    
    if (length(selected_chars_labels) > 0) {
      char_summary <- paste(paste0(selected_chars_labels, ": ", selected_chars_values), collapse = ", ")
      subset_label_parts <- c(subset_label_parts, paste0("(", char_summary, ")"))
    }
    
    subset_label <- paste(subset_label_parts, collapse = " - ")
    create_scatter_plot(filtered_subset_data_scatter(), music_effects, "Hours per Day vs. Music Effects", subset_label)
  })
  # Updated Mental Health Clusters section in the server.R file
  # Only showing the relevant parts for the Mental Health Clusters tab
  
  # Add this to your existing server function
  
  # Reactive expression for cluster data
  cluster_data <- reactive({
    req(input$cluster_var1, input$cluster_var2, input$num_clusters)
    
    # Select the variables for clustering
    cluster_vars <- c("anxiety", "depression", "insomnia", "ocd")
    
    # Create data frame for clustering with only complete cases
    data_for_clustering <- music_processed %>%
      select(all_of(cluster_vars), 
             hours_per_day, 
             starts_with("frequency_"), 
             music_effects, 
             fav_genre) %>%
      na.omit()
    
    # Perform k-means clustering on the mental health variables only
    set.seed(123) # For reproducibility
    kmeans_result <- kmeans(data_for_clustering[, cluster_vars], 
                            centers = input$num_clusters)
    
    # Add cluster assignment to the data
    data_for_clustering$cluster <- factor(kmeans_result$cluster)
    
    # Return the clustered data
    return(data_for_clustering)
  })
  
  # Selected cluster data
  selected_cluster_data <- reactive({
    # Get the event data from clicking on the cluster plot
    event_data <- event_data("plotly_click", source = "cluster_plot")
    
    # If no point is selected, return NULL
    if(is.null(event_data)) {
      return(NULL)
    }
    
    # Get the cluster number from the point's customdata
    selected_cluster <- event_data$customdata[1]
    
    # Filter the cluster data for the selected cluster
    cluster_data() %>%
      filter(cluster == selected_cluster)
  })
  
  # Display cluster or "All Data" label for titles
  cluster_label <- reactive({
    selected_data <- selected_cluster_data()
    if(is.null(selected_data)) {
      return("All Data")
    } else {
      return(paste("Cluster", unique(selected_data$cluster)))
    }
  })
  
  # Cluster plot
  output$mental_health_cluster_plot <- renderPlotly({
    req(cluster_data(), input$cluster_var1, input$cluster_var2)
    
    data <- cluster_data()
    
    # Create the scatter plot
    p <- plot_ly(data, 
                 x = ~get(input$cluster_var1),
                 y = ~get(input$cluster_var2),
                 color = ~cluster,
                 customdata = ~cluster,
                 type = "scatter",
                 mode = "markers",
                 marker = list(size = 10, opacity = 0.7),
                 source = "cluster_plot") %>%
      layout(title = "Mental Health Profile Clusters",
             xaxis = list(title = input$cluster_var1),
             yaxis = list(title = input$cluster_var2),
             showlegend = TRUE)
    
    # Add cluster centroids if requested
    if(input$show_cluster_labels) {
      # Calculate cluster centroids for the plotted variables
      centroids <- data %>%
        group_by(cluster) %>%
        summarize(x = mean(get(input$cluster_var1)),
                  y = mean(get(input$cluster_var2)))
      
      # Add annotations for centroids
      p <- p %>% add_annotations(
        data = centroids,
        x = ~x,
        y = ~y,
        text = paste("Cluster", centroids$cluster),
        showarrow = TRUE,
        arrowhead = 1,
        arrowsize = 1,
        arrowwidth = 2,
        arrowcolor = "black",
        font = list(size = 12)
      )
    }
    
    return(p)
  })
  
  # IMPROVED: Radar chart for selected cluster music preferences with proper normalization
  output$selected_cluster_radar <- renderPlotly({
    req(cluster_data())
    
    # If no cluster is selected, show the overall average
    selected_data <- selected_cluster_data()
    all_data <- cluster_data()
    
    if(is.null(selected_data)) {
      # Use all data if no cluster is selected
      selected_data <- all_data
    }
    
    # Get the frequency columns
    frequency_cols <- colnames(selected_data)[grep("^frequency_", colnames(selected_data))]
    
    # Calculate the NORMALIZED frequency for each genre
    # First, convert categories to numeric: Never = 1, Rarely = 2, Sometimes = 3, Very frequently = 4
    
    # Process for selected cluster/all data
    radar_data_selected <- selected_data %>%
      mutate(across(all_of(frequency_cols), 
                    ~as.numeric(factor(., levels = c("Never", "Rarely", "Sometimes", "Very frequently"))))) %>%
      summarize(across(all_of(frequency_cols), mean, na.rm = TRUE))
    
    # Process for all data (for comparison)
    radar_data_all <- all_data %>%
      mutate(across(all_of(frequency_cols), 
                    ~as.numeric(factor(., levels = c("Never", "Rarely", "Sometimes", "Very frequently"))))) %>%
      summarize(across(all_of(frequency_cols), mean, na.rm = TRUE))
    
    # Reshape for radar chart - selected data
    radar_long_selected <- radar_data_selected %>%
      tidyr::pivot_longer(cols = everything(), 
                          names_to = "Genre", 
                          values_to = "Frequency") %>%
      mutate(Genre = gsub("frequency_", "", Genre),
             Genre = gsub("_", " ", Genre),
             Dataset = cluster_label())
    
    # Reshape for radar chart - all data (only if showing selected cluster)
    radar_long_all <- radar_data_all %>%
      tidyr::pivot_longer(cols = everything(), 
                          names_to = "Genre", 
                          values_to = "Frequency") %>%
      mutate(Genre = gsub("frequency_", "", Genre),
             Genre = gsub("_", " ", Genre),
             Dataset = "All Data")
    
    # Combine datasets if showing a selected cluster
    if(!is.null(selected_cluster_data())) {
      radar_long <- bind_rows(radar_long_selected, radar_long_all)
    } else {
      radar_long <- radar_long_selected
    }
    
    # Create radar chart
    plot_ly() %>%
      add_trace(
        data = radar_long,
        r = ~Frequency,
        theta = ~Genre,
        color = ~Dataset,
        fill = 'toself',
        type = 'scatterpolar',
        line = list(width = 2)
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(1, 4),
            tickvals = c(1, 2, 3, 4),
            ticktext = c("Never", "Rarely", "Sometimes", "Very frequently")
          )
        ),
        title = paste("Music Genre Preferences -", cluster_label()),
        showlegend = TRUE
      )
  })
  
  # Only showing relevant parts related to the Mental Health Clusters tab changes
  # The improved sections will replace the existing mental health cluster visualizations
  
  # IMPROVED: Replaced with side-by-side boxplots for all mental health metrics for selected cluster
  output$selected_cluster_barchart <- renderPlotly({
    req(cluster_data())
    
    # Get all clusters data
    all_data <- cluster_data()
    
    # Get selected cluster if any
    selected_data <- selected_cluster_data()
    
    # Determine which data to use and prepare a title
    if(is.null(selected_data)) {
      # If no cluster selected, use all data but make it clear 
      plot_data <- all_data
      plot_title <- "Mental Health Metrics - All Data (Select a cluster from the plot)"
    } else {
      # Use only the selected cluster data
      plot_data <- selected_data
      plot_title <- paste("Mental Health Metrics - Cluster", unique(selected_data$cluster))
    }
    
    # Extract mental health metrics
    mental_health_cols <- c("anxiety", "depression", "insomnia", "ocd")
    
    # Reshape data for boxplot - longer format for easier plotting
    boxplot_data <- plot_data %>%
      select(all_of(mental_health_cols)) %>%
      tidyr::pivot_longer(cols = everything(), 
                          names_to = "Metric", 
                          values_to = "Value") %>%
      mutate(Metric = factor(Metric, levels = mental_health_cols))
    
    # Define custom colors for each metric
    metric_colors <- c("anxiety" = "#FF7F50", "depression" = "#6495ED", 
                       "insomnia" = "#32CD32", "ocd" = "#FFD700")
    
    # Create boxplots showing all four metrics side by side
    p <- plot_ly() %>%
      add_boxplot(
        data = boxplot_data,
        x = ~Metric,
        y = ~Value,
        color = ~Metric,
        colors = metric_colors,
        boxpoints = "all",
        jitter = 0.3,
        pointpos = 0,
        marker = list(opacity = 0.7),
        showlegend = FALSE
      ) %>%
      layout(
        title = plot_title,
        xaxis = list(title = "", 
                     categoryorder = "array",
                     categoryarray = mental_health_cols,
                     ticktext = c("Anxiety", "Depression", "Insomnia", "OCD"),
                     tickvals = mental_health_cols),
        yaxis = list(title = "Score (0-10)",
                     range = c(0, 10))
      )
    
    return(p)
  })
  
  # IMPROVED: Radar chart with better visibility between selected cluster and global data
  output$selected_cluster_radar <- renderPlotly({
    req(cluster_data())
    
    # Get all clusters data
    all_data <- cluster_data()
    
    # Get selected cluster if any
    selected_data <- selected_cluster_data()
    
    # Determine if we're showing a specific cluster or all data
    if(is.null(selected_data)) {
      # If no cluster selected, just show all data
      plot_title <- "Music Genre Preferences - All Data"
      show_comparison <- FALSE
      selected_data <- all_data  # Use all data as the "selected" data
    } else {
      # If a cluster is selected, prepare both datasets
      plot_title <- paste("Music Genre Preferences - Cluster", unique(selected_data$cluster), "vs All Data")
      show_comparison <- TRUE
    }
    
    # Get the frequency columns
    frequency_cols <- colnames(all_data)[grep("^frequency_", colnames(all_data))]
    
    # Process for selected cluster/all data
    radar_data_selected <- selected_data %>%
      mutate(across(all_of(frequency_cols), 
                    ~as.numeric(factor(., levels = c("Never", "Rarely", "Sometimes", "Very frequently"))))) %>%
      summarize(across(all_of(frequency_cols), mean, na.rm = TRUE))
    
    # Process for all data (for comparison)
    radar_data_all <- all_data %>%
      mutate(across(all_of(frequency_cols), 
                    ~as.numeric(factor(., levels = c("Never", "Rarely", "Sometimes", "Very frequently"))))) %>%
      summarize(across(all_of(frequency_cols), mean, na.rm = TRUE))
    
    # Reshape for radar chart - selected data
    radar_long_selected <- radar_data_selected %>%
      tidyr::pivot_longer(cols = everything(), 
                          names_to = "Genre", 
                          values_to = "Frequency") %>%
      mutate(Genre = gsub("frequency_", "", Genre),
             Genre = gsub("_", " ", Genre),
             Dataset = if(is.null(selected_cluster_data())) "All Data" else paste("Cluster", unique(selected_data$cluster)))
    
    # Create radar chart
    p <- plot_ly()
    
    # If showing comparison, add global data with reduced opacity
    if(show_comparison) {
      # Reshape for radar chart - all data (only if showing selected cluster)
      radar_long_all <- radar_data_all %>%
        tidyr::pivot_longer(cols = everything(), 
                            names_to = "Genre", 
                            values_to = "Frequency") %>%
        mutate(Genre = gsub("frequency_", "", Genre),
               Genre = gsub("_", " ", Genre),
               Dataset = "All Data")
      
      # Add all data trace with reduced opacity
      p <- p %>% add_trace(
        data = radar_long_all,
        r = ~Frequency,
        theta = ~Genre,
        name = "All Data",
        fill = 'toself',
        type = 'scatterpolar',
        fillcolor = 'rgba(200, 200, 200, 0.2)',  # Very light gray fill
        line = list(color = 'rgba(150, 150, 150, 0.7)', width = 1, dash = 'dot') # Light gray dotted line
      )
    }
    
    # Add selected cluster trace with vibrant color
    p <- p %>% add_trace(
      data = radar_long_selected,
      r = ~Frequency,
      theta = ~Genre,
      name = unique(radar_long_selected$Dataset),
      fill = 'toself',
      type = 'scatterpolar',
      fillcolor = 'rgba(255, 100, 50, 0.6)',  # Vibrant orange fill with transparency
      line = list(color = 'rgb(255, 100, 50)', width = 2.5)  # Solid thick orange line
    )
    
    # Layout
    p <- p %>% layout(
      polar = list(
        radialaxis = list(
          visible = TRUE,
          range = c(1, 4),
          tickvals = c(1, 2, 3, 4),
          ticktext = c("Never", "Rarely", "Sometimes", "Very frequently")
        )
      ),
      title = plot_title,
      legend = list(x = 0.5, y = 1.1, orientation = 'h')
    )
    
    return(p)
  })
  # Data table for the selected cluster or all data
  output$cluster_data_table <- renderDataTable({
    # If no cluster is selected, show all clustered data
    data <- selected_cluster_data()
    if(is.null(data)) {
      data <- cluster_data()
    }
    
    # Clean data for display
    display_data <- data %>%
      select(cluster, anxiety, depression, insomnia, ocd, hours_per_day, fav_genre, music_effects)
    
    # Create datatable
    datatable(display_data, 
              options = list(pageLength = 5, 
                             scrollX = TRUE),
              caption = if(is.null(selected_cluster_data())) {
                "All Respondents (Click on a cluster in the plot above to filter)"
              } else {
                paste("Respondents in Cluster", unique(data$cluster))
              })
  })
} # End of shinyServer function
