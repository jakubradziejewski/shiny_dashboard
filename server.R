library(shiny)
library(DT)
library(dplyr)
library(janitor)
library(readr)
library(ggplot2)
library(plotly)
library(rlang)
library(tidyr)
library(viridis)
library(scales)
library(cluster)
library(stringr)  
music_raw <- read_csv("music.csv", show_col_types = FALSE)

music_clean_names <- music_raw %>%
  clean_names()

bpm_median <- median(music_clean_names$bpm[music_clean_names$bpm <= 200], na.rm = TRUE)

music_clean_names <- music_clean_names %>%
  mutate(bpm = ifelse(is.na(bpm) | bpm > 200, bpm_median, bpm)) %>%
  filter(hours_per_day < 18)
interval_cols <- music_clean_names %>%
  select(anxiety, depression, insomnia, ocd)

overall_min <- min(interval_cols, na.rm = TRUE)
overall_max <- max(interval_cols, na.rm = TRUE)

num_intervals <- 5

interval_breaks <- seq(overall_min, overall_max, length.out = num_intervals + 1)

frequency_levels <- c("Never", "Rarely", "Sometimes", "Very frequently")

characteristic_levels <- c("No", "Yes")

music_processed <- music_clean_names %>%
  select(-timestamp, -permissions) %>%
  mutate(
    primary_streaming_service = recode(primary_streaming_service,
                                       "I do not use a streaming service." = "None"),
    
    anxiety_interval = cut(anxiety, breaks = interval_breaks, include.lowest = TRUE, dig.lab = 4),
    depression_interval = cut(depression, breaks = interval_breaks, include.lowest = TRUE, dig.lab = 4),
    insomnia_interval = cut(insomnia, breaks = interval_breaks, include.lowest = TRUE, dig.lab = 4),
    ocd_interval = cut(ocd, breaks = interval_breaks, include.lowest = TRUE, dig.lab = 4),
    
    music_effects = factor(music_effects),
    
    across(starts_with("frequency_"), ~factor(., levels = frequency_levels, ordered = TRUE)),
    
    while_working = factor(while_working, levels = characteristic_levels),
    instrumentalist = factor(instrumentalist, levels = characteristic_levels),
    composer = factor(composer, levels = characteristic_levels),
    exploratory = factor(exploratory, levels = characteristic_levels),
    foreign_languages = factor(foreign_languages, levels = characteristic_levels)
  ) %>%
  filter(!is.na(fav_genre) & fav_genre != "")

server <- function(input, output, session) {
  
  observe({
    freq_cols <- colnames(music_processed %>% select(starts_with("frequency_")))
    freq_col_labels <- gsub("frequency_", "Frequency - ", freq_cols)
    freq_col_labels <- gsub("_", " ", freq_col_labels)
    
    updateSelectInput(session, "selected_freq_column",
                      choices = setNames(freq_cols, freq_col_labels),
                      selected = freq_cols[1])
    
    updateSelectInput(session, "selected_freq_column_scatter",
                      choices = setNames(freq_cols, freq_col_labels),
                      selected = freq_cols[1])
  })
  
  observeEvent(input$selected_freq_column, {
    req(input$selected_freq_column)
    
    freq_levels <- levels(music_processed[[input$selected_freq_column]])
    updateSelectInput(session, "selected_frequency_level",
                      choices = freq_levels,
                      selected = freq_levels[1])
  })
  
  observeEvent(input$selected_freq_column_scatter, {
    req(input$selected_freq_column_scatter)
    
    freq_levels <- levels(music_processed[[input$selected_freq_column_scatter]])
    updateSelectInput(session, "selected_frequency_level_scatter",
                      choices = freq_levels,
                      selected = freq_levels[1])
  })
  
  filtered_subset_data_freq <- reactive({
    req(input$selected_freq_column, input$selected_frequency_level)
    
    data <- music_processed %>%
      filter(!!sym(input$selected_freq_column) == input$selected_frequency_level)
    
    filter_while_working_val <- input$filter_while_working_freq
    filter_instrumentalist_val <- input$filter_instrumentalist_freq
    filter_composer_val <- input$filter_composer_freq
    filter_exploratory_val <- input$filter_exploratory_freq
    filter_foreign_languages_val <- input$filter_foreign_languages_freq
    
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
    
    data
  })
  
  filtered_subset_data_scatter <- reactive({
    req(input$selected_freq_column_scatter, input$selected_frequency_level_scatter)
    
    data <- music_processed %>%
      filter(!!sym(input$selected_freq_column_scatter) == input$selected_frequency_level_scatter)
    
    filter_while_working_val <- input$filter_while_working
    filter_instrumentalist_val <- input$filter_instrumentalist
    filter_composer_val <- input$filter_composer
    filter_exploratory_val <- input$filter_exploratory
    filter_foreign_languages_val <- input$filter_foreign_languages
    
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
    
    data
  })
  
  global_data_for_freq_plot <- reactive({
    req(input$selected_freq_column)
    
    music_processed %>%
      filter(!is.na(!!sym(input$selected_freq_column)))
  })
  
  create_distribution_plot <- function(subset_data, global_comparison_data, metric_col, base_title, subset_label, global_label) {
    metric_sym <- enquo(metric_col)
    metric_name <- as_label(metric_sym)
    
    if (!is.factor(subset_data[[metric_name]])) {
      subset_data[[metric_name]] <- factor(subset_data[[metric_name]])
    }
    if (!is.factor(global_comparison_data[[metric_name]])) {
      global_comparison_data[[metric_name]] <- factor(global_comparison_data[[metric_name]])
    }
    
    metric_levels <- levels(music_processed[[metric_name]])
    
    subset_plot_data <- subset_data %>%
      count(!!metric_sym, .drop = FALSE) %>%
      filter(!is.na(!!metric_sym)) %>%
      mutate(proportion = n / sum(n)) %>%
      complete(!!metric_sym, fill = list(n = 0, proportion = 0)) %>%
      mutate(!!metric_sym := factor(!!metric_sym, levels = metric_levels))
    
    global_plot_data <- global_comparison_data %>%
      count(!!metric_sym, .drop = FALSE) %>%
      filter(!is.na(!!metric_sym)) %>%
      mutate(proportion = n / sum(n)) %>%
      complete(!!metric_sym, fill = list(n = 0, proportion = 0)) %>%
      mutate(!!metric_sym := factor(!!metric_sym, levels = metric_levels))
    
    plot_title <- paste(base_title, "for", subset_label, "vs.", global_label)
    
    p <- ggplot() +
      geom_col(data = subset_plot_data, aes(x = !!metric_sym, y = proportion, fill = !!metric_sym),
               width = 0.7) +
      geom_line(data = global_plot_data, aes(x = !!metric_sym, y = proportion, group = 1),
                color = "red", size = 1) +
      geom_point(data = global_plot_data, aes(x = !!metric_sym, y = proportion, group = 1),
                 color = "red", size = 2) +
      theme_minimal() +
      labs(title = plot_title,
           x = metric_name,
           y = "Proportion of Respondents",
           fill = metric_name) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set2") +
      scale_y_continuous(limits = c(0, 1))
    
    ggplotly(p, tooltip = c("x", "y"))
  }
  
  create_scatter_plot <- function(data, metric_col, base_title, subset_label) {
    metric_sym <- enquo(metric_col)
    metric_name <- as_label(metric_sym)
    
    if (!is.numeric(data[[metric_name]]) && !is.factor(data[[metric_name]])) {
      warning(paste("Metric column", metric_name, "is not numeric or factor. Attempting factor conversion."))
      data[[metric_name]] <- factor(data[[metric_name]])
    }
    
    plot_title <- paste(base_title, "for", subset_label)
    
    if (is.numeric(data[[metric_name]])) {
      p <- ggplot(data, aes(x = hours_per_day, y = !!metric_sym, color = age)) +
        geom_point(alpha = 0.6) +
        theme_minimal() +
        labs(title = plot_title,
             x = "Hours per Day",
             y = metric_name,
             color = "Age") +
        scale_color_viridis_c(option = "plasma", labels = scales::number_format())
    } else if (is.factor(data[[metric_name]])) {
      p <- ggplot(data, aes(x = hours_per_day, y = !!metric_sym, color = age)) +
        geom_point(alpha = 0.6, position = position_jitter(height = 0.1)) +
        theme_minimal() +
        labs(title = plot_title,
             x = "Hours per Day",
             y = metric_name,
             color = "Age") +
        scale_color_viridis_c(option = "plasma", labels = scales::number_format())
    } else {
      warning(paste("Metric column", metric_name, "is not numeric or factor. Cannot plot."))
      return(NULL)
    }
    
    ggplotly(p, tooltip = c("x", "y", "color", "age"))
  }
  
  output$data <- renderDataTable({
    datatable(music_processed, options = list(scrollX = TRUE))
  })
  
  output$plot_anxiety_freq_dist <- renderPlotly({
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
    create_distribution_plot(filtered_subset_data_freq(), global_data_for_freq_plot(), anxiety_interval, "Anxiety Distribution", subset_label, global_label)
  })
  
  output$plot_depression_freq_dist <- renderPlotly({
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
  
  output$plot_hours_anxiety_scatter <- renderPlotly({
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
  
  ### Real Dashboard starts here ###
  
  # Reset changes every time the variable is changed or number of clusters is less than selected cluster number
  # Act like a live object
  rv <- reactiveValues(
    previous_cluster_var1 = NULL,
    previous_cluster_var2 = NULL,
    previous_num_clusters = NULL,
    selected_cluster = NULL
  )
  
  # Reactive expression for clustering 
  cluster_data <- reactive({
    req(input$cluster_var1, input$cluster_var2, input$num_clusters)
    
    if (is.null(rv$previous_cluster_var1) || is.null(rv$previous_cluster_var2) || is.null(rv$previous_num_clusters) ||
        rv$previous_cluster_var1 != input$cluster_var1 || 
        rv$previous_cluster_var2 != input$cluster_var2 || 
        rv$previous_num_clusters != input$num_clusters) 
    {
      rv$selected_cluster <- NULL
      rv$previous_cluster_var1 <- input$cluster_var1
      rv$previous_cluster_var2 <- input$cluster_var2
      rv$previous_num_clusters <- input$num_clusters
    }
    
    cluster_vars <- c(input$cluster_var1, input$cluster_var2)
    
    all_relevant_cols <- unique(c(
      cluster_vars,
      "anxiety", "depression", "insomnia", "ocd",
      "hours_per_day", "age", "bpm",
      colnames(music_processed)[grep("^frequency_", colnames(music_processed))],
      "music_effects", "fav_genre",
      "cluster"
    ))
    
    data_for_clustering <- music_processed %>%
      select(any_of(all_relevant_cols)) %>%
      na.omit()
    
    if (nrow(data_for_clustering) == 0) {
      return(NULL)
    }
    
    actual_cluster_vars_present <- intersect(cluster_vars, colnames(data_for_clustering))
    
    if (length(actual_cluster_vars_present) < 2) {
      return(NULL)
    }
    
    scaled_data <- scale(data_for_clustering[, actual_cluster_vars_present])
    
    dist_matrix <- dist(scaled_data, method = "euclidean")
    
    # Performs hierarchical clustering on scaled data
    hclust_result <- hclust(dist_matrix, method = "ward.D2")
    
    cluster_assignments <- cutree(hclust_result, k = input$num_clusters)
    
    data_for_clustering$cluster <- factor(cluster_assignments)
    
    return(data_for_clustering)
  })
  
  # Update the selected cluster when main plot is clicked
  observeEvent(event_data("plotly_click", source = "cluster_plot"), {
    click_data <- event_data("plotly_click", source = "cluster_plot")
    
    if(!is.null(click_data) && !is.null(click_data$customdata)) {
      selected_cluster <- click_data$customdata[1]
      
      if(!is.null(input$num_clusters) && as.numeric(selected_cluster) <= input$num_clusters) {
        rv$selected_cluster <- selected_cluster
      } else {
        rv$selected_cluster <- NULL
      }
    }
  })
  
  # Both observers are ensuring proper x and y axes dropdowns
  observeEvent(input$cluster_var1, {
    req(input$cluster_var1)
    all_clustering_vars <- names(music_processed)[sapply(music_processed, is.numeric)]
    y_choices <- setdiff(all_clustering_vars, input$cluster_var1)
    
    updateSelectInput(session, "cluster_var2",
                      choices = y_choices,
                      selected = if(input$cluster_var2 != input$cluster_var1) input$cluster_var2 else y_choices[1])
  })
  
  observeEvent(input$cluster_var2, {
    req(input$cluster_var2)
    all_clustering_vars <- names(music_processed)[sapply(music_processed, is.numeric)]
    
    x_choices <- setdiff(all_clustering_vars, input$cluster_var2)
    updateSelectInput(session, "cluster_var1",
                      choices = x_choices,
                      selected = if(input$cluster_var1 != input$cluster_var2) input$cluster_var1 else x_choices[1])
  })
  
  # Provides data for currently selected cluster
  selected_cluster_data <- reactive({
    if(is.null(rv$selected_cluster) || is.null(cluster_data()) || is.null(input$num_clusters)) {
      return(NULL)
    }
    
    if(as.numeric(rv$selected_cluster) > input$num_clusters) {
      rv$selected_cluster <- NULL
      return(NULL)
    }
    
    filtered_data <- cluster_data() %>%
      filter(cluster == rv$selected_cluster)
    
    if(nrow(filtered_data) == 0) {
      rv$selected_cluster <- NULL
      return(NULL)
    }
    
    return(filtered_data)
  })
  
  # Reactive object updates for title updates
  cluster_label <- reactive({
    selected_data <- selected_cluster_data()
    if(is.null(selected_data)) {
      return("All Data")
    } else {
      return(paste("Cluster", rv$selected_cluster))
    }
  })
  
  # Main plot
  output$mental_health_cluster_plot <- renderPlotly({
    req(cluster_data(), input$cluster_var1, input$cluster_var2)
    
    data <- cluster_data()
    
    x_var <- input$cluster_var1
    y_var <- input$cluster_var2
    
    p <- plot_ly(data,
                 x = ~get(x_var),
                 y = ~get(y_var),
                 color = ~cluster,
                 customdata = ~cluster,
                 type = "scatter",
                 mode = "markers",
                 marker = list(size = 10, opacity = 0.7),
                 source = "cluster_plot") %>%
      layout(title = "Clusters (click on a point to select a cluster)",
             xaxis = list(title = x_var),
             yaxis = list(title = y_var),
             showlegend = TRUE)
    
    if(input$show_cluster_labels) {
      centroids <- data %>%
        group_by(cluster) %>%
        summarize(x = mean(get(x_var)),
                  y = mean(get(y_var)))
      
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
  
  
  
  # Radar plot
  output$selected_cluster_radar <- renderPlotly({
    req(cluster_data())
    
    all_data <- cluster_data()
    
    selected_data <- selected_cluster_data()
    
    if(is.null(selected_data)) {
      plot_title <- "Music Genre Preferences - All Data"
      show_comparison <- FALSE
      selected_data <- all_data
    } else {
      plot_title <- paste("Music Genre Preferences - Cluster", unique(selected_data$cluster), "vs All Data")
      show_comparison <- TRUE
    }
    
    frequency_cols <- colnames(all_data)[grep("^frequency_", colnames(all_data))]
    
    if (length(frequency_cols) == 0) {
      return(plotly_empty() %>% layout(title = "No frequency data available for radar chart."))
    }
    
    radar_data_selected <- selected_data %>%
      mutate(across(all_of(frequency_cols),
                    ~as.numeric(factor(., levels = c("Never", "Rarely", "Sometimes", "Very frequently"))))) %>%
      summarize(across(all_of(frequency_cols), mean, na.rm = TRUE))
    
    radar_data_all <- all_data %>%
      mutate(across(all_of(frequency_cols),
                    ~as.numeric(factor(., levels = c("Never", "Rarely", "Sometimes", "Very frequently"))))) %>%
      summarize(across(all_of(frequency_cols), mean, na.rm = TRUE))
    
    radar_long_selected <- radar_data_selected %>%
      tidyr::pivot_longer(cols = everything(),
                          names_to = "Genre",
                          values_to = "Frequency") %>%
      mutate(Genre = gsub("frequency_", "", Genre),
             Genre = gsub("_", " ", Genre),
             Dataset = if(is.null(selected_cluster_data())) "All Data" else paste("Cluster", unique(selected_data$cluster)))
    
    radar_long_all <- radar_data_all %>%
      tidyr::pivot_longer(cols = everything(),
                          names_to = "Genre",
                          values_to = "Frequency") %>%
      mutate(Genre = gsub("frequency_", "", Genre),
             Genre = gsub("_", " ", Genre),
             Dataset = "All Data")
    
    p <- plot_ly()
    
    cluster_fill_color <- if(is.null(selected_cluster_data())) 'rgba(70, 130, 180, 0.3)' else 'rgba(255, 100, 50, 0.6)'
    cluster_line_color <- if(is.null(selected_cluster_data())) 'rgba(50, 100, 160, 0.9)' else 'rgba(220, 70, 20, 0.9)'
    
    p <- p %>% add_trace(
      data = radar_long_selected,
      r = ~Frequency,
      theta = ~Genre,
      name = unique(radar_long_selected$Dataset),
      fill = 'toself',
      type = 'scatterpolar',
      fillcolor = cluster_fill_color,
      line = list(
        color = cluster_line_color, 
        width = 2.5
      ),
      marker = list(
        size = 5,
        color = cluster_line_color,
        symbol = 'circle'
      )
    )
    
    p <- p %>% add_trace(
      data = radar_long_all,
      r = ~Frequency,
      theta = ~Genre,
      name = "Global Average",
      fill = 'none',
      type = 'scatterpolar',
      line = list(
        color = 'rgb(0, 50, 150)',
        width = 3,
        dash = 'solid'
      ),
      marker = list(
        size = 6, 
        color = 'rgb(0, 50, 150)',
        symbol = 'circle'
      )
    )
    
    p <- p %>% layout(
      polar = list(
        radialaxis = list(
          visible = TRUE,
          range = c(0, 4.2),
          tickvals = c(1, 2, 3, 4),
          ticktext = c("Never", "Rarely", "Sometimes", "Very frequently")
        )
      ),
      title = plot_title,
      legend = list(
        x = 0.5, 
        y = 1.1, 
        orientation = 'h',
        traceorder = "normal"
      ),
      margin = list(t = 100, b = 50, l = 50, r = 50),
      autosize = TRUE,
      height = 600
    )
    
    return(p)
  })
  
  # Box plot with mental health metrics
  output$selected_cluster_boxplots <- renderPlotly({
    req(cluster_data())
    
    all_data <- cluster_data()
    
    selected_data <- selected_cluster_data()
    
    if(is.null(selected_data)) {
      plot_data <- all_data
      plot_title <- "Mental Health Metrics - All Data (Click on a cluster from the plot)"
    } else {
      plot_data <- selected_data
      plot_title <- paste("Mental Health Metrics - Cluster", unique(selected_data$cluster))
    }
    
    mental_health_cols_to_plot <- c("anxiety", "depression", "insomnia", "ocd")
    
    plot_data_for_boxplot <- plot_data %>%
      select(all_of(mental_health_cols_to_plot))
    
    if (nrow(plot_data_for_boxplot) == 0) {
      return(plotly_empty() %>% layout(title = "No data available for Mental Health Metrics."))
    }
    
    boxplot_data <- plot_data_for_boxplot %>%
      tidyr::pivot_longer(cols = everything(),
                          names_to = "Metric",
                          values_to = "Value") %>%
      mutate(Metric = factor(Metric, levels = mental_health_cols_to_plot))
    
    metric_colors <- c("anxiety" = "#FF7F50", "depression" = "#6495ED",
                       "insomnia" = "#32CD32", "ocd" = "#FFD700")
    
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
                     categoryarray = mental_health_cols_to_plot,
                     ticktext = c("Anxiety", "Depression", "Insomnia", "OCD"),
                     tickvals = mental_health_cols_to_plot),
        yaxis = list(title = "Score (0-10)",
                     range = c(0, 10))
      )
    
    return(p)
  })
  
  output$cluster_data_table <- renderDataTable({
    data <- selected_cluster_data()
    if(is.null(data)) {
      data <- cluster_data()
    }
    
    display_cols <- c("cluster", "anxiety", "depression", "insomnia", "ocd",
                      "hours_per_day", "fav_genre", "music_effects")
    
    available_display_cols <- intersect(display_cols, colnames(data))
    
    display_data <- data %>%
      select(all_of(available_display_cols))
    
    datatable(display_data,
              options = list(pageLength = 5,
                             scrollX = TRUE),
              caption = if(is.null(selected_cluster_data())) {
                "All Respondents (Click on a cluster in the plot above to filter)"
              } else {
                paste("Respondents in Cluster", unique(data$cluster))
              })
  })
  
  # Line graph
  output$cluster_mental_health_comparison <- renderPlotly({
    req(cluster_data())
    
    data <- cluster_data()
    selected_data <- selected_cluster_data()
    
    mental_health_vars <- c("anxiety", "depression", "insomnia", "ocd")
    available_vars <- intersect(mental_health_vars, colnames(data))
    
    if(length(available_vars) == 0) {
      return(plotly_empty() %>% layout(title = "No mental health data available"))
    }
    
    cluster_stats <- data %>%
      group_by(cluster) %>%
      summarise(
        across(all_of(available_vars), list(
          mean = ~mean(.x, na.rm = TRUE),
          median = ~median(.x, na.rm = TRUE),
          sd = ~sd(.x, na.rm = TRUE),
          q75 = ~quantile(.x, 0.75, na.rm = TRUE)
        ), .names = "{.col}_{.fn}"),
        cluster_size = n(),
        avg_hours = mean(hours_per_day, na.rm = TRUE),
        music_helps_pct = mean(music_effects == "Improve", na.rm = TRUE) * 100,
        .groups = 'drop'
      )
    
    mean_cols <- paste0(available_vars, "_mean")
    cluster_means <- cluster_stats %>%
      select(cluster, cluster_size, avg_hours, music_helps_pct, all_of(mean_cols)) %>%
      tidyr::pivot_longer(cols = all_of(mean_cols),
                          names_to = "Metric",
                          values_to = "Average_Score") %>%
      mutate(Metric = gsub("_mean$", "", Metric),
             Metric = tools::toTitleCase(Metric))
    
    sd_cols <- paste0(available_vars, "_sd")
    cluster_sds <- cluster_stats %>%
      select(cluster, all_of(sd_cols)) %>%
      tidyr::pivot_longer(cols = all_of(sd_cols),
                          names_to = "Metric",
                          values_to = "SD") %>%
      mutate(Metric = gsub("_sd$", "", Metric),
             Metric = tools::toTitleCase(Metric))
    
    cluster_plot_data <- cluster_means %>%
      left_join(cluster_sds, by = c("cluster", "Metric"))
    
    cluster_colors <- viridis::viridis(length(unique(cluster_plot_data$cluster)), option = "plasma")
    names(cluster_colors) <- sort(unique(cluster_plot_data$cluster))
    
    
    p <- plot_ly()
    
    for(clust in unique(cluster_plot_data$cluster)) {
      cluster_subset <- cluster_plot_data %>% filter(cluster == clust)
      
      is_selected <- !is.null(selected_data) && clust == unique(selected_data$cluster)
      
      p <- p %>% add_trace(
        data = cluster_subset,
        x = ~Metric,
        y = ~Average_Score,
        error_y = list(array = ~SD, color = cluster_colors[as.character(clust)]),
        name = paste0("Cluster ", clust, " (n=", unique(cluster_subset$cluster_size), ")"),
        type = "scatter",
        mode = "lines+markers",
        line = list(width = if(is_selected) 4 else 2.5, color = cluster_colors[as.character(clust)]),
        marker = list(size = if(is_selected) 12 else 8, color = cluster_colors[as.character(clust)]),
        text = ~paste(
          "Cluster:", clust, "<br>",
          "Size:", cluster_size, "people<br>",
          "Score:", round(Average_Score, 2), "±", round(SD, 2), "<br>",
          "Avg Hours/Day:", round(avg_hours, 1), "<br>",
          "Music Helps:", round(music_helps_pct, 1), "%"
        ),
        hovertemplate = "%{text}<extra></extra>"
      )
    }
    
    p <- p %>% layout(
      title = list(
        text = "Mental Health Profiles by Cluster<br><sub>Error bars show ±1 standard deviation</sub>",
        font = list(size = 16)
      ),
      xaxis = list(title = "Mental Health Metrics"),
      yaxis = list(title = "Average Score (0-10)", range = c(0, 10)),
      legend = list(title = list(text = "Cluster"))
    )
    
    return(p)
  })
  
  # Age bar chart
  output$cluster_demographics <- renderPlotly({
    req(cluster_data())
    
    data <- cluster_data()
    selected_data <- selected_cluster_data()
    
    demographic_vars <- c("age", "primary_streaming_service")
    available_vars <- intersect(demographic_vars, colnames(data))
    
    if(length(available_vars) == 0) {
      return(plotly_empty() %>% layout(title = "No demographic data available"))
    }
    
    if("age" %in% available_vars) {
      data$age_group <- cut(data$age, 
                            breaks = c(0, 20, 30, 40, 50, 100), 
                            labels = c("Under 20", "20-29", "30-39", "40-49", "50+"),
                            include.lowest = TRUE)
      
      age_summary <- data %>%
        group_by(cluster, age_group) %>%
        summarise(count = n(), .groups = 'drop') %>%
        group_by(cluster) %>%
        mutate(percentage = count / sum(count) * 100) %>%
        filter(!is.na(age_group))
      
      age_summary$is_selected <- FALSE
      if(!is.null(selected_data)) {
        selected_cluster_num <- unique(selected_data$cluster)
        age_summary$is_selected <- age_summary$cluster == selected_cluster_num
      }
      
      age_colors <- c("Under 20" = "#FF6B6B", "20-29" = "#4ECDC4", "30-39" = "#45B7D1", 
                      "40-49" = "#96CEB4", "50+" = "#FECA57")
      
      p <- plot_ly(age_summary,
                   x = ~as.factor(cluster),
                   y = ~percentage,
                   color = ~age_group,
                   colors = age_colors,
                   type = "bar",
                   opacity = ~ifelse(is_selected, 1.0, 0.6),
                   text = ~paste(count, "people"),
                   textposition = "inside",
                   hovertemplate = paste("Cluster %{x}<br>",
                                         "Age Group: %{data.name}<br>",
                                         "Percentage: %{y:.1f}%<br>",
                                         "Count: %{text}<br>",
                                         "<extra></extra>")) %>%
        layout(title = "Age Group Distribution by Cluster",
               xaxis = list(title = "Cluster"),
               yaxis = list(title = "Percentage"),
               barmode = "stack")
      
      return(p)
    }
    
    return(plotly_empty() %>% layout(title = "Age data not available"))
  })
  
  ### Enhanced Correlation Heatmap Tab with Category Filtering and Separate Radar Plots ###
  
  # Define category groups
  category_groups <- list(
    "Genres" = c("frequency_classical", "frequency_country", "frequency_edm", "frequency_folk", 
                 "frequency_gospel", "frequency_hip_hop", "frequency_jazz", "frequency_k_pop", 
                 "frequency_latin", "frequency_lofi", "frequency_metal", "frequency_pop", 
                 "frequency_r_b", "frequency_rap", "frequency_rock", "frequency_video_game_music"),
    "Mental Health" = c("anxiety", "depression", "insomnia", "ocd"),
    "Music Background" = c("instrumentalist", "composer", "while_working", "exploratory", "foreign_languages"),
    "Music Characteristics" = c("bpm", "hours_per_day", "music_effects"),
    "Demographics" = c("age")
  )
  
  # Prepare correlation data with category filtering
  correlation_data <- reactive({
    req(input$selected_category_groups)
    
    # Get selected categories
    selected_categories <- c()
    if ("genres" %in% input$selected_category_groups) {
      selected_categories <- c(selected_categories, category_groups$Genres)
    }
    if ("mental_health" %in% input$selected_category_groups) {
      selected_categories <- c(selected_categories, category_groups$`Mental Health`)
    }
    if ("music_background" %in% input$selected_category_groups) {
      selected_categories <- c(selected_categories, category_groups$`Music Background`)
    }
    if ("music_characteristics" %in% input$selected_category_groups) {
      selected_categories <- c(selected_categories, category_groups$`Music Characteristics`)
    }
    if ("demographics" %in% input$selected_category_groups) {
      selected_categories <- c(selected_categories, category_groups$Demographics)
    }
    
    # If no categories selected, use all available variables
    if (length(selected_categories) == 0) {
      selected_categories <- names(music_processed)[sapply(music_processed, function(x) is.numeric(x) || is.logical(x))]
      selected_categories <- setdiff(selected_categories, c("timestamp", "permissions"))
    }
    
    # Filter for existing columns only
    available_categories <- intersect(selected_categories, names(music_processed))
    
    if (length(available_categories) == 0) {
      return(NULL)
    }
    
    # Prepare data for correlation
    corr_data <- music_processed %>%
      select(all_of(available_categories))
    
    # Convert different data types to numeric
    corr_data <- corr_data %>%
      mutate(
        # Convert frequency columns to numeric (assuming they have levels like Never, Rarely, Sometimes, Very frequently)
        across(starts_with("frequency_"), ~{
          if (is.factor(.)) {
            as.numeric(.)
          } else if (is.character(.)) {
            factor_levels <- c("Never", "Rarely", "Sometimes", "Very frequently")
            as.numeric(factor(., levels = factor_levels))
          } else {
            as.numeric(.)
          }
        }),
        # Convert logical columns to numeric
        across(where(is.logical), as.numeric),
        # Convert character columns that might be Yes/No to numeric
        across(where(is.character), ~{
          if (all(na.omit(.) %in% c("Yes", "No"))) { # Handle NA values correctly
            as.numeric(. == "Yes")
          } else if (is.factor(.)) { # If it's already a factor (like music_effects)
            as.numeric(.)
          }
          else {
            # For other character columns, attempt to factorize then numeric,
            # but be cautious as this might not always be meaningful.
            # This part might need specific handling if other char columns are meant for correlation.
            # For now, we assume they are not primary correlation targets unless specified.
            as.numeric(as.factor(.))
          }
        }),
        # Ensure other columns are numeric
        across(everything(), as.numeric)
      )
    
    # Remove columns with all NA or constant values
    corr_data <- corr_data %>%
      select(where(~!all(is.na(.)) && length(unique(na.omit(.))) > 1))
    
    # Check if we have any valid numeric data
    if (ncol(corr_data) < 2) {
      return(NULL)
    }
    
    # Calculate correlation matrix
    cor_matrix <- cor(corr_data, use = "pairwise.complete.obs")
    
    return(list(
      matrix = cor_matrix,
      data = corr_data,
      raw_data = music_processed,
      selected_vars = names(corr_data)
    ))
  })
  
  # Find category combinations above threshold
  category_combinations <- reactive({
    req(correlation_data(), input$correlation_threshold)
    
    cor_data <- correlation_data()
    if (is.null(cor_data)) return(data.frame())
    
    cor_matrix <- cor_data$matrix
    mental_health_vars <- intersect(category_groups$`Mental Health`, rownames(cor_matrix))
    
    if (length(mental_health_vars) == 0) {
      return(data.frame())
    }
    
    # Find variables that correlate strongly with any mental health variable
    combinations <- list()
    
    for (mh_var in mental_health_vars) {
      # Find variables with strong correlation to this mental health variable
      correlations <- cor_matrix[mh_var, ]
      strong_correlations <- correlations[abs(correlations) >= input$correlation_threshold & 
                                            names(correlations) != mh_var & 
                                            !is.na(correlations)]
      
      if (length(strong_correlations) > 0) {
        combinations[[mh_var]] <- names(strong_correlations)
      }
    }
    
    # Create combination data frame
    combination_df <- data.frame()
    for (mh_var in names(combinations)) {
      for (corr_var in combinations[[mh_var]]) {
        clean_var_name <- gsub("frequency_|_", " ", corr_var)
        clean_var_name <- tools::toTitleCase(clean_var_name)
        
        combination_df <- rbind(combination_df, data.frame(
          mental_health_var = mh_var,
          correlated_var = corr_var,
          correlation = cor_matrix[mh_var, corr_var],
          combination_name = paste(tools::toTitleCase(gsub("_", " ", mh_var)), "×", clean_var_name),
          stringsAsFactors = FALSE
        ))
      }
    }
    
    return(combination_df)
  })
  
  # Create correlation heatmap with category filtering
  output$correlation_heatmap_plot <- renderPlotly({
    req(correlation_data(), input$correlation_threshold)
    
    cor_data <- correlation_data()
    if (is.null(cor_data)) {
      return(plotly_empty() %>% 
               layout(title = "No valid numeric data found for selected categories"))
    }
    
    cor_matrix <- cor_data$matrix
    
    # Convert correlation matrix to long format for plotting
    cor_long <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
    cor_long$value <- as.vector(cor_matrix)
    
    # Clean up variable names for better display
    cor_long$Var1_clean <- gsub("frequency_", "", cor_long$Var1)
    cor_long$Var1_clean <- tools::toTitleCase(gsub("_", " ", cor_long$Var1_clean))
    cor_long$Var2_clean <- gsub("frequency_", "", cor_long$Var2)
    cor_long$Var2_clean <- tools::toTitleCase(gsub("_", " ", cor_long$Var2_clean))
    
    # Determine if a cell should be highlighted
    cor_long$is_highlighted <- abs(cor_long$value) >= input$correlation_threshold | 
      cor_long$Var1 == cor_long$Var2
    
    # Create z_for_plot for dual colorscale
    cor_long$z_for_plot <- ifelse(
      cor_long$is_highlighted,
      cor_long$value,
      2 + (cor_long$value + 1) / 2 
    )
    
    # Define colorscale
    zmin_plot <- -1
    zmax_plot <- 3 
    z_span <- zmax_plot - zmin_plot
    
    vibrant_colors_def <- list(
      list(0, "#053061"), list(0.1, "#2166AC"), list(0.2, "#4393C3"),
      list(0.3, "#92C5DE"), list(0.4, "#D1E5F0"), list(0.5, "#F7F7F7"),
      list(0.6, "#FDBF6F"), list(0.7, "#FF7F00"), list(0.8, "#E31A1C"),
      list(0.9, "#B10026"), list(1, "#67001F")
    )
    
    vibrant_transformed <- lapply(vibrant_colors_def, function(item) {
      p_orig_norm = item[[1]]
      new_norm = ((-1 + 2 * p_orig_norm) - zmin_plot) / z_span 
      list(new_norm, item[[2]])
    })
    
    gray_colors_def <- list(
      list(0, "#BEBEBE"), list(0.5, "#F0F0F0"), list(1, "#BEBEBE")
    )
    
    gray_transformed <- lapply(gray_colors_def, function(item) {
      p_gray_norm = item[[1]]
      z_val_for_gray = 2 + p_gray_norm 
      new_norm = (z_val_for_gray - zmin_plot) / z_span
      list(new_norm, item[[2]])
    })
    
    final_colorscale <- c(vibrant_transformed, gray_transformed)
    final_colorscale <- final_colorscale[order(sapply(final_colorscale, `[[`, 1))]
    
    p <- plot_ly(
      data = cor_long,
      x = ~Var2_clean,
      y = ~Var1_clean,
      z = ~z_for_plot,
      customdata = ~value,
      type = "heatmap",
      colorscale = final_colorscale,
      zmin = zmin_plot,
      zmax = zmax_plot,
      hovertemplate = paste(
        "<b>%{y} vs %{x}</b><br>",
        "Correlation: %{customdata:.3f}<br>",
        "<extra></extra>"
      ),
      showscale = TRUE,
      colorbar = list(
        title = "Correlation",
        titleside = "right",
        tickvals = c(-1, -0.5, 0, 0.5, 1),
        ticktext = c("-1", "-0.5", "0", "0.5", "1")
      )
    ) %>%
      layout(
        title = list(
          text = paste("Correlation Matrix - Highlighting |r| ≥", input$correlation_threshold),
          font = list(size = 16)
        ),
        xaxis = list(
          title = "",
          tickangle = 45,
          side = "bottom",
          categoryorder = "array",
          categoryarray = unique(cor_long$Var2_clean[order(match(cor_long$Var2, colnames(cor_matrix)))])
        ),
        yaxis = list(
          title = "",
          autorange = "reversed",
          categoryorder = "array",
          categoryarray = unique(cor_long$Var1_clean[order(match(cor_long$Var1, rownames(cor_matrix)))])
        ),
        margin = list(l = 120, r = 50, t = 100, b = 120)
      )
    
    if (input$show_values) {
      annotation_data <- cor_long[abs(cor_long$value) >= input$correlation_threshold & 
                                    cor_long$Var1 != cor_long$Var2 &
                                    !is.na(cor_long$value), ] # Ensure no NA values for annotation
      
      if (nrow(annotation_data) > 0) {
        p <- p %>% add_annotations(
          data = annotation_data,
          x = ~Var2_clean,
          y = ~Var1_clean,
          text = ~round(value, 2),
          showarrow = FALSE,
          font = list(color = "white", size = 10, family = "Arial")
        )
      }
    }
    
    return(p)
  })
  
  # Create strong correlations summary table
  output$strong_correlations_table <- renderDataTable({
    req(correlation_data(), input$correlation_threshold)
    
    cor_data <- correlation_data()
    if (is.null(cor_data)) {
      return(datatable(data.frame(Message = "No valid data for selected categories")))
    }
    
    cor_matrix <- cor_data$matrix
    
    cor_long <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
    cor_long$Correlation <- as.vector(cor_matrix)
    
    strong_cors <- cor_long %>%
      filter(abs(Correlation) >= input$correlation_threshold,
             Var1 != Var2,
             !is.na(Correlation)) %>%
      filter(as.numeric(factor(Var1)) < as.numeric(factor(Var2))) %>%
      arrange(desc(abs(Correlation)))
    
    if (nrow(strong_cors) == 0) {
      return(datatable(data.frame(Message = paste("No correlations found above threshold", input$correlation_threshold))))
    }
    
    strong_cors$Variable1 <- gsub("frequency_", "", strong_cors$Var1)
    strong_cors$Variable1 <- tools::toTitleCase(gsub("_", " ", strong_cors$Variable1))
    
    strong_cors$Variable2 <- gsub("frequency_", "", strong_cors$Var2)
    strong_cors$Variable2 <- tools::toTitleCase(gsub("_", " ", strong_cors$Variable2))
    
    strong_cors$Strength <- ifelse(abs(strong_cors$Correlation) >= 0.7, "Strong",
                                   ifelse(abs(strong_cors$Correlation) >= 0.5, "Moderate", "Weak"))
    
    strong_cors$Direction <- ifelse(strong_cors$Correlation > 0, "Positive", "Negative")
    
    display_data <- strong_cors %>%
      select(Variable1, Variable2, Correlation, Strength, Direction) %>%
      mutate(Correlation = round(Correlation, 3))
    
    datatable(
      display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        order = list(list(2, 'desc'))
      ),
      caption = paste("Variable pairs with |correlation| ≥", input$correlation_threshold),
      rownames = FALSE
    ) %>%
      formatStyle(
        "Correlation",
        backgroundColor = styleInterval(
          cuts = c(-0.7, -0.5, -0.3, 0.3, 0.5, 0.7),
          values = c("#053061", "#2166AC", "#92C5DE", "#F7F7F7", "#FDBF6F", "#E31A1C", "#67001F")
        ),
        color = styleInterval(
          cuts = c(-0.3, 0.3),
          values = c("white", "black", "white")
        )
      )
  })
  
}