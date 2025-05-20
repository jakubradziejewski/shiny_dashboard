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

music_raw <- read_csv("music.csv", show_col_types = FALSE)

music_clean_names <- music_raw %>%
  clean_names()

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
  
  cluster_data <- reactive({
    req(input$cluster_var1, input$cluster_var2, input$num_clusters)
    
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
    hclust_result <- hclust(dist_matrix, method = "ward.D2")
    
    cluster_assignments <- cutree(hclust_result, k = input$num_clusters)
    
    data_for_clustering$cluster <- factor(cluster_assignments)
    
    return(data_for_clustering)
  })
  
  selected_cluster_data <- reactive({
    event_data <- event_data("plotly_click", source = "cluster_plot")
    
    if(is.null(event_data)) {
      return(NULL)
    }
    
    selected_cluster <- event_data$customdata[1]
    
    cluster_data() %>%
      filter(cluster == selected_cluster)
  })
  
  cluster_label <- reactive({
    selected_data <- selected_cluster_data()
    if(is.null(selected_data)) {
      return("All Data")
    } else {
      return(paste("Cluster", unique(selected_data$cluster)))
    }
  })
  
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
      layout(title = "Mental Health Profile Clusters",
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
    
    p <- plot_ly()
    
    if(show_comparison) {
      radar_long_all <- radar_data_all %>%
        tidyr::pivot_longer(cols = everything(),
                            names_to = "Genre",
                            values_to = "Frequency") %>%
        mutate(Genre = gsub("frequency_", "", Genre),
               Genre = gsub("_", " ", Genre),
               Dataset = "All Data")
      
      p <- p %>% add_trace(
        data = radar_long_all,
        r = ~Frequency,
        theta = ~Genre,
        name = "All Data",
        fill = 'toself',
        type = 'scatterpolar',
        fillcolor = 'rgba(200, 200, 200, 0.2)',
        line = list(color = 'rgba(150, 150, 150, 0.7)', width = 1, dash = 'dot')
      )
    }
    
    p <- p %>% add_trace(
      data = radar_long_selected,
      r = ~Frequency,
      theta = ~Genre,
      name = unique(radar_long_selected$Dataset),
      fill = 'toself',
      type = 'scatterpolar',
      fillcolor = 'rgba(255, 100, 50, 0.6)',
      line = list(color = 'rgb(255, 100, 50)', width = 2.5)
    )
    
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
  
  output$selected_cluster_barchart <- renderPlotly({
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
  
}