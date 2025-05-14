# --- server.R ---

library(shiny)
library(dlnm)
library(dplyr)
library(plotly)
library(DT)
library(RColorBrewer)
library(tidyr)
library(tibble)
library(later)

source("../db_connect.R")
source("../queries/extract_procedures.R")
source("../dlnm_data.R")

# Utility function: darken a given color for line contrast
darken_color <- function(col, factor = 0.7) {
  rgb_val <- col2rgb(col)
  new_rgb <- round(rgb_val * factor)
  rgb(new_rgb[1,1], new_rgb[2,1], new_rgb[3,1], maxColorValue = 255)
}

# Shared container for analysis results across the session
analysis_results <- reactiveValues(
  basePlot = NULL, 
  pred = NULL, 
  selected_lags = NULL, 
  base_colors = NULL, 
  line_colors = NULL
  )


# Defining the server logic
server <- function(input, output, session) {
  shinyjs::useShinyjs()
  
  # Clear manual input when switching to "list" mode
  observeEvent(input$procedure_mode, {
    if (input$procedure_mode == "list") {
      updateTextInput(session, "procedure_id_manual", value = "")
    }
  })
  
  
  # Hospital selection dropdown grouped by city
  observe({
    grouped_choices <- data.frame(
      label = c(
        "All",
        "Tartu Ülikooli Kliinikum SA",
        "Ida-Tallinna Keskhaigla AS", "Põhja-Eesti Regionaalhaigla SA",
        "Lääne-Tallinna Keskhaigla AS", "Tallinna Lastehaigla SA",
        "Pärnu Haigla SA", "Läänemaa Haigla SA", "Hiiumaa Haigla SA",
        "Ida-Viru Keskhaigla SA", "Rakvere Haigla AS",
        "Kuressaare Haigla SA", "Narva Haigla SA", "Järvamaa Haigla AS",
        "Valga Haigla AS", "Viljandi Haigla SA", "Lõuna-Eesti Haigla AS",
        "Jõgeva Haigla SA", "Raplamaa Haigla SA"
      ),
      value = c(
        "All",
        "Tartu Ülikooli Kliinikum SA",
        "Ida-Tallinna Keskhaigla AS", "Põhja-Eesti Regionaalhaigla SA",
        "Lääne-Tallinna Keskhaigla AS", "Tallinna Lastehaigla SA",
        "Pärnu Haigla SA", "Läänemaa Haigla SA", "Hiiumaa Haigla SA",
        "Ida-Viru Keskhaigla SA", "Rakvere Haigla AS",
        "Kuressaare Haigla SA", "Narva Haigla SA", "Järvamaa Haigla AS",
        "Valga Haigla AS", "Viljandi Haigla SA", "Lõuna-Eesti Haigla AS",
        "Jõgeva Haigla SA", "Raplamaa Haigla SA"
      ),
      city = c(
        "--- All ---",
        rep("Tartu", 1),
        rep("Tallinn", 4),
        rep("Pärnu", 1),
        rep("Haapsalu", 1),
        rep("Kärdla", 1),
        rep("Jõhvi", 1),
        rep("Rakvere", 1),
        rep("Kuressaare", 1),
        rep("Narva", 1),
        rep("Paide", 1),
        rep("Valga", 1),
        rep("Viljandi", 1),
        rep("Võru", 1),
        rep("Jõgeva", 1),
        rep("Rapla", 1)
      ),
      stringsAsFactors = FALSE
    )
    
    updateSelectizeInput(
      session,
      inputId = "hospitals",
      choices = grouped_choices,
      server = TRUE
    )
  })
  
  
  # Procedure dropdown with preselected list
  observe({
    top_procs <- get_selected_procedures()
    
    choices <- setNames(
      top_procs$procedure_concept_id,
      paste0(top_procs$concept_name, " (", top_procs$procedure_concept_id, ")")
    )
    
    updateSelectizeInput(session, "procedure_id_select", choices = choices, server = TRUE)
  })
  
  
  # Triggering the main DLNM analysis when the user clicks "Run Analysis"
  observeEvent(input$run_analysis, {
    
    # Validate that at least one hospital is selected
    if (is.null(input$hospitals) || length(input$hospitals) == 0) {
      showNotification("Please select at least one hospital.", type = "error")
      return(NULL)
    }
  
    # Get procedure ID from manual input or dropdown, depending on mode
    procedure_id <- if (input$procedure_mode == "manual") {
      as.numeric(input$procedure_id_manual)
    } else {
      as.numeric(input$procedure_id_select)
    }
    
    # Check if the procedure ID is valid
    if (is.na(procedure_id) || is.null(procedure_id)) {
      showNotification("Please enter or select a valid procedure concept ID.", type = "error")
      return(NULL)
    }
    
    # Show modal dialog while analysis is running
    showModal(modalDialog(
      title = "Running Analysis", 
      "Please wait while the analysis is being computed...",
      footer = NULL,
      easyClose = FALSE
    ))
    
    shiny::withProgress(message = "Analysing data...", value = 0.5, {
    
    # Connect to the database and load relevant data
    conn <- connect_db()
    combined_data <- prepare_data(conn, procedure_id, "2012-01-01", "2019-12-31")
    disconnect_db(conn)

    # Filter by hospitals unless "All" is selected
    if (!is.null(input$hospitals) && !"All" %in% input$hospitals) {
      combined_data <- combined_data %>%
        filter(care_site_name %in% input$hospitals)
    }
    
    # Check that we have any data after filtering
    if (nrow(combined_data) == 0) {
      showNotification("No data available for the selected procedure and city/hospital.", type = "warning")
      removeModal()
      return(NULL)
    }
    
    # Convert temperature column to numeric
    combined_data$Day_temp <- as.numeric(combined_data$Day_temp)
    
    # Finding the median temp
    median_temp <- as.numeric(median(combined_data$Day_temp, na.rm = TRUE))    
  

    # Ensure non-empty dataset
    if (nrow(combined_data) == 0) {
      showNotification("No data found for the selected procedure/city combination.", type = "error")
      removeModal()
      return()
    }
    
    # Check for missing values in key variables
    if (all(is.na(combined_data$Day_temp)) || all(is.na(combined_data$Procedure_count))) {
      showNotification("Temperature or procedure data contains only missing values.", type = "error")
      removeModal()
      return()
    }
    
    # Require some variation in temperature
    if (length(unique(combined_data$Day_temp)) < 3) {
      showNotification("Not enough temperature variation to run model.", type = "error")
      removeModal()
      return()
    }
    
    
    # Defining the maximum lag used in DLNM
    max_lag <- 10
    
    # Attempt to build basis matrix, fit model and compute predictions
    pred <- tryCatch({
      
      # Create a crossbasis matrix
      basis.temp <- crossbasis(combined_data$Day_temp,
                               lag = max_lag,
                               argvar = list(fun = "ns", df = 6), # natural spline for temperature
                               arglag = list(fun = "ns", df = 5)) # natural spline for lag
      
      # Fit a quasi-Poisson regression model with the basis matrix
      model <- glm(Procedure_count ~ basis.temp, family = quasipoisson(), data = combined_data)
      
      # Generate predictions across temperature range, centered at median
      crosspred(basis.temp, model, by = 1, cen = median_temp)
      
    }, error = function(e) {
      msg <- e$message
      
      # Handling specific known errors with user-friendly messages
      if (grepl("lag.*must be less than.*length", msg, ignore.case = TRUE)) {
        showNotification(
          paste0(
            "The selected lag (", input$heatmap_lag, 
            ") exceeds what the data allows for this procedure and hospital. ",
            "Try choosing a procedure with more data or reducing the lag."
          ),
          type = "error"
        )
      } else if (grepl("coef|basis matrix", msg, ignore.case = TRUE)) {
        showNotification(
          "Not enough data to run the model with these settings. Try another hospital or procedure.",
          type = "error"
        )
      } else {
        showNotification(
          "Unable to run the analysis due to a technical issue. Try adjusting the inputs.",
          type = "error"
        )
      }
      
      removeModal()
      return(NULL)
    })
    
    if (is.null(pred)) return(NULL)
    
    # Save prediction object for later use in visualizations
    analysis_results$pred <- pred
    
    # Calculate baseline average procedure count near the median temperature
    baseline_count <- combined_data %>%
      filter(abs(Day_temp - median_temp) < 0.7) %>%
      summarize(avg = mean(Procedure_count, na.rm = TRUE)) %>%
      pull(avg)
    
    # Storing baseline in reactive object
    analysis_results$baseline_count <- baseline_count
    
    
    # Get lag days selected by the user
    selected_lags <- as.numeric(input$selected_lags)
    if (length(selected_lags) == 0) {
      showNotification("Please select at least one lag day.", type = "error")
      removeModal()
      return(NULL)
    }
    
    # Generate color palette for each selected lag
    n_lags <- length(selected_lags)
    if(n_lags > 8){
      base_colors <- colorRampPalette(brewer.pal(8, "Set3"))(n_lags)
    } else {
      base_colors <- brewer.pal(n_lags, "Set3")
    }
    
    # Generate darker colors for the lines
    line_colors <- sapply(base_colors, darken_color, factor = 0.7)
    
    # Extract temperature values used in prediction grid
    temp_vals <- pred$predvar
    
    # Store lag and color info in reactive values
    analysis_results$selected_lags <- selected_lags
    analysis_results$base_colors <- base_colors
    analysis_results$line_colors <- line_colors
    })
    
    removeModal()
    
    # Show checkboxes for selecting which lag lines will display dots
    output$dotOptions <- renderUI({
      req(analysis_results$selected_lags)
      checkboxGroupInput("dot_lags", "Show dots on lines:",
                         choices = analysis_results$selected_lags,
                         selected = numeric(0))
    })
    
  })
  
  
  overallPlot <- reactive({
    # Ensuring that necessary analysis data is available before plotting
    req(analysis_results$pred, analysis_results$selected_lags, analysis_results$base_colors, analysis_results$line_colors)

    # Dummy input to trigger reactivity when user selects dots
    dummy <- input$dot_lags
    
    # Retrieve reactive values
    selected_lags <- analysis_results$selected_lags
    base_colors <- analysis_results$base_colors
    line_colors <- analysis_results$line_colors
    temp_vals <- analysis_results$pred$predvar
    
    p <- plot_ly()
    
    # Loop over each selected lag (from the analysis)
    for(i in seq_along(selected_lags)) {
      lag <- selected_lags[i]
      lag_index <- lag + 1
      
      # Extract RR estimates and confidence intervals
      rr_fit  <- analysis_results$pred$matRRfit[, lag_index]
      rr_low  <- analysis_results$pred$matRRlow[, lag_index]
      rr_high <- analysis_results$pred$matRRhigh[, lag_index]
      
      # Convert RR to estimated procedure counts
      est_counts <- rr_fit * analysis_results$baseline_count
      
      # Build a string for the confidence interval for hover info
      CI_string <- paste0("(", sprintf("%.2f", rr_low), ", ", sprintf("%.2f", rr_high), ")")
      EST_string <- paste0(sprintf("%.1f", est_counts))
      
      trace_mode <- if (!is.null(input$dot_lags) &&
                        (as.numeric(lag) %in% as.numeric(input$dot_lags))) {
        "lines+markers"
      } else {
        "lines"
      }
      
      # Hovering content
      rr_text <- paste0(
        "Temperature: ", sprintf("%.1f", temp_vals), "°C",
        "<br>Lag: ", lag,
        "<br>RR: ", sprintf("%.2f", rr_fit),
        "<br>CI: ", CI_string,
        "<br>Estimated procedures: ", EST_string
      )
      
      # Add main line trace for this lag
      p <- p %>% add_trace(
        x = temp_vals,
        y = rr_fit,
        type = "scatter",
        mode = trace_mode,
        name = paste("Lag", lag),
        line = list(color = line_colors[i]),
        marker = if(trace_mode == "lines+markers") list(size = 6, color = line_colors[i], symbol = "circle") else NULL,
        text = rr_text,
        hoverinfo = "text"
      )
      
      # Add ribbons for the confidence interval
      p <- p %>% add_ribbons(
        x = temp_vals,
        ymin = rr_low,
        ymax = rr_high,
        name = paste("Lag", lag, "CI"),
        fillcolor = base_colors[i],
        line = list(color = line_colors[i]),
        opacity = 0.2,
        hoverinfo = "skip"
      )
      
      # Layout settings for the overall plot
      p <- p %>% layout(
        title = "Estimated Relative Risk by Temperature for Selected Lags",
        xaxis = list(title = "Temperature (°C)"),
        yaxis = list(title = "Relative Risk"),
        hovermode = "closest",
        hoverlabel = list(
          align = "left",
          font = list(size = 12),
          bgcolor = "white",
          bordercolor = "black"
        )
      )
    }
      
    # Add horizontal line at RR = 1 for reference
    shapes_list <- list(
      list(
        type = "line",
        x0 = min(temp_vals, na.rm = TRUE),
        x1 = max(temp_vals, na.rm = TRUE),
        xref = "x",
        y0 = 1,
        y1 = 1,
        yref = "y",
        line = list(color = "#000000", dash = "solid", width = 1)
      )
    )
    
    # Add threshold line if selected
    annotations_list <- list()
    if (!is.na(input$threshold)) {
      shapes_list <- c(shapes_list, list(
        list(
          type = "line",
          x0 = input$threshold,
          x1 = input$threshold,
          xref = "x",
          y0 = 0,
          y1 = 1,
          yref = "paper",
          line = list(color = "grey", dash = "dash")
        )
      ))
      
      # Annotate threshold line
      annotations_list <- list(
        list(
          x = input$threshold,
          y = 1,
          xref = "x",
          yref = "paper",
          text = paste("Lag Plot Temp:", input$threshold, "°C"),
          showarrow = TRUE,
          arrowhead = 2,
          ax = 0,
          ay = -30,
          font = list(color = "grey"),
          yanchor = "bottom"
        )
      )
    }
    
    # Finalize plot layout
    p <- p %>% layout(
      margin = list(t = 120, b = 50, l = 50, r = 50),
      xaxis = list(
        title = "Temperature (°C)",
        autorange = FALSE,
        range = c(min(temp_vals, na.rm = TRUE), max(temp_vals, na.rm = TRUE)),
        zeroline = FALSE,
        zerolinecolor = "rgba(0,0,0,0)"
      ),
      yaxis = list(
        title = "Relative Risk"
      ),
      shapes = shapes_list,
      annotations = annotations_list,
      hovermode = "closest",
      hoverlabel = list(
        align = "left",
        font = list(size = 12),
        bgcolor = "white",
        bordercolor = "black"
      )
    )
    
    p
  })
    
  
  # Render the main overall plot
  output$interactiveOverallPlot <- renderPlotly({
    overallPlot()
  })
  
  
  output$lagPlot <- renderPlotly({
    # Only run if model has been computed
    req(analysis_results$pred)
    pred <- analysis_results$pred
    baseline_count <- analysis_results$baseline_count
    threshold_value <- as.numeric(input$threshold)
    
    # Validate threshold
    if (is.na(threshold_value) || threshold_value < min(pred$predvar) || threshold_value > max(pred$predvar)) {
      showNotification("Selected threshold is out of prediction range.", type = "warning")
      return(NULL)
    }
    
    # Find index in temperature sequence closest to threshold
    lag_values <- 0:(ncol(pred$matRRfit) - 1)
    temp_index <- which.min(abs(pred$predvar - threshold_value))
    
    rr_vals <- pred$matRRfit[temp_index, ]
    ci_low  <- pred$matRRlow[temp_index, ]
    ci_high <- pred$matRRhigh[temp_index, ]
    est_proc <- round(rr_vals * baseline_count, 1)
    
    # Hover text
    hover_text <- paste0(
      "Lag: ", lag_values,
      "<br>RR: ", sprintf("%.2f", rr_vals),
      "<br>CI: (", sprintf("%.2f", ci_low), ", ", sprintf("%.2f", ci_high), ")",
      "<br>Estimated procedures: ", est_proc
    )
    
    # Build Plotly plot for lag-specific RR
    plot_ly() %>%
      add_trace(
        x = lag_values,
        y = rr_vals,
        type = "scatter",
        mode = "lines+markers",
        name = "Relative Risk",
        line = list(color = "blue"),
        marker = list(size = 6, color = "blue"),
        text = hover_text,
        hoverinfo = "text"
      ) %>%
      add_ribbons(
        x = lag_values,
        ymin = ci_low,
        ymax = ci_high,
        name = "95% CI",
        fillcolor = "blue",
        line = list(color = "blue"),
        opacity = 0.2,
        hoverinfo = "skip"
      ) %>%
      layout(
        title = list(
          text = paste0("Lag-Specific Effect at ", threshold_value, "°C"),
          x = 0.5
        ),
        xaxis = list(title = "Lag (Days)"),
        yaxis = list(title = "Relative Risk"),
        margin = list(t = 80, b = 50, l = 60, r = 50),
        shapes = list(
          list(
            type = "line",
            x0 = min(lag_values),
            x1 = max(lag_values),
            y0 = 1,
            y1 = 1,
            xref = "x",
            yref = "y",
            line = list(color = "black", width = 1, dash = "solid")
          )
        ),
        hoverlabel = list(
          bgcolor = "white",
          bordercolor = "black",
          font = list(size = 12),
          align = "left"
        )
      )
    
  })
  
  
  # A reactive value to cache previously computed heatmaps
  heatmap_cache <- reactiveVal(list())
  
  # Function to compute RR matrix and significance matrix across all selected procedures and hospitals at a given temp and lag
  compute_rr_matrix_data <- function(conn, temp_value, lag_value) {
    top_procs <- get_selected_procedures()
    procedures <- top_procs$procedure_concept_id
    procedure_names <- setNames(top_procs$concept_name, procedures)
    
    # Defining all hospitals to analyze
    hospitals <- c(
      "Tartu Ülikooli Kliinikum SA", "Ida-Tallinna Keskhaigla AS", "Põhja-Eesti Regionaalhaigla SA",
      "Lääne-Tallinna Keskhaigla AS", "Tallinna Lastehaigla SA", "Pärnu Haigla SA",
      "Läänemaa Haigla SA", "Hiiumaa Haigla SA", "Ida-Viru Keskhaigla SA", "Rakvere Haigla AS",
      "Kuressaare Haigla SA", "Narva Haigla SA", "Järvamaa Haigla AS", "Valga Haigla AS",
      "Viljandi Haigla SA", "Lõuna-Eesti Haigla AS", "Jõgeva Haigla SA", "Raplamaa Haigla SA"
    )
    
    # Initializing result matrices
    rr_matrix <- matrix(NA, nrow = length(procedures), ncol = length(hospitals))
    ci_matrix <- matrix(FALSE, nrow = length(procedures), ncol = length(hospitals))
    rownames(rr_matrix) <- procedure_names
    colnames(rr_matrix) <- hospitals
    
    # Load all relevant data in one go
    all_data <- prepare_all_procedure_data(conn, procedures, "2012-01-01", "2019-12-31")
    
    # Loop over each procedure and hospital
    for (i in seq_along(procedures)) {
      proc_id <- procedures[i]
      for (j in seq_along(hospitals)) {
        hosp <- hospitals[j]
        
        # Filter data for current procedure and hospital
        data <- all_data %>%
          filter(procedure_concept_id == proc_id, care_site_name == hosp)
        
        # Only proceed if there is enough data and not all values are NA
        if (nrow(data) >= 20 && !all(is.na(data$Day_temp)) && !all(is.na(data$Procedure_count))) {
          data$Day_temp <- as.numeric(data$Day_temp)
          median_temp <- median(data$Day_temp, na.rm = TRUE)
          
          
          # Build basis matrix
          basis.temp <- crossbasis(data$Day_temp, lag = 10,
                                   argvar = list(fun = "ns", df = 6),
                                   arglag = list(fun = "ns", df = 5))
          
          # Fit quasi-Poisson model
          model <- tryCatch({
            glm(Procedure_count ~ basis.temp, family = quasipoisson(), data = data)
          }, error = function(e) NULL)
          
          # If model succeeds, run prediction
          if (!is.null(model)) {
            pred <- tryCatch({
              crosspred(basis.temp, model, by = 1, cen = median_temp)
            }, error = function(e) NULL)
            
            if (!is.null(pred)) {
              lag_idx <- lag_value + 1
            temp_idx <- which.min(abs(pred$predvar - temp_value))

            # Extract relative risk and confidence interval
            rr <- pred$matRRfit[temp_idx, lag_idx]
            rr_low <- pred$matRRlow[temp_idx, lag_idx]
            rr_high <- pred$matRRhigh[temp_idx, lag_idx]
              
              rr_matrix[i, j] <- round(rr, 2)
              
              # CI significance: TRUE if CI does not include 1
              ci_matrix[i, j] <- !(rr_low <= 1 && rr_high >= 1)
            }
          }
        }
      }
    }
    
    return(list(rr = rr_matrix, sig = ci_matrix))
  }
  
  # Reactive containers
  heatmap_results <- reactiveValues(matrix = NULL)
  heatmap_inputs_used <- reactiveValues(temp = NULL, lag = NULL)
  cached_heatmaps <- reactiveValues(all = list())
  
  # Generate heatmap on button click
  observeEvent(input$generate_heatmap, {
    req(input$heatmap_temp, input$heatmap_lag)
    
    heatmap_inputs_used$temp <- input$heatmap_temp
    heatmap_inputs_used$lag <- input$heatmap_lag
    
    # Build cache key
    key <- paste0("lag", input$heatmap_lag, "_temp", input$heatmap_temp)
    current_cache <- heatmap_cache()
    
    # Check if the result is cached already, if yes, use it
    if (!is.null(current_cache[[key]])) {
      message("Using cached result for: ", key)
      heatmap_results$matrix <- current_cache[[key]]$rr
      heatmap_results$sig <- current_cache[[key]]$sig
      return()
    }
    
    # Show loading modal if not cached
    showModal(modalDialog(
      title = "Generating Heatmap",
      "Please wait while the data is being processed...",
      footer = NULL,
      easyClose = FALSE
    ))
    
    shiny::withProgress(message = 'Running model...', value = 0.5, {
      
      # Clear previous data
      heatmap_results$matrix <- NULL
      heatmap_results$sig <- NULL
      
      # Run computation
      conn <- connect_db()
      result <- compute_rr_matrix_data(conn, input$heatmap_temp, as.numeric(input$heatmap_lag))
      disconnect_db(conn)
      
      # Save result to current output
      heatmap_results$matrix <- result$rr
      heatmap_results$sig <- result$sig
      
      # Save result to cache
      current_cache[[key]] <- result
      heatmap_cache(current_cache)
    })
    
    # Save current result into cache with a label
    key <- paste0("Temp:", input$heatmap_temp, "_Lag:", input$heatmap_lag)
    cached_heatmaps$all[[key]] <- list(
      rr = result$rr,
      sig = result$sig,
      temp = input$heatmap_temp,
      lag = input$heatmap_lag
    )
    
    # Update the checkboxes in the Compare tab
    updateCheckboxGroupInput(session, "selected_cached_heatmaps",
                             choices = names(cached_heatmaps$all))
    
    removeModal()
  })
  
  # Clickable heatmap: auto-fill input and run analysis
  observeEvent(event_data("plotly_click", source = "heatmap_click"), {
    click <- event_data("plotly_click", source = "heatmap_click")
    req(click)
    
    clicked_hospital <- click$x
    clicked_procedure <- click$y
    
    showModal(modalDialog(
      title = "Preparing Analysis",
      "Getting the necessary inputs...",
      footer = NULL,
      easyClose = FALSE
    ))
    
    # Fill form inputs based on clicked cell
    updateNumericInput(session, "threshold", value = input$heatmap_temp)
    updateSelectInput(session, "selected_lags", selected = as.character(input$heatmap_lag))
    
    # Switch to "Plots" tab
    updateTabsetPanel(session, "main_tabs", selected = "Plots")
    
    shinyjs::delay(500, {
      
      updateRadioButtons(session, "procedure_mode", selected = "list")
      
      top_procs <- get_selected_procedures()
      
      concept_id <- top_procs$procedure_concept_id[top_procs$concept_name == clicked_procedure]
      if (length(concept_id) == 1) {
        updateSelectizeInput(session, "procedure_id_select", selected = concept_id)
      }
      
      updateSelectizeInput(session, "hospitals", selected = clicked_hospital)
      
      # Remove modal & trigger analysis
      shinyjs::delay(500, {
        removeModal()  
        click("run_analysis")
      })
    })
  })
  
  
  # Heatmap rendering
  output$heatmap_plot <- renderPlotly({
    req(heatmap_results$matrix, heatmap_results$sig)

    rr_matrix <- heatmap_results$matrix
    sig_matrix <- heatmap_results$sig
    hospitals <- colnames(rr_matrix)
    procedures <- rownames(rr_matrix)
    
    
    # Hover text
    hover_text <- matrix("", nrow = nrow(rr_matrix), ncol = ncol(rr_matrix))
    for (i in seq_len(nrow(rr_matrix))) {
      for (j in seq_len(ncol(rr_matrix))) {
        rr <- rr_matrix[i, j]
        if (is.na(rr)) next
        sig_label <- if (sig_matrix[i, j]) " (Significant)" else ""
        hover_text[i, j] <- paste0(
          "Hospital: ", hospitals[j], "<br>",
          "Procedure: ", procedures[i], "<br>",
          "RR: ", rr, sig_label
        )
      }
    }
    
    # Cap RR visually at 2
    rr_matrix_capped <- rr_matrix
    rr_matrix_capped[rr_matrix_capped > 2] <- 2
    
    # Color scale setup
    zmin <- min(rr_matrix_capped, na.rm = TRUE)
    zmax <- max(rr_matrix_capped, na.rm = TRUE)
    rr_white_pos <- (1.0 - zmin) / (zmax - zmin)
    rr_white_pos <- max(0, min(1, rr_white_pos))
    
    tick_vals <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0)
    tick_text <- c("0", "0.2", "0.4", "0.6", "0.8", "1", "1.2", "1.4", "1.6", "1.8", "2+")
    
    # Plot
    p <- plot_ly(
      x = hospitals,
      y = procedures,
      z = rr_matrix_capped,
      type = "heatmap",
      source = "heatmap_click",
      colorscale = list(
        c(0.00, "#2166ac"),
        c(rr_white_pos - 0.07, "#a6c8e0"),
        c(rr_white_pos - 0.03, "#dbe8ef"),
        c(rr_white_pos, "#ffffff"),
        c(rr_white_pos + 0.03, "#f0c8b0"),
        c(rr_white_pos + 0.07, "#f4a582"),
        c(1.00, "#b2182b")
      ),
      colorbar = list(
        title = "RR",
        tickvals = tick_vals,
        ticktext = tick_text
      ),
      text = hover_text,
      hoverinfo = "text"
    )
    
    event_register(p, "plotly_click")
    
    # Asterisk annotations (only if significant AND matches RR direction)
    annotations <- list()
    for (i in seq_len(nrow(sig_matrix))) {
      for (j in seq_len(ncol(sig_matrix))) {
        rr <- rr_matrix[i, j]
        is_sig <- sig_matrix[i, j]
        
        if (is_sig &&
            ((input$show_sig_rr_gt_1 && rr > 1) ||
             (input$show_sig_rr_lt_1 && rr < 1))) {
          annotations <- append(annotations, list(list(
            x = hospitals[j],
            y = procedures[i],
            text = "*",
            showarrow = FALSE,
            font = list(color = "black", size = 16, family = "Arial"),
            xanchor = "center",
            yanchor = "middle"
          )))
        }
      }
    }
    
    
    p %>% layout(
        title = list(
          text = paste("Relative Risk by Hospital and Procedure",
                       "<br><sub>Temp:", heatmap_inputs_used$temp, "°C | Lag:", heatmap_inputs_used$lag, "</sub>"),
          x = 0.5
        ),
        margin = list(t = 120, b = 60, l = 100, r = 60),
        xaxis = list(title = "Hospital"),
        yaxis = list(title = "Procedure", automargin = TRUE),
        annotations = annotations
      )
  })

  
  # Dynamically render UI to display multiple cached heatmaps side-by-side
  output$compare_heatmap_output <- renderUI({
    req(input$selected_cached_heatmaps)
    
    # List of keys corresponding to selected cached heatmaps
    selected <- input$selected_cached_heatmaps
    
    # Add inline CSS styles for responsive heatmap layout
    div(
      class = "container-fluid",
      tags$style(HTML("
      .heatmap-compare-wrapper {
        display: flex;
        flex-wrap: wrap;
        justify-content: center;
      }
      .heatmap-col {
        flex: 0 0 48%;
        max-width: 48%;
        min-width: 800px;
        box-sizing: border-box;
        padding: 10px;
      }
      @media (max-width: 1650px) {
        .heatmap-col {
          flex: 0 0 100%;
          max-width: 100%;
        }
      }
    ")),
      
      # Generate one plotlyOutput per selected heatmap
      div(
        class = "heatmap-compare-wrapper",
        lapply(selected, function(key) {
          safe_id <- gsub("[^A-Za-z0-9]", "_", key)
          div(
            class = "heatmap-col",
            div(
              style = "border: 1px solid #ccc; padding: 10px; background-color: #f9f9f9;",
              plotlyOutput(
                outputId = paste0("heatmap_compare_", safe_id),
                height = "1000px"
              )
            )
          )
        })
      )
    )
  })
  
  
  # For each selected heatmap key, register a corresponding output renderer
  observe({
    req(input$selected_cached_heatmaps)
    selected <- input$selected_cached_heatmaps
    
    for (key in selected) {
      local({
        this_key <- key
        safe_id <- gsub("[^A-Za-z0-9]", "_", this_key)
        output[[paste0("heatmap_compare_", safe_id)]] <- renderPlotly({
          data <- cached_heatmaps$all[[this_key]]
          
          # Generate the individual heatmap using the helper function
          create_heatmap_plot(
            rr_matrix = data$rr,
            sig_matrix = data$sig,
            temp = data$temp,
            lag = data$lag,
            show_gt_1 = input$show_sig_rr_gt_1,
            show_lt_1 = input$show_sig_rr_lt_1
          )
          
        })
      })
    }
  })
  
  # Generates one individual heatmap for given RR matrix and significance matrix
  create_heatmap_plot <- function(rr_matrix, sig_matrix, temp, lag,
                                  show_gt_1 = TRUE, show_lt_1 = TRUE) {
    hospitals <- colnames(rr_matrix)
    procedures <- rownames(rr_matrix)
    
    # Cap values greater than 2 to prevent outliers from distorting color scale
    rr_matrix_capped <- rr_matrix
    rr_matrix_capped[rr_matrix_capped > 2] <- 2
    
    # Calculate color scale midpoint position
    zmin <- min(rr_matrix_capped, na.rm = TRUE)
    zmax <- max(rr_matrix_capped, na.rm = TRUE)
    rr_white_pos <- max(0, min(1, (1.0 - zmin) / (zmax - zmin)))
    
    # Hover text
    hover_text <- matrix("", nrow = nrow(rr_matrix), ncol = ncol(rr_matrix))
    for (i in seq_len(nrow(rr_matrix))) {
      for (j in seq_len(ncol(rr_matrix))) {
        rr <- rr_matrix[i, j]
        if (!is.na(rr)) {
          hover_text[i, j] <- paste0("Hospital: ", hospitals[j],
                                     "<br>Procedure: ", procedures[i],
                                     "<br>RR: ", rr)
        }
      }
    }
    
    
    
    # Add * to cells where CI does not include 1 and meets user filter
    annotations <- list()
    for (i in seq_len(nrow(sig_matrix))) {
      for (j in seq_len(ncol(sig_matrix))) {
        rr <- rr_matrix[i, j]
        is_sig <- sig_matrix[i, j]
        
        if (is_sig &&
            ((show_gt_1 && rr > 1) || (show_lt_1 && rr < 1))) {
          annotations <- append(annotations, list(list(
            x = hospitals[j],
            y = procedures[i],
            text = "*",
            showarrow = FALSE,
            font = list(color = "black", size = 16),
            xanchor = "center",
            yanchor = "middle"
          )))
        }
      }
    }
    
    # Create and return heatmap Plotly object
    p <- plot_ly(
      x = hospitals,
      y = procedures,
      z = rr_matrix_capped,
      type = "heatmap",
      colorscale = list(
        c(0.00, "#2166ac"),
        c(rr_white_pos - 0.07, "#a6c8e0"),
        c(rr_white_pos - 0.03, "#dbe8ef"),
        c(rr_white_pos, "#ffffff"),
        c(rr_white_pos + 0.03, "#f0c8b0"),
        c(rr_white_pos + 0.07, "#f4a582"),
        c(1.00, "#b2182b")
      ),
      colorbar = list(title = "RR"),
      text = hover_text,
      hoverinfo = "text"
    ) 
    p %>% layout(
        title = paste0("Temp: ", temp, "°C | Lag: ", lag),
        xaxis = list(title = "Hospital", tickangle = -45, automargin = TRUE),
        yaxis = list(title = "Procedure", automargin = TRUE),
        annotations = annotations,
        margin = list(t = 80, b = 80, l = 100, r = 100)
      )
  }
  
}
