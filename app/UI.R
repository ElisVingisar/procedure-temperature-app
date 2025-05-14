# --- UI.R ---

library(shiny)
library(plotly)
library(shinyjs)


# Defining the UI layout
ui <- fluidPage(
  useShinyjs(),
  
  # App title
  titlePanel("DLNM Temperature and Procedure Analysis"),
  
  # Main navigation tabs
  tabsetPanel(id = "main_tabs",
    
              # ---- Heatmap tap ----
    tabPanel("Heatmap",
         sidebarLayout(
           sidebarPanel(
             
             # Generate sub-tab
             conditionalPanel(
               condition = "input.heatmap_subtab == 'Generate'",
               numericInput("heatmap_temp", "Temperature:", 20),
               selectInput("heatmap_lag", "Lag (days):", choices = 0:10),
               checkboxInput("show_sig_rr_gt_1", "Show significant values for RR > 1", TRUE),
               checkboxInput("show_sig_rr_lt_1", "Show significant values for RR < 1", TRUE),
               actionButton("generate_heatmap", "Generate heatmap")
             ),
             
             # Compare sub-tab
             conditionalPanel(
               condition = "input.heatmap_subtab == 'Compare'",
               tags$label("Select heatmaps to compare:"),
               checkboxGroupInput("selected_cached_heatmaps", NULL, choices = NULL),
               tags$label("Filter significant values: "),
               checkboxInput("show_sig_rr_gt_1", "Show significant values for RR > 1", TRUE),
               checkboxInput("show_sig_rr_lt_1", "Show significant values for RR < 1", TRUE)
             )
           ),
           
           # Main panel for Generate and Compare views
           mainPanel(
             tabsetPanel(
               id = "heatmap_subtab",
               tabPanel("Generate",
                        plotlyOutput("heatmap_plot", height = "1000px")
               ),
               tabPanel("Compare",
                        uiOutput("compare_heatmap_output")
               )
             )
           )
         )
    ),
    
    # ---- Plots tab ----
    tabPanel("Plots",
         sidebarLayout(
           sidebarPanel(
             
             # Hospital selection grouped by city
             selectizeInput(
               inputId = "hospitals",
               label = "Select hospitals:",
               choices = NULL,
               multiple = TRUE,
               options = list(
                 placeholder = 'Select one or more hospitals',
                 optgroupField = 'city',
                 labelField = 'label',
                 valueField = 'value',
                 searchField = 'label'
               )
             ),
             
             # Mode toggle: list or manual entry of procedure
             radioButtons(
               inputId = "procedure_mode", 
               label = "Select procedure input mode:",
               choices = c("Choose from list" = "list", "Enter concept ID manually" = "manual"),
               selected = "list"
             ),
             
             # Manual procedure concept ID input
             conditionalPanel(
               condition = "input.procedure_mode == 'manual'",
               textInput(
                 inputId = "procedure_id_manual",
                 label = "Enter procedure concept ID:",
                 placeholder = "e.g. 4353741"
               )
             ),
             
             # Procedure dropdown list
             conditionalPanel(
               condition = "input.procedure_mode == 'list'",
               selectizeInput(
                 inputId = "procedure_id_select",
                 label = "Select a procedure:",
                 choices = NULL,
                 options = list(placeholder = 'Choose from list')
               )
             ),
             
             # Lag day selection for plot display
             selectInput("selected_lags", 
                         "Select lag days (0 to 10):", 
                         choices = 0:10, 
                         selected = NULL,
                         multiple = TRUE),
             
             # Temperature value input for the lag plot
             numericInput("threshold", "Temperature value for lag plot (°C)", value = NA),
             actionButton("run_analysis", "Run analysis"),
             div(style = "margin-top: 15px;", uiOutput("dotOptions"))
           ),
           
           # Main panel for interactive plots
           mainPanel(
             plotlyOutput("interactiveOverallPlot"),
             br(),
             div(style = "margin-top: 80px;", plotlyOutput("lagPlot"))
           )
         )
    )
  )
)
