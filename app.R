# ==============================================================================
# CardioPredict: Cardiovascular Disease Occurrence Prediction and Visualization Dashboard
# File: app.R
# Technology: R and Shiny with bslib, plotly, dplyr, tidyr, randomForest, e1071
# Run with: shiny::runApp()
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(plotly)
  library(dplyr)
  library(tidyr)
  library(readr)
})

# Source modular R files
source("R/utils.R")
source("R/data_processing.R")
source("R/age_prevalence.R")
source("R/risk_factor_heatmap.R")
source("R/feature_importance.R")
source("R/model_evaluation.R")
source("R/socioeconomic_analysis.R")

# Custom Light Theme: Navy, Teal, and Slate
cardiopredict_theme <- bs_theme(
  version = 5,
  bootswatch = "minty",
  primary = "#0284c7",      # Slate/Blue
  secondary = "#0d9488",    # Teal
  success = "#10b981",
  info = "#0284c7",
  warning = "#f59e0b",
  danger = "#e11d48",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter"),
  code_font = font_google("Fira Code"),
  bg = "#ffffff",
  fg = "#0f172a"
)

# ==============================================================================
# UI DEFINITION
# ==============================================================================
ui <- page_sidebar(
  theme = cardiopredict_theme,
  title = div(
    class = "d-flex align-items-center justify-content-between w-100",
    div(
      span(style = "font-weight: 700; color: #0f172a; font-size: 1.25rem;", "CardioPredict"),
      span(style = "color: #64748b; font-size: 0.95rem; margin-left: 0.5rem;", "— Cardiovascular Disease Prediction & Visualization")
    ),
    div(
      class = "d-flex align-items-center gap-2",
      uiOutput("dataset_badge_ui"),
      actionButton("btn_open_settings", "Data & Settings", icon = icon("sliders"), class = "btn-outline-primary btn-sm")
    )
  ),
  
  # ----------------------------------------------------------------------------
  # LEFT SIDEBAR NAVIGATION
  # ----------------------------------------------------------------------------
  sidebar = sidebar(
    width = 280,
    div(
      style = "padding: 0.5rem 0;",
      h6(style = "text-transform: uppercase; letter-spacing: 0.05em; color: #64748b; font-size: 0.75rem; font-weight: 700; margin-bottom: 0.75rem;", "Core Visualizations"),
      
      radioButtons(
        inputId = "nav_page",
        label = NULL,
        choices = c(
          "1. Age Prevalence" = "page_age",
          "2. Risk Factor Heatmap" = "page_risk",
          "3. Model Explainability" = "page_importance",
          "4. Model Evaluation" = "page_evaluation",
          "5. Socioeconomic Analysis" = "page_socioeconomic"
        ),
        selected = "page_age",
        width = "100%"
      ),
      
      hr(style = "margin: 1.25rem 0; border-color: #e2e8f0;"),
      
      # Dataset Information Panel (Record count, target definition, data source)
      div(
        class = "card p-3 border-0 bg-light shadow-none",
        style = "border-radius: 8px; font-size: 0.85rem;",
        h6(style = "font-weight: 600; font-size: 0.85rem; color: #334155; margin-bottom: 0.5rem;", icon("info-circle"), " Dataset Info"),
        uiOutput("sidebar_dataset_info")
      )
    )
  ),
  
  # ----------------------------------------------------------------------------
  # MAIN BODY: FIVE NAVIGATION PAGES
  # ----------------------------------------------------------------------------
  tags$head(
    tags$style(HTML("
      body { background-color: #f8fafc; font-family: 'Inter', sans-serif; }
      .finding-callout {
        background: #f0fdf4;
        border-left: 4px solid #10b981;
        padding: 0.85rem 1.15rem;
        border-radius: 6px;
        margin-top: 1rem;
        font-size: 0.925rem;
        color: #064e3b;
      }
      .finding-callout.warning {
        background: #fffbeb;
        border-left-color: #f59e0b;
        color: #78350f;
      }
      .research-badge {
        background: #e0f2fe;
        color: #0369a1;
        font-weight: 600;
        font-size: 0.8rem;
        padding: 0.35rem 0.65rem;
        border-radius: 4px;
        display: inline-block;
        margin-bottom: 0.5rem;
      }
    "))
  ),
  
  # Conditionally rendered pages (1 through 5)
  uiOutput("page_content_ui")
)

# ==============================================================================
# SERVER LOGIC
# ==============================================================================
server <- function(input, output, session) {
  
  # Reactive values for dataset state and models
  rv <- reactiveValues(
    active_dataset = "kaggle", # "kaggle" or "brfss"
    kaggle_processed = NULL,
    brfss_processed = NULL,
    model_results = NULL,
    is_training = FALSE,
    training_status = "Not initiated"
  )
  
  # Initialize sample datasets if available
  observe({
    kaggle_path <- if (file.exists("data/cardio_train.csv")) {
      "data/cardio_train.csv"
    } else if (file.exists("data/cardio_train_sample.csv")) {
      "data/cardio_train_sample.csv"
    } else {
      NULL
    }
    
    if (!is.null(kaggle_path) && is.null(rv$kaggle_processed)) {
      tryCatch({
        rv$kaggle_processed <- preprocess_kaggle_data(kaggle_path)
      }, error = function(e) {
        message("Notice: Default Kaggle data not loaded: ", e$message)
      })
    }
    
    brfss_path <- if (file.exists("data/brfss2015.csv")) {
      "data/brfss2015.csv"
    } else if (file.exists("data/brfss2015_sample.csv")) {
      "data/brfss2015_sample.csv"
    } else if (file.exists("data/LLCP2015.XPT")) {
      "data/LLCP2015.XPT"
    } else {
      NULL
    }
    
    if (!is.null(brfss_path) && is.null(rv$brfss_processed)) {
      tryCatch({
        rv$brfss_processed <- preprocess_brfss_data(brfss_path)
      }, error = function(e) {
        message("Notice: Default BRFSS data not loaded: ", e$message)
      })
    }
  })
  
  # Active dataset getter
  current_data <- reactive({
    if (rv$active_dataset == "kaggle") {
      rv$kaggle_processed$data
    } else {
      rv$brfss_processed$data
    }
  })
  
  # Header Badge
  output$dataset_badge_ui <- renderUI({
    ds_name <- if (rv$active_dataset == "kaggle") "Kaggle Dataset (cardio_train)" else "CDC BRFSS 2015"
    has_data <- !is.null(current_data()) && nrow(current_data()) > 0
    
    div(
      class = "d-flex align-items-center gap-2",
      span(
        class = paste0("badge ", if (has_data) "bg-success" else "bg-warning text-dark"),
        style = "font-size: 0.85rem; font-weight: 500;",
        if (has_data) icon("check-circle") else icon("exclamation-circle"),
        paste0(" ", ds_name, if (has_data) " (Active)" else " (Empty)")
      )
    )
  })
  
  # Sidebar Dataset Info Panel
  output$sidebar_dataset_info <- renderUI({
    df <- current_data()
    ds_type <- rv$active_dataset
    
    if (is.null(df) || nrow(df) == 0) {
      return(p(class = "text-muted mb-0", "No dataset currently active. Use 'Data & Settings' to upload."))
    }
    
    meta <- if (ds_type == "kaggle") rv$kaggle_processed else rv$brfss_processed
    
    tagList(
      div(strong("Source: "), meta$source),
      div(strong("Records: "), formatC(nrow(df), format = "d", big.mark = ",")),
      div(strong("Excluded: "), formatC(meta$excluded_n, format = "d", big.mark = ","), " invalid/missing"),
      div(strong("Target: "), if (ds_type == "kaggle") "cardio (0/1)" else "CVD Composite (Heart Attack/CAD)")
    )
  })
  
  # ----------------------------------------------------------------------------
  # DATA & SETTINGS MODAL
  # ----------------------------------------------------------------------------
  observeEvent(input$btn_open_settings, {
    showModal(modalDialog(
      title = div(icon("sliders"), " Data Import, Preprocessing & Model Configuration"),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close"),
      
      div(
        class = "container-fluid p-0",
        h6(style = "font-weight: 600;", "1. Select Active Dataset"),
        radioButtons(
          "modal_select_dataset",
          label = NULL,
          choices = c(
            "Kaggle Cardiovascular Disease Dataset (cardio_train.csv)" = "kaggle",
            "CDC BRFSS 2015 Dataset (XPT or CSV)" = "brfss"
          ),
          selected = rv$active_dataset
        ),
        hr(),
        
        h6(style = "font-weight: 600;", "2. Upload Dataset File"),
        p(class = "text-muted small", "Upload official cardio_train.csv (semicolon or comma delimited) or BRFSS 2015 XPT/CSV."),
        fileInput("file_upload_input", "Choose File", accept = c(".csv", ".xpt")),
        
        hr(),
        h6(style = "font-weight: 600;", "3. Machine Learning Training Pipeline"),
        p(class = "text-muted small", "Train Logistic Regression, Random Forest, and SVM models on the active dataset (stratified 80/20 train/test split)."),
        div(
          class = "d-flex align-items-center gap-3",
          actionButton("btn_train_models", "Train All 3 Models", icon = icon("play"), class = "btn-primary"),
          span(textOutput("training_status_text"), class = "text-muted small")
        )
      )
    ))
  })
  
  # Handle Dataset Switching
  observeEvent(input$modal_select_dataset, {
    rv$active_dataset <- input$modal_select_dataset
  })
  
  # Handle File Upload
  observeEvent(input$file_upload_input, {
    req(input$file_upload_input)
    fpath <- input$file_upload_input$datapath
    fname <- input$file_upload_input$name
    
    withProgress(message = "Processing uploaded dataset...", value = 0.5, {
      if (grepl("cardio", fname, ignore.case = TRUE) || rv$active_dataset == "kaggle") {
        tryCatch({
          res <- preprocess_kaggle_data(fpath)
          rv$kaggle_processed <- res
          rv$active_dataset <- "kaggle"
          showNotification(paste("Successfully imported Kaggle dataset:", nrow(res$data), "clean records."), type = "message")
        }, error = function(e) {
          showNotification(paste("Kaggle import error:", e$message), type = "error")
        })
      } else {
        tryCatch({
          res <- preprocess_brfss_data(fpath)
          rv$brfss_processed <- res
          rv$active_dataset <- "brfss"
          showNotification(paste("Successfully imported CDC BRFSS dataset:", nrow(res$data), "records."), type = "message")
        }, error = function(e) {
          showNotification(paste("BRFSS import error:", e$message), type = "error")
        })
      }
    })
  })
  
  # Handle Model Training
  observeEvent(input$btn_train_models, {
    df <- current_data()
    req(df)
    
    rv$is_training <- TRUE
    rv$training_status <- "Training models..."
    
    withProgress(message = "Training Machine Learning Models (Stratified 80/20)...", detail = "Fitting models...", value = 0.2, {
      tryCatch({
        results <- train_and_evaluate_models(df, target_var = "cardio_num", seed = 42)
        rv$model_results <- results
        rv$training_status <- paste0("Models trained on ", results$train_n, " rows; tested on ", results$test_n, " rows.")
        showNotification("All 3 models trained and evaluated on held-out test cohort!", type = "message")
      }, error = function(e) {
        rv$training_status <- paste("Training error:", e$message)
        showNotification(paste("Model training failed:", e$message), type = "error")
      })
    })
    
    rv$is_training <- FALSE
  })
  
  output$training_status_text <- renderText({
    rv$training_status
  })
  
  # ----------------------------------------------------------------------------
  # PAGE ROUTER
  # ----------------------------------------------------------------------------
  output$page_content_ui <- renderUI({
    selected_page <- input$nav_page
    
    if (selected_page == "page_age") {
      # ------------------------------------------------------------------------
      # PAGE 1: AGE PREVALENCE (Contains only Visualization 1)
      # ------------------------------------------------------------------------
      div(
        class = "container-fluid py-3",
        div(
          class = "card border-0 shadow-sm p-4 bg-white",
          div(class = "research-badge", "Research Question 1"),
          h4(style = "font-weight: 700; color: #0f172a; margin-bottom: 0.25rem;", "Which age groups have the highest recorded cardiovascular disease prevalence?"),
          p(class = "text-muted", "Examining recorded cardiovascular disease prevalence across ascending standardized age cohorts."),
          
          div(style = "margin-top: 1rem; min-height: 480px;", plotlyOutput("viz1_age_plot", height = "480px")),
          
          div(class = "finding-callout", htmlOutput("viz1_finding_text"))
        )
      )
      
    } else if (selected_page == "page_risk") {
      # ------------------------------------------------------------------------
      # PAGE 2: RISK FACTOR ANALYSIS (Contains only Visualization 2)
      # ------------------------------------------------------------------------
      div(
        class = "container-fluid py-3",
        div(
          class = "card border-0 shadow-sm p-4 bg-white",
          div(class = "research-badge", "Research Question 2"),
          h4(style = "font-weight: 700; color: #0f172a; margin-bottom: 0.25rem;", "How does disease prevalence vary across cardiovascular risk factors?"),
          p(class = "text-muted", "Cross-tabulated interactive heatmap of recorded cardiovascular disease prevalence across clinical and behavioral risk factor strata."),
          
          div(
            class = "row g-3 my-2",
            div(
              class = "col-md-6",
              selectInput(
                "viz2_factor_x",
                "Primary Risk Factor (X-Axis):",
                choices = c(
                  "Serum Cholesterol Category" = "cholesterol_cat",
                  "Fasting Glucose Category" = "gluc_cat",
                  "Smoking Status" = "smoke_cat",
                  "Physical Activity" = "active_cat",
                  "Body Mass Index (BMI) Category" = "bmi_cat"
                ),
                selected = "cholesterol_cat"
              )
            ),
            div(
              class = "col-md-6",
              selectInput(
                "viz2_factor_y",
                "Stratifying Risk Factor (Y-Axis):",
                choices = c(
                  "Blood Pressure Category (AHA/ACC)" = "bp_category",
                  "Age Band" = "age_group",
                  "Physical Activity" = "active_cat",
                  "Smoking Status" = "smoke_cat"
                ),
                selected = "bp_category"
              )
            )
          ),
          
          div(style = "margin-top: 1rem; min-height: 480px;", plotlyOutput("viz2_risk_plot", height = "480px")),
          
          div(class = "finding-callout", htmlOutput("viz2_finding_text"))
        )
      )
      
    } else if (selected_page == "page_importance") {
      # ------------------------------------------------------------------------
      # PAGE 3: MODEL EXPLAINABILITY (Contains only Visualization 3)
      # ------------------------------------------------------------------------
      div(
        class = "container-fluid py-3",
        div(
          class = "card border-0 shadow-sm p-4 bg-white",
          div(class = "research-badge", "Research Question 3"),
          h4(style = "font-weight: 700; color: #0f172a; margin-bottom: 0.25rem;", "Which features contribute most to the machine learning model's predictions?"),
          p(class = "text-muted", "Feature importance ranked by Random Forest Mean Decrease in Gini Impurity (excluding target and IDs)."),
          
          div(style = "margin-top: 1rem; min-height: 480px;", plotlyOutput("viz3_importance_plot", height = "480px")),
          
          div(class = "finding-callout", htmlOutput("viz3_finding_text"))
        )
      )
      
    } else if (selected_page == "page_evaluation") {
      # ------------------------------------------------------------------------
      # PAGE 4: MODEL EVALUATION (Contains only Visualization 4 + Individual Prediction Form)
      # ------------------------------------------------------------------------
      div(
        class = "container-fluid py-3",
        div(
          class = "card border-0 shadow-sm p-4 bg-white",
          div(class = "research-badge", "Research Question 4"),
          h4(style = "font-weight: 700; color: #0f172a; margin-bottom: 0.25rem;", "Which machine learning model best distinguishes the recorded cardiovascular disease outcome?"),
          p(class = "text-muted", "Multi-model Receiver Operating Characteristic (ROC) curve comparison on the 20% held-out test set."),
          
          div(style = "margin-top: 1rem; min-height: 480px;", plotlyOutput("viz4_roc_plot", height = "480px")),
          
          div(class = "finding-callout", htmlOutput("viz4_finding_text")),
          
          hr(style = "margin: 2rem 0; border-color: #e2e8f0;"),
          
          # Optional individual prediction form (Permitted in Section 7 without introducing a 6th chart)
          div(
            class = "p-3 bg-light rounded",
            h6(style = "font-weight: 600;", icon("calculator"), " Individual Risk Factor Estimation (Model Prediction)"),
            p(class = "text-muted small", "Enter patient clinical values to query the trained Random Forest model for estimated outcome probability. (Does not predict future incidence)."),
            div(
              class = "row g-3",
              div(class = "col-md-2", numericInput("pred_age", "Age (Years)", value = 55, min = 18, max = 100)),
              div(class = "col-md-2", numericInput("pred_sbp", "Systolic BP (mmHg)", value = 135, min = 70, max = 220)),
              div(class = "col-md-2", numericInput("pred_dbp", "Diastolic BP (mmHg)", value = 85, min = 40, max = 150)),
              div(class = "col-md-2", numericInput("pred_bmi", "BMI (kg/m²)", value = 27.5, min = 14, max = 55, step = 0.5)),
              div(class = "col-md-2", selectInput("pred_chol", "Cholesterol", choices = c("Normal" = 1, "Above Normal" = 2, "Well Above Normal" = 3))),
              div(class = "col-md-2", selectInput("pred_smoke", "Smoking", choices = c("No" = 0, "Yes" = 1)))
            ),
            div(
              class = "mt-3 d-flex align-items-center gap-3",
              actionButton("btn_predict_individual", "Calculate Estimated Probability", class = "btn-secondary btn-sm"),
              uiOutput("pred_result_ui")
            )
          )
        )
      )
      
    } else if (selected_page == "page_socioeconomic") {
      # ------------------------------------------------------------------------
      # PAGE 5: SOCIOECONOMIC ANALYSIS (Contains only Visualization 5)
      # ------------------------------------------------------------------------
      div(
        class = "container-fluid py-3",
        div(
          class = "card border-0 shadow-sm p-4 bg-white",
          div(class = "research-badge", "Research Question 5"),
          h4(style = "font-weight: 700; color: #0f172a; margin-bottom: 0.25rem;", "How does recorded cardiovascular disease prevalence vary across income and education groups?"),
          p(class = "text-muted", "Evaluating disparities across socioeconomic gradients using the CDC BRFSS 2015 dataset with survey weights and 95% confidence intervals."),
          
          div(
            class = "row g-3 my-2",
            div(
              class = "col-md-4",
              selectInput(
                "viz5_factor_type",
                "Socioeconomic Dimension:",
                choices = c("Household Income Level" = "income", "Educational Attainment" = "education"),
                selected = "income"
              )
            ),
            div(
              class = "col-md-4",
              checkboxInput("viz5_use_weights", "Apply CDC Survey Weights (_LLCPWT)", value = TRUE)
            )
          ),
          
          div(style = "margin-top: 1rem; min-height: 480px;", plotlyOutput("viz5_socio_plot", height = "480px")),
          
          div(class = "finding-callout", htmlOutput("viz5_finding_text"))
        )
      )
    }
  })
  
  # ----------------------------------------------------------------------------
  # VIZ 1: AGE PREVALENCE OUTPUTS
  # ----------------------------------------------------------------------------
  viz1_stats <- reactive({
    df <- current_data()
    req(df)
    compute_age_prevalence(df, use_weights = (rv$active_dataset == "brfss"))
  })
  
  output$viz1_age_plot <- renderPlotly({
    df <- current_data()
    if (is.null(df) || nrow(df) == 0) {
      return(plotly_empty(type = "scatter", mode = "text") %>% layout(title = "No data loaded."))
    }
    stats <- viz1_stats()
    ds_label <- if (rv$active_dataset == "kaggle") "Kaggle Cardiovascular Cohort" else "CDC BRFSS 2015 Survey"
    render_age_prevalence_plot(stats, dataset_name = ds_label)
  })
  
  output$viz1_finding_text <- renderUI({
    stats <- viz1_stats()
    HTML(generate_age_prevalence_finding(stats))
  })
  
  # ----------------------------------------------------------------------------
  # VIZ 2: RISK FACTOR HEATMAP OUTPUTS
  # ----------------------------------------------------------------------------
  viz2_data <- reactive({
    df <- current_data()
    req(df, input$viz2_factor_x, input$viz2_factor_y)
    compute_risk_heatmap_data(df, factor_x = input$viz2_factor_x, factor_y = input$viz2_factor_y)
  })
  
  output$viz2_risk_plot <- renderPlotly({
    hdata <- viz2_data()
    x_name <- input$viz2_factor_x
    y_name <- input$viz2_factor_y
    render_risk_heatmap_plot(hdata, title_x = x_name, title_y = y_name)
  })
  
  output$viz2_finding_text <- renderUI({
    hdata <- viz2_data()
    HTML(generate_risk_heatmap_finding(hdata, title_x = input$viz2_factor_x, title_y = input$viz2_factor_y))
  })
  
  # ----------------------------------------------------------------------------
  # VIZ 3: FEATURE IMPORTANCE OUTPUTS
  # ----------------------------------------------------------------------------
  viz3_importance_data <- reactive({
    req(rv$model_results$models$rf)
    extract_feature_importance(rv$model_results$models$rf)
  })
  
  output$viz3_importance_plot <- renderPlotly({
    if (is.null(rv$model_results)) {
      return(
        plotly_empty(type = "scatter", mode = "text") %>%
          layout(title = list(text = "Models not yet trained. Click 'Data & Settings' -> 'Train All 3 Models'."))
      )
    }
    imp_df <- viz3_importance_data()
    render_feature_importance_plot(imp_df, model_name = "Random Forest (Kaggle Predictors)")
  })
  
  output$viz3_finding_text <- renderUI({
    if (is.null(rv$model_results)) {
      return(HTML("Random Forest model has not been trained yet. Initiate training via 'Data & Settings'."))
    }
    imp_df <- viz3_importance_data()
    HTML(generate_feature_importance_statement(imp_df))
  })
  
  # ----------------------------------------------------------------------------
  # VIZ 4: MODEL EVALUATION (ROC) OUTPUTS
  # ----------------------------------------------------------------------------
  output$viz4_roc_plot <- renderPlotly({
    render_model_roc_plot(rv$model_results)
  })
  
  output$viz4_finding_text <- renderUI({
    HTML(generate_model_evaluation_statement(rv$model_results))
  })
  
  # Individual prediction form
  observeEvent(input$btn_predict_individual, {
    if (is.null(rv$model_results$models$rf)) {
      showNotification("Please train the machine learning models first.", type = "warning")
      return()
    }
    
    new_obs <- tibble(
      age_years = as.numeric(input$pred_age),
      ap_hi = as.numeric(input$pred_sbp),
      ap_lo = as.numeric(input$pred_dbp),
      bmi = as.numeric(input$pred_bmi),
      cholesterol = as.numeric(input$pred_chol),
      gluc = 1,
      smoke = as.numeric(input$pred_smoke),
      alco = 0,
      active = 1
    )
    
    # Predict with Random Forest
    rf_mod <- rv$model_results$models$rf
    prob <- predict(rf_mod, newdata = new_obs, type = "prob")[, "1"]
    
    output$pred_result_ui <- renderUI({
      pct <- round(prob * 100, 1)
      badge_class <- if (pct < 30) "bg-success" else if (pct < 60) "bg-warning text-dark" else "bg-danger"
      span(
        class = paste("badge p-2", badge_class),
        style = "font-size: 0.95rem;",
        icon("stethoscope"), paste0(" Estimated Probability of Recorded CVD: ", pct, "%")
      )
    })
  })
  
  # ----------------------------------------------------------------------------
  # VIZ 5: SOCIOECONOMIC DISPARITIES OUTPUTS
  # ----------------------------------------------------------------------------
  viz5_data <- reactive({
    b_df <- rv$brfss_processed$data
    if (is.null(b_df) || nrow(b_df) == 0) return(NULL)
    compute_socioeconomic_stats(b_df, factor_type = input$viz5_factor_type, use_weights = input$viz5_use_weights)
  })
  
  output$viz5_socio_plot <- renderPlotly({
    stats <- viz5_data()
    render_socioeconomic_plot(stats, factor_type = input$viz5_factor_type)
  })
  
  output$viz5_finding_text <- renderUI({
    stats <- viz5_data()
    HTML(generate_socioeconomic_finding(stats, factor_type = input$viz5_factor_type))
  })
}

# ==============================================================================
# RUN APP
# ==============================================================================
shinyApp(ui = ui, server = server)
