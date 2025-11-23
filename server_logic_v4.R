# server_logic_v4.R - Server logic for D-value Meta-Analyzer application v3 (version modifiée)
# Modifications principales:
# - Ajout d'IC (95%/99%) pour les prédictions (base/heuristic/truncated/mixed)
# - Ajout de diagnostics de fit par Microorganism x Publication
# - Export Excel enrichi (feuilles Diagnostics et Model_Info)

library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(tidyr)
library(openxlsx)

server <- function(input, output, session) {
  all_data <- reactiveVal(NULL)
  predictions_all <- reactiveVal(NULL)
  log_text <- reactiveVal("")
  mixed_model_info <- reactiveVal("")
  
  # Palette de couleurs étendue pour tous les types de microorganismes
  colors <- c(
    "Bacteria" = "blue",
    "non-enveloped virus" = "red",
    "Enveloped virus" = "darkred",
    "Bacterial spores" = "green",
    "Yeast" = "orange",
    "Yeast_ascospores" = "darkorange",
    "Moulds" = "purple",
    "Mould_ascospores" = "darkmagenta"
  )
  
  add_log <- function(msg) {
    log_text(paste(log_text(), paste(Sys.time(), "—", msg), sep = "\n"))
  }
  
  # Notifications standard
  show_error <- function(msg) {
    showNotification(msg, type = "error", duration = 10)
    add_log(paste("ERROR:", msg))
  }
  show_warning <- function(msg) {
    showNotification(msg, type = "warning", duration = 5)
    add_log(paste("WARNING:", msg))
  }
  show_success <- function(msg) {
    showNotification(msg, type = "message", duration = 3)
    add_log(paste("SUCCESS:", msg))
  }
  
  # ---------- Helpers: diagnostics & IC ----------
  compute_group_diagnostics <- function(df_group) {
    ok <- sum(!is.na(df_group$Dvalue) & !is.na(df_group$Temperature)) >= 3 &&
      length(unique(df_group$Temperature[!is.na(df_group$Temperature)])) >= 3
    res <- list(
      n = nrow(df_group), n_unique_T = length(unique(df_group$Temperature)),
      rmse = NA_real_, slope = NA_real_, slope_se = NA_real_,
      intercept = NA_real_, intercept_se = NA_real_,
      Z_from_fit = NA_real_, Z_SE_from_fit = NA_real_
    )
    if (ok) {
      fit <- lm(log10(Dvalue) ~ Temperature, data = df_group)
      sm  <- summary(fit)
      b1  <- coef(sm)[["Temperature","Estimate"]]
      se1 <- coef(sm)[["Temperature","Std. Error"]]
      b0  <- coef(sm)[["(Intercept)","Estimate"]]
      se0 <- coef(sm)[["(Intercept)","Std. Error"]]
      res$rmse <- sqrt(mean(sm$residuals^2, na.rm = TRUE))
      res$slope <- b1; res$slope_se <- se1
      res$intercept <- b0; res$intercept_se <- se0
      if (!is.na(b1) && b1 != 0) {
        Z <- -1 / b1
        res$Z_from_fit <- Z
        res$Z_SE_from_fit <- abs(se1 / (b1^2)) # delta method
      }
    }
    return(res)
  }
  
  get_diagnostics_table <- function(data) {
    combos <- unique(data[, c("Microorganism","Publication")])
    out <- data.frame()
    for (i in seq_len(nrow(combos))) {
      m <- combos$Microorganism[i]; p <- combos$Publication[i]
      df_g <- data[data$Microorganism == m & data$Publication == p, , drop = FALSE]
      di <- compute_group_diagnostics(df_g)
      out <- rbind(out, data.frame(
        Microorganism = m, Publication = p,
        n = di$n, n_unique_T = di$n_unique_T, rmse = di$rmse,
        slope = di$slope, slope_se = di$slope_se,
        intercept = di$intercept, intercept_se = di$intercept_se,
        Z_from_fit = di$Z_from_fit, Z_SE_from_fit = di$Z_SE_from_fit,
        stringsAsFactors = FALSE
      ))
    }
    out
  }
  
  attach_prediction_ci <- function(pred_df, data, level = 95) {
    # Récupère incertitudes par groupe (Microorganism x Publication)
    ref_stats <- data %>%
      dplyr::group_by(Microorganism, Publication) %>%
      dplyr::summarise(
        se_log10Dref = stats::sd(log10(Dvalue), na.rm = TRUE) /
          sqrt(sum(!is.na(Dvalue))),
        Z_final = mean(ifelse(is.na(Z_publication), Z_estimated, Z_publication), na.rm = TRUE),
        Z_SE_avail = suppressWarnings(mean(Z_SE, na.rm = TRUE)),
        Temperature_ref = mean(Temperature, na.rm = TRUE),
        .groups = "drop"
      )
    diags <- get_diagnostics_table(data)[, c("Microorganism","Publication","Z_SE_from_fit")]
    ref_stats <- dplyr::left_join(ref_stats, diags, by = c("Microorganism","Publication")) %>%
      dplyr::mutate(se_Z = dplyr::coalesce(Z_SE_avail, Z_SE_from_fit, 0.5))
    out <- dplyr::left_join(pred_df, ref_stats, by = c("Microorganism","Publication"))
    
    se_log10Dref <- dplyr::coalesce(out$se_log10Dref, 0)
    se_Z         <- dplyr::coalesce(out$se_Z, 0)
    Z_final      <- dplyr::coalesce(out$Z_final, 5.5)
    Tref         <- dplyr::coalesce(out$Temperature_ref, mean(out$Temperature, na.rm = TRUE))
    
    out$log10D_SE <- sqrt( (se_log10Dref)^2 + (((out$Temperature - Tref) / (Z_final^2))^2) * (se_Z^2) )
    
    out$log10D_CI95_lower <- out$log10_D - 1.96 * out$log10D_SE
    out$log10D_CI95_upper <- out$log10_D + 1.96 * out$log10D_SE
    out$log10D_CI99_lower <- out$log10_D - 2.576 * out$log10D_SE
    out$log10D_CI99_upper <- out$log10_D + 2.576 * out$log10D_SE
    
    out$Dvalue_CI95_lower <- 10^(out$log10D_CI95_lower)
    out$Dvalue_CI95_upper <- 10^(out$log10D_CI95_upper)
    out$Dvalue_CI99_lower <- 10^(out$log10D_CI99_lower)
    out$Dvalue_CI99_upper <- 10^(out$log10D_CI99_upper)
    out
  }
  # ---------- Fin helpers ----------
  
  output$log_messages <- renderText({ log_text() })
  output$validation_messages <- renderText({ validate_temperature_ranges(input) })
  output$mixed_model_summary <- renderText({ mixed_model_info() })
  
  # AJOUT: conversion CI -> SE et préparation (reprend ton code)
  convert_ci_to_se <- function(data, confidence_level = 95) {
    z_factor <- ifelse(confidence_level == 95, 1.96, 2.576)
    ci_suffix <- paste0("CI", confidence_level)
    d_upper_col <- paste0(ci_suffix, "_D_upper")
    d_lower_col <- paste0(ci_suffix, "_D_lower")
    if (d_upper_col %in% colnames(data) && d_lower_col %in% colnames(data)) {
      valid_d_ci <- !is.na(data[[d_upper_col]]) & !is.na(data[[d_lower_col]]) &
        data[[d_upper_col]] > 0 & data[[d_lower_col]] > 0 & data[[d_upper_col]] > data[[d_lower_col]]
      if (any(valid_d_ci)) {
        log_d_upper <- log10(pmax(data[[d_upper_col]], 1e-10, na.rm = TRUE))
        log_d_lower <- log10(pmax(data[[d_lower_col]], 1e-10, na.rm = TRUE))
        data$Dvalue_SE_from_CI <- NA
        data$Dvalue_SE_from_CI[valid_d_ci] <- (log_d_upper[valid_d_ci] - log_d_lower[valid_d_ci]) / (2 * z_factor)
        if (!"Dvalue_SE" %in% colnames(data) || all(is.na(data$Dvalue_SE))) {
          data$Dvalue_SE <- data$Dvalue_SE_from_CI
        } else {
          missing_se <- is.na(data$Dvalue_SE) & !is.na(data$Dvalue_SE_from_CI)
          data$Dvalue_SE[missing_se] <- data$Dvalue_SE_from_CI[missing_se]
        }
        add_log(paste("Converted", sum(valid_d_ci), "D-value CIs to SE"))
      }
    }
    z_upper_col <- paste0(ci_suffix, "_Z_upper")
    z_lower_col <- paste0(ci_suffix, "_Z_lower")
    if (z_upper_col %in% colnames(data) && z_lower_col %in% colnames(data)) {
      valid_z_ci <- !is.na(data[[z_upper_col]]) & !is.na(data[[z_lower_col]]) &
        data[[z_upper_col]] > 0 & data[[z_lower_col]] > 0 & data[[z_upper_col]] > data[[z_lower_col]]
      if (any(valid_z_ci)) {
        data$Z_SE_from_CI <- NA
        data$Z_SE_from_CI[valid_z_ci] <- (data[[z_upper_col]][valid_z_ci] - data[[z_lower_col]][valid_z_ci]) / (2 * z_factor)
        if (!"Z_SE" %in% colnames(data) || all(is.na(data$Z_SE))) {
          data$Z_SE <- data$Z_SE_from_CI
        } else {
          missing_se <- is.na(data$Z_SE) & !is.na(data$Z_SE_from_CI)
          data$Z_SE[missing_se] <- data$Z_SE_from_CI[missing_se]
        }
        add_log(paste("Converted", sum(valid_z_ci), "Z-value CIs to SE"))
      }
    }
    data
  }
  
  prepare_data <- function(df, confidence_level = 95) {
    add_log("Starting data preparation...")
    df <- df %>% mutate(across(where(is.character), ~ trimws(.)))
    df$Temperature   <- as.numeric(gsub(",", ".", trimws(df$Temperature)))
    df$Dvalue        <- as.numeric(gsub(",", ".", trimws(df$Dvalue)))
    df$Z_publication <- as.numeric(gsub(",", ".", trimws(df$Z_publication)))
    if (!"Matrix_Type" %in% colnames(df)) {
      df$Matrix_Type <- "Unknown"
      add_log("Added missing Matrix_Type = 'Unknown'")
    }
    uncertainty_cols <- c("Dvalue_SE", "Dvalue_SD", "Z_SE", "Z_SD",
                          "CI95_D_lower", "CI95_D_upper", "CI99_D_lower", "CI99_D_upper",
                          "CI95_Z_lower", "CI95_Z_upper", "CI99_Z_lower", "CI99_Z_upper")
    for (col in uncertainty_cols) if (col %in% colnames(df)) df[[col]] <- as.numeric(gsub(",", ".", trimws(df[[col]])))
    if (!"Dvalue_SE" %in% colnames(df) || all(is.na(df$Dvalue_SE))) {
      if ("Dvalue_SD" %in% colnames(df) && any(!is.na(df$Dvalue_SD))) {
        df$Dvalue_SE <- df$Dvalue_SD / sqrt(3)
        add_log("Converted D SD -> SE")
      }
    }
    if (!"Z_SE" %in% colnames(df) || all(is.na(df$Z_SE))) {
      if ("Z_SD" %in% colnames(df) && any(!is.na(df$Z_SD))) {
        df$Z_SE <- df$Z_SD / sqrt(3)
        add_log("Converted Z SD -> SE")
      }
    }
    df <- convert_ci_to_se(df, confidence_level)
    z_defaults <- c(
      "Bacteria" = 5.0, "Bacterial spores" = 8.0, "Enveloped virus" = 4.0,
      "non-enveloped virus" = 6.0, "Yeast" = 4.5, "Yeast_ascospores" = 7.0,
      "Moulds" = 5.5, "Mould_ascospores" = 8.5
    )
    df$Z_estimated <- z_defaults[df$TypeMicroorganism]
    df$Z_estimated[is.na(df$Z_estimated)] <- 5.5
    d_se_count <- sum(!is.na(df$Dvalue_SE)); z_se_count <- sum(!is.na(df$Z_SE))
    add_log(paste("Prepared", nrow(df), "rows with", d_se_count, "D SEs and", z_se_count, "Z SEs"))
    df
  }
  
  calculate_predictions_simple <- function(data, prediction_range) {
    predictions <- data.frame()
    summary_data <- data %>%
      group_by(TypeMicroorganism, Microorganism) %>%
      summarise(
        Dvalue_ref     = exp(mean(log(Dvalue), na.rm = TRUE)),
        Temperature_ref = mean(Temperature, na.rm = TRUE),
        Z_final        = mean(ifelse(is.na(Z_publication), Z_estimated, Z_publication), na.rm = TRUE),
        Category       = first(Category),
        Matrix_Type    = first(Matrix_Type),
        Publication    = first(Publication),
        D_Correction   = first(ifelse(is.na(Z_publication), "Estimated Z", "Published Z")),
        .groups = 'drop'
      )
    for (i in 1:nrow(summary_data)) {
      row <- summary_data[i, ]
      pred_data <- data.frame(
        Microorganism    = row$Microorganism,
        Category         = row$Category,
        TypeMicroorganism= row$TypeMicroorganism,
        Matrix_Type      = row$Matrix_Type,
        Publication      = row$Publication,
        Temperature      = prediction_range,
        log10_D          = log10(row$Dvalue_ref) - (prediction_range - row$Temperature_ref) / row$Z_final,
        D_Correction     = row$D_Correction,
        Z_Correction     = row$D_Correction
      )
      pred_data$Dvalue <- 10^pred_data$log10_D
      predictions <- rbind(predictions, pred_data)
    }
    predictions
  }
  
  validate_temperature_ranges <- function(input) {
    errors <- c(); general_range <- input$temp_range
    if (input$heuristic_use_range) {
      heur_range <- input$heuristic_range
      if (heur_range[1] < general_range[1] || heur_range[2] > general_range[2]) errors <- c(errors, "Heuristic range extends beyond general prediction range")
    }
    if (input$mixed_use_range) {
      mixed_range <- input$mixed_range
      if (mixed_range[1] < general_range[1] || mixed_range[2] > general_range[2]) errors <- c(errors, "Mixed model range extends beyond general prediction range")
    }
    trunc_range <- input$trunc_range
    if (trunc_range[1] < general_range[1] || trunc_range[2] > general_range[2]) errors <- c(errors, "Truncated range extends beyond general prediction range")
    paste(errors, collapse = "; ")
  }
  
  output$log_messages <- renderText({ log_text() })
  output$validation_messages <- renderText({ validate_temperature_ranges(input) })
  output$mixed_model_summary <- renderText({ mixed_model_info() })
  
  observeEvent(input$load_sample, {
    tryCatch({
      possible_paths <- c(
        "sample_data.txt",
        file.path(getwd(), "sample_data.txt")
      )
      sample_file <- NULL
      for (path in possible_paths) if (file.exists(path)) { sample_file <- path; break }
      if (is.null(sample_file)) {
        add_log("sample_data.txt not found, creating example dataset...")
        df <- data.frame(
          Microorganism = rep(c("Salmonella Typhimurium", "Escherichia coli", "Listeria monocytogenes"), each = 4),
          TypeMicroorganism = rep("Bacteria", 12),
          Temperature = rep(c(55, 60, 65, 70), 3),
          Dvalue = c(45.2, 12.3, 3.8, 1.2, 38.7, 10.9, 3.1, 0.9, 52.1, 15.6, 4.7, 1.5),
          Z_publication = rep(c(5.2, 4.8, 5.5), each = 4),
          Publication = rep(c("Smith et al. 2020", "Jones et al. 2021", "Brown et al. 2019"), each = 4),
          Category = rep("Food pathogen", 12),
          Matrix_Type = rep("Buffer", 12),
          Dvalue_SE = c(2.1, 0.8, 0.3, 0.1, 1.9, 0.7, 0.2, 0.08, 2.3, 0.9, 0.4, 0.12),
          Z_SE = rep(c(0.3, 0.2, 0.4), each = 4),
          stringsAsFactors = FALSE
        )
        add_log("Created built-in example dataset with 12 observations")
      } else {
        add_log(paste("Loading sample data from:", sample_file))
        df <- read.delim(sample_file, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
        add_log(paste("Loaded sample data with", nrow(df), "observations"))
      }
      df <- prepare_data(df, as.numeric(input$confidence_level))
      all_data(df)
      updateSelectInput(session, "filter_type", choices = unique(df$TypeMicroorganism))
      updateSelectInput(session, "filter_micro", choices = unique(df$Microorganism))
      updateSelectInput(session, "filter_matrix", choices = unique(df$Matrix_Type))
      show_success("Sample dataset loaded successfully!")
    }, error = function(e) {
      error_msg <- paste("Error loading sample dataset:", e$message)
      add_log(error_msg); show_error(error_msg)
    })
  })
  
  observeEvent(input$file, {
    tryCatch({
      df <- read.delim(input$file$datapath, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
      df <- prepare_data(df, as.numeric(input$confidence_level))
      all_data(df)
      updateSelectInput(session, "filter_type", choices = unique(df$TypeMicroorganism))
      updateSelectInput(session, "filter_micro", choices = unique(df$Microorganism))
      updateSelectInput(session, "filter_matrix", choices = unique(df$Matrix_Type))
      add_log("User dataset loaded and prepared with uncertainty analysis.")
    }, error = function(e) { add_log(paste("Error loading user file:", e$message)) })
  })
  
  dataset <- reactive({
    req(all_data()); df <- all_data()
    df <- df[df$Temperature >= input$temp_range[1] & df$Temperature <= input$temp_range[2], ]
    if (!is.null(input$filter_type) && length(input$filter_type) > 0) df <- df %>% filter(TypeMicroorganism %in% input$filter_type)
    if (!is.null(input$filter_micro) && length(input$filter_micro) > 0) df <- df %>% filter(Microorganism %in% input$filter_micro)
    if (!is.null(input$filter_matrix) && length(input$filter_matrix) > 0) df <- df %>% filter(Matrix_Type %in% input$filter_matrix)
    df
  })
  
  output$table_preview <- renderDT({ 
    req(dataset())
    cols_to_show <- c("Microorganism", "TypeMicroorganism", "Matrix_Type", "Temperature", "Dvalue", "Z_publication", "Publication")
    uncertainty_cols <- c("Dvalue_SE","Dvalue_SD", "Z_SE", "CI95_D_lower", "CI95_D_upper","CI99_D_lower", "CI99_D_upper", "Z_CI95_lower", "Z_CI95_upper", "Z_CI99_lower", "Z_CI99_upper")
    available_uncertainty <- uncertainty_cols[uncertainty_cols %in% colnames(dataset())]
    dataset() %>% select(all_of(c(cols_to_show, available_uncertainty)))
  })
  
  output$data_summary_text <- renderText({
    req(dataset()); data <- dataset()
    d_se_available <- sum(!is.na(data$Dvalue_SE)); z_se_available <- sum(!is.na(data$Z_SE))
    ci95_d_available <- sum(!is.na(data$CI95_D_lower) & !is.na(data$CI95_D_upper))
    ci99_d_available <- sum(!is.na(data$CI99_D_lower) & !is.na(data$CI99_D_upper))
    ci95_z_available <- sum(!is.na(data$CI95_Z_lower) & !is.na(data$CI95_Z_upper))
    ci99_z_available <- sum(!is.na(data$CI99_Z_lower) & !is.na(data$CI99_Z_upper))
    paste(
      paste("Total observations:", nrow(data)),
      paste("Unique microorganisms:", length(unique(data$Microorganism))),
      paste("Unique publications:", length(unique(data$Publication))),
      paste("Temperature range:", round(min(data$Temperature, na.rm = TRUE), 1), "°C to", round(max(data$Temperature, na.rm = TRUE), 1), "°C"),
      paste("D-value range:", round(min(data$Dvalue, na.rm = TRUE), 3), "to", round(max(data$Dvalue, na.rm = TRUE), 3), "min"),
      "",
      "Uncertainty information available:",
      paste("- D-value standard errors:", d_se_available, "observations"),
      paste("- Z-value standard errors:", z_se_available, "observations"),
      paste("- D-value 95% CI:", ci95_d_available, "observations"),
      paste("- D-value 99% CI:", ci99_d_available, "observations"),
      paste("- Z-value 95% CI:", ci95_z_available, "observations"),
      paste("- Z-value 99% CI:", ci99_z_available, "observations"),
      sep = "\n"
    )
  })
  
  observeEvent(input$start_analysis, {
    validation_errors <- validate_temperature_ranges(input)
    if (length(validation_errors) > 0 && any(validation_errors != "")) show_warning(paste("Temperature range issues:", validation_errors))
    if (is.null(dataset()) || nrow(dataset()) == 0) { show_error("No data available for analysis. Please load a dataset first."); return() }
    
    tryCatch({
      req(dataset()); data <- dataset()
      add_log("Starting enhanced analysis with uncertainty propagation...")
      data$Z_final <- ifelse(is.na(data$Z_publication), data$Z_estimated, data$Z_publication)
      prediction_range <- seq(input$temp_range[1], input$temp_range[2], by = input$temp_step)
      predictions <- calculate_predictions_simple(data, prediction_range)
      if (nrow(predictions) == 0) { add_log("ERROR: No valid predictions could be generated"); return() }
      add_log(paste("Generated", nrow(predictions), "aggregated predictions"))
      
      # IC pour Base
      if (isTRUE(input$use_uncertainty)) predictions <- attach_prediction_ci(predictions, data, as.numeric(input$confidence_level))
      
      # Heuristic
      predictions_heuristic <- predictions
      if (input$heuristic_use_range) {
        in_range <- predictions_heuristic$Temperature >= input$heuristic_range[1] & predictions_heuristic$Temperature <= input$heuristic_range[2]
        predictions_heuristic$log10_D[in_range] <- predictions_heuristic$log10_D[in_range] + input$heuristic_shift
        predictions_heuristic$Dvalue[in_range]  <- 10^predictions_heuristic$log10_D[in_range]
        if ("Dvalue_CI95_lower" %in% names(predictions_heuristic)) {
          predictions_heuristic$Dvalue_CI95_lower[in_range] <- 10^(log10(predictions_heuristic$Dvalue_CI95_lower[in_range]) + input$heuristic_shift)
          predictions_heuristic$Dvalue_CI95_upper[in_range] <- 10^(log10(predictions_heuristic$Dvalue_CI95_upper[in_range]) + input$heuristic_shift)
          predictions_heuristic$Dvalue_CI99_lower[in_range] <- 10^(log10(predictions_heuristic$Dvalue_CI99_lower[in_range]) + input$heuristic_shift)
          predictions_heuristic$Dvalue_CI99_upper[in_range] <- 10^(log10(predictions_heuristic$Dvalue_CI99_upper[in_range]) + input$heuristic_shift)
        }
        predictions_heuristic$D_Correction[in_range] <- paste("Heuristic (", input$heuristic_shift, ") - Range limited")
        add_log(paste("Applied heuristic correction", input$heuristic_shift, "to", sum(in_range), "predictions in", paste(input$heuristic_range, collapse = "-"), "°C"))
      } else {
        predictions_heuristic$log10_D <- predictions_heuristic$log10_D + input$heuristic_shift
        predictions_heuristic$Dvalue   <- 10^predictions_heuristic$log10_D
        if ("Dvalue_CI95_lower" %in% names(predictions_heuristic)) {
          predictions_heuristic$Dvalue_CI95_lower <- 10^(log10(predictions_heuristic$Dvalue_CI95_lower) + input$heuristic_shift)
          predictions_heuristic$Dvalue_CI95_upper <- 10^(log10(predictions_heuristic$Dvalue_CI95_upper) + input$heuristic_shift)
          predictions_heuristic$Dvalue_CI99_lower <- 10^(log10(predictions_heuristic$Dvalue_CI99_lower) + input$heuristic_shift)
          predictions_heuristic$Dvalue_CI99_upper <- 10^(log10(predictions_heuristic$Dvalue_CI99_upper) + input$heuristic_shift)
        }
        predictions_heuristic$D_Correction <- paste("Heuristic (", input$heuristic_shift, ") - Global")
        add_log(paste("Applied global heuristic correction", input$heuristic_shift, "to all predictions"))
      }
      if (isTRUE(input$use_uncertainty)) predictions_heuristic <- attach_prediction_ci(predictions_heuristic, data, as.numeric(input$confidence_level))
      
      # Truncated
      predictions_truncated <- predictions
      in_trunc_range <- predictions_truncated$Temperature >= input$trunc_range[1] & predictions_truncated$Temperature <= input$trunc_range[2]
      predictions_truncated$log10_D[in_trunc_range] <- predictions_truncated$log10_D[in_trunc_range] + input$truncated_shift
      predictions_truncated$Dvalue[in_trunc_range]   <- 10^predictions_truncated$log10_D[in_trunc_range]
      if ("Dvalue_CI95_lower" %in% names(predictions_truncated)) {
        predictions_truncated$Dvalue_CI95_lower[in_trunc_range] <- 10^(log10(predictions_truncated$Dvalue_CI95_lower[in_trunc_range]) + input$truncated_shift)
        predictions_truncated$Dvalue_CI95_upper[in_trunc_range] <- 10^(log10(predictions_truncated$Dvalue_CI95_upper[in_trunc_range]) + input$truncated_shift)
        predictions_truncated$Dvalue_CI99_lower[in_trunc_range] <- 10^(log10(predictions_truncated$Dvalue_CI99_lower[in_trunc_range]) + input$truncated_shift)
        predictions_truncated$Dvalue_CI99_upper[in_trunc_range] <- 10^(log10(predictions_truncated$Dvalue_CI99_upper[in_trunc_range]) + input$truncated_shift)
      }
      predictions_truncated$D_Correction[in_trunc_range] <- paste("Truncated (", input$truncated_shift, ")")
      add_log(paste("Applied truncated correction", input$truncated_shift, "to", sum(in_trunc_range), "predictions in", paste(input$trunc_range, collapse = "-"), "°C"))
      if (isTRUE(input$use_uncertainty)) predictions_truncated <- attach_prediction_ci(predictions_truncated, data, as.numeric(input$confidence_level))
      
      # Mixed-effects
      predictions_mixed <- predictions
      tryCatch({
        add_log("Starting mixed effects model following Garre et al. approach...")
        mixed_data <- data.frame(
          log10_D = log10(data$Dvalue),
          Temperature = data$Temperature,
          Microorganism = factor(data$Microorganism),
          Publication   = factor(data$Publication),
          TypeMicroorganism = factor(data$TypeMicroorganism),
          Matrix_Type   = factor(data$Matrix_Type)
        )
        if (input$weighted_analysis && input$use_uncertainty && "Dvalue_SE" %in% colnames(data)) {
          se_values <- data$Dvalue_SE; valid_se <- !is.na(se_values) & se_values > 0
          if (any(valid_se)) {
            median_se <- median(se_values[valid_se], na.rm = TRUE)
            se_values[!valid_se] <- median_se
            mixed_data$weight <- 1 / (se_values^2)
            mixed_data$weight[is.infinite(mixed_data$weight)] <- max(mixed_data$weight[is.finite(mixed_data$weight)])
            mixed_model_intercept <- lmer(log10_D ~ Temperature + (1|Publication), data = mixed_data, weights = weight)
            mixed_model_slope     <- lmer(log10_D ~ Temperature + (Temperature|Publication), data = mixed_data, weights = weight)
            mixed_model_both      <- lmer(log10_D ~ Temperature + (1|Microorganism) + (Temperature|Publication), data = mixed_data, weights = weight)
          } else {
            mixed_model_intercept <- lmer(log10_D ~ Temperature + (1|Publication), data = mixed_data)
            mixed_model_slope     <- lmer(log10_D ~ Temperature + (Temperature|Publication), data = mixed_data)
            mixed_model_both      <- lmer(log10_D ~ Temperature + (1|Microorganism) + (Temperature|Publication), data = mixed_data)
          }
        } else {
          mixed_model_intercept <- lmer(log10_D ~ Temperature + (1|Publication), data = mixed_data)
          mixed_model_slope     <- lmer(log10_D ~ Temperature + (Temperature|Publication), data = mixed_data)
          mixed_model_both      <- lmer(log10_D ~ Temperature + (1|Microorganism) + (Temperature|Publication), data = mixed_data)
        }
        
        # Sélection du meilleur modèle
        model_name <- "Random Intercept Model"; mixed_model <- mixed_model_intercept
        tryCatch({
          aics <- c(AIC(mixed_model_intercept), AIC(mixed_model_slope), AIC(mixed_model_both))
          idx <- which.min(aics)
          mixed_model <- list(mixed_model_intercept, mixed_model_slope, mixed_model_both)[[idx]]
          model_name  <- c("Random Intercept Model", "Random Slope Model", "Random Intercept + Slope Model")[idx]
          add_log(paste("Best model:", model_name, "AIC =", round(aics[idx], 2)))
        }, error = function(e) add_log("Model comparison failed, using random intercept model"))
        
        # Info modèle pour l'UI
        model_summary <- paste(
          "=== MIXED EFFECTS MODELS (Garre et al. 2023) ===",
          "\nSelected:", model_name,
          "\nAICs:",
          paste("- Random Intercept:", round(AIC(mixed_model_intercept), 2)),
          paste("- Random Slope:", round(AIC(mixed_model_slope), 2)),
          paste("- Both:", round(AIC(mixed_model_both), 2)),
          sep = "\n"
        )
        mixed_model_info(model_summary)
        
        # Prédictions
        for (i in 1:nrow(predictions_mixed)) {
          temp <- predictions_mixed$Temperature[i]
          micro <- predictions_mixed$Microorganism[i]
          pub <- predictions_mixed$Publication[i]
          new_data <- data.frame(
            Temperature = temp,
            Microorganism = factor(micro, levels = levels(mixed_data$Microorganism)),
            Publication   = factor(pub, levels = levels(mixed_data$Publication))
          )
          pred_log10_D <- tryCatch({ predict(mixed_model, newdata = new_data, allow.new.levels = TRUE) }, error = function(e) NA)
          if (!is.na(pred_log10_D)) {
            if (input$mixed_use_range) {
              if (temp >= input$mixed_range[1] && temp <= input$mixed_range[2]) {
                predictions_mixed$log10_D[i] <- pred_log10_D
                predictions_mixed$Dvalue[i]  <- 10^pred_log10_D
                predictions_mixed$D_Correction[i] <- paste("Mixed Model:", model_name)
              }
            } else {
              predictions_mixed$log10_D[i] <- pred_log10_D
              predictions_mixed$Dvalue[i]  <- 10^pred_log10_D
              predictions_mixed$D_Correction[i] <- paste("Mixed Model:", model_name)
            }
          }
        }
        
        # Intervalles pour le modèle mixte
        if (requireNamespace("merTools", quietly = TRUE)) {
          newdf <- predictions_mixed %>%
            dplyr::select(Temperature, Microorganism, Publication) %>%
            dplyr::mutate(
              Microorganism = factor(Microorganism, levels = levels(mixed_data$Microorganism)),
              Publication   = factor(Publication,   levels = levels(mixed_data$Publication))
            )
          pi95 <- merTools::predictInterval(mixed_model, newdata = newdf, level = 0.95, n.sims = 500, include.resid.var = TRUE)
          pi99 <- merTools::predictInterval(mixed_model, newdata = newdf, level = 0.99, n.sims = 500, include.resid.var = TRUE)
          predictions_mixed$log10D_CI95_lower <- pi95$lwr
          predictions_mixed$log10D_CI95_upper <- pi95$upr
          predictions_mixed$log10D_CI99_lower <- pi99$lwr
          predictions_mixed$log10D_CI99_upper <- pi99$upr
          predictions_mixed$Dvalue_CI95_lower <- 10^(predictions_mixed$log10D_CI95_lower)
          predictions_mixed$Dvalue_CI95_upper <- 10^(predictions_mixed$log10D_CI95_upper)
          predictions_mixed$Dvalue_CI99_lower <- 10^(predictions_mixed$log10D_CI99_lower)
          predictions_mixed$Dvalue_CI99_upper <- 10^(predictions_mixed$log10D_CI99_upper)
          add_log("Mixed model prediction intervals computed with merTools::predictInterval")
        } else {
          model_se <- sigma(mixed_model)
          predictions_mixed$log10D_SE <- model_se
          predictions_mixed$Dvalue_CI95_lower <- 10^(predictions_mixed$log10_D - 1.96 * model_se)
          predictions_mixed$Dvalue_CI95_upper <- 10^(predictions_mixed$log10_D + 1.96 * model_se)
          predictions_mixed$Dvalue_CI99_lower <- 10^(predictions_mixed$log10_D - 2.576 * model_se)
          predictions_mixed$Dvalue_CI99_upper <- 10^(predictions_mixed$log10_D + 2.576 * model_se)
          add_log("Mixed model confidence bands computed from residual SD (fallback)")
        }
        
        add_log("Mixed effects model predictions completed")
      }, error = function(e) {
        add_log(paste("Error in mixed effects model:", e$message))
        predictions_mixed <- predictions
        mixed_model_info("Mixed effects model failed to converge. Using base predictions.")
      })
      
      # Stocker toutes les prédictions
      predictions_all(list(
        base = predictions,
        heuristic = predictions_heuristic,
        truncated = predictions_truncated,
        mixed = predictions_mixed
      ))
      add_log("Analysis completed successfully!")
      
    }, error = function(e) { add_log(paste("CRITICAL ERROR in analysis:", e$message)) })
  })
  
  # Graphiques
  create_plot_with_uncertainty <- function(predictions_data, title, show_confidence = TRUE) {
    if (is.null(predictions_data) || nrow(predictions_data) == 0) return(ggplot() + ggtitle("No data available"))
    p <- ggplot(predictions_data, aes(x = Temperature, y = Dvalue, color = TypeMicroorganism)) +
      geom_point(alpha = 0.7, size = 2) +
      geom_line(aes(group = interaction(Microorganism, Publication)), alpha = 0.6) +
      scale_color_manual(values = colors, name = "Type") +
      scale_y_log10() +
      labs(title = title, x = "Temperature (°C)", y = "D-value (min)") +
      theme_minimal() +
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
    if (show_confidence && "Dvalue_CI95_lower" %in% colnames(predictions_data)) {
      if (as.numeric(input$confidence_level) == 95) {
        p <- p + geom_ribbon(aes(ymin = Dvalue_CI95_lower, ymax = Dvalue_CI95_upper, fill = TypeMicroorganism), alpha = 0.2) +
          scale_fill_manual(values = colors, name = "Type", guide = "none")
      } else {
        p <- p + geom_ribbon(aes(ymin = Dvalue_CI99_lower, ymax = Dvalue_CI99_upper, fill = TypeMicroorganism), alpha = 0.2) +
          scale_fill_manual(values = colors, name = "Type", guide = "none")
      }
    }
    p
  }
  
  create_limits_plot <- function(predictions_data, title) {
    if (is.null(predictions_data) || nrow(predictions_data) == 0) return(ggplot() + ggtitle("No data available"))
    limits_data <- predictions_data %>%
      group_by(Temperature, TypeMicroorganism) %>%
      summarise(
        P5 = quantile(Dvalue, 0.05, na.rm = TRUE),
        P25 = quantile(Dvalue, 0.25, na.rm = TRUE),
        P75 = quantile(Dvalue, 0.75, na.rm = TRUE),
        P95 = quantile(Dvalue, 0.95, na.rm = TRUE),
        .groups = 'drop'
      )
    ggplot(limits_data, aes(x = Temperature)) +
      geom_ribbon(aes(ymin = P5, ymax = P95, fill = TypeMicroorganism), alpha = 0.3) +
      geom_ribbon(aes(ymin = P25, ymax = P75, fill = TypeMicroorganism), alpha = 0.5) +
      geom_line(aes(y = P5, color = TypeMicroorganism), linetype = "dashed") +
      geom_line(aes(y = P95, color = TypeMicroorganism), linetype = "dashed") +
      scale_y_log10() +
      scale_fill_manual(values = colors, name = "Type") +
      scale_color_manual(values = colors, name = "Type") +
      labs(title = paste(title, "- Upper and Lower Limits"), x = "Temperature (°C)", y = "D-value (min)") +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
  }
  
  output$plot_base      <- renderPlot({ req(predictions_all()); create_plot_with_uncertainty(predictions_all()$base,      "Base Model Predictions",      input$use_uncertainty) })
  output$limits_base    <- renderPlot({ req(predictions_all()); create_limits_plot(predictions_all()$base,      "Base Model") })
  output$plot_heuristic <- renderPlot({ req(predictions_all()); create_plot_with_uncertainty(predictions_all()$heuristic,  "Heuristic Model Predictions", input$use_uncertainty) })
  output$limits_heuristic <- renderPlot({ req(predictions_all()); create_limits_plot(predictions_all()$heuristic,  "Heuristic Model") })
  output$plot_truncated <- renderPlot({ req(predictions_all()); create_plot_with_uncertainty(predictions_all()$truncated,  "Truncated Model Predictions", input$use_uncertainty) })
  output$limits_truncated <- renderPlot({ req(predictions_all()); create_limits_plot(predictions_all()$truncated,  "Truncated Model") })
  output$plot_mixed     <- renderPlot({ req(predictions_all()); create_plot_with_uncertainty(predictions_all()$mixed,     "Mixed Effects Model Predictions", input$use_uncertainty) })
  output$limits_mixed   <- renderPlot({ req(predictions_all()); create_limits_plot(predictions_all()$mixed,     "Mixed Effects Model") })
  
  # Tables
  output$table_base <- renderDT({ req(predictions_all()); predictions_all()$base %>% select(Microorganism, TypeMicroorganism, Temperature, Dvalue, D_Correction) %>% datatable(options = list(pageLength = 10, scrollX = TRUE)) })
  output$table_heuristic <- renderDT({ req(predictions_all()); predictions_all()$heuristic %>% select(Microorganism, TypeMicroorganism, Temperature, Dvalue, D_Correction) %>% datatable(options = list(pageLength = 10, scrollX = TRUE)) })
  output$table_truncated <- renderDT({ req(predictions_all()); predictions_all()$truncated %>% select(Microorganism, TypeMicroorganism, Temperature, Dvalue, D_Correction) %>% datatable(options = list(pageLength = 10, scrollX = TRUE)) })
  output$table_mixed <- renderDT({ req(predictions_all()); predictions_all()$mixed %>% select(Microorganism, TypeMicroorganism, Temperature, Dvalue, D_Correction) %>% datatable(options = list(pageLength = 10, scrollX = TRUE)) })
  
  # Téléchargements CSV unitaires
  output$download_base <- downloadHandler(filename = function() paste("base_model_predictions_", Sys.Date(), ".csv", sep = ""), content = function(file) { if (!is.null(predictions_all())) write.csv(predictions_all()$base, file, row.names = FALSE) })
  output$download_heuristic <- downloadHandler(filename = function() paste("heuristic_model_predictions_", Sys.Date(), ".csv", sep = ""), content = function(file) { if (!is.null(predictions_all())) write.csv(predictions_all()$heuristic, file, row.names = FALSE) })
  output$download_truncated <- downloadHandler(filename = function() paste("truncated_model_predictions_", Sys.Date(), ".csv", sep = ""), content = function(file) { if (!is.null(predictions_all())) write.csv(predictions_all()$truncated, file, row.names = FALSE) })
  output$download_mixed <- downloadHandler(filename = function() paste("mixed_model_predictions_", Sys.Date(), ".csv", sep = ""), content = function(file) { if (!is.null(predictions_all())) write.csv(predictions_all()$mixed, file, row.names = FALSE) })
  
  # Export Excel complet
  output$download_excel <- downloadHandler(
    filename = function() paste("all_predictions_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) {
      if (!is.null(predictions_all())) {
        wb <- createWorkbook()
        addWorksheet(wb, "Base_Model");      writeData(wb, "Base_Model",      predictions_all()$base)
        addWorksheet(wb, "Heuristic_Model"); writeData(wb, "Heuristic_Model", predictions_all()$heuristic)
        addWorksheet(wb, "Truncated_Model"); writeData(wb, "Truncated_Model", predictions_all()$truncated)
        addWorksheet(wb, "Mixed_Model");     writeData(wb, "Mixed_Model",     predictions_all()$mixed)
        
        # Diagnostics
        addWorksheet(wb, "Diagnostics")
        diag_tbl <- get_diagnostics_table(dataset())
        if ("TypeMicroorganism" %in% names(dataset())) {
          diag_tbl <- dplyr::left_join(diag_tbl, dataset() %>% dplyr::select(Microorganism, Publication, TypeMicroorganism) %>% dplyr::distinct(), by = c("Microorganism","Publication"))
        }
        writeData(wb, "Diagnostics", diag_tbl)
        
        # Model info
        addWorksheet(wb, "Model_Info")
        model_info_df <- data.frame(Info = c("Selected mixed model & AIC"), Value = c(gsub("\n", " ", mixed_model_info())), stringsAsFactors = FALSE)
        writeData(wb, "Model_Info", model_info_df)
        
        saveWorkbook(wb, file)
      }
    }
  )
}
