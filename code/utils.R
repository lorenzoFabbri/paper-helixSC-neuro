#' Load and process DAG
#'
#' @param dags A list of DAGs. A list.
#' @param exposure A string defining the exposure of interest. A string.
#' @param outcome A string defining the outcome of interest. A string.
#' @param params_dag A list of parameters for `dagitty`. A list.
#'
#' @return A named list with DAG and adjustment sets. A list.
#'
#' @export
load_dag <- function(dags, exposure, outcome, params_dag) {
  ret <- list()
  rq <- Sys.getenv("TAR_PROJECT")
  which_dag <- switch(rq, 
                      "rq01" = "chem_to_out", 
                      "rq1" = "chem_to_out", 
                      "rq02" = "chem_to_marker", 
                      "rq2" = "chem_to_marker", 
                      "rq03" = "marker_to_out", 
                      "rq3" = "marker_to_out")
  
  dag <- dags[[which_dag]]
  ret$dag <- dag
  
  # Find the adjustment set(s) of interest
  adjustment_set <- dagitty::adjustmentSets(x = dag, 
                                            exposure = exposure, 
                                            outcome = outcome, 
                                            type = params_dag$type, 
                                            effect = params_dag$effect)
  ret$adjustment_sets <- adjustment_set
  
  # Identify precision covariates to add to adjustment set
  prec_covars <- dagitty::parents(x = dag, 
                                  v = outcome)
  parents_exposure <- dagitty::parents(x = dag, 
                                       v = exposure)
  ret$prec_covars <- setdiff(setdiff(prec_covars, 
                                     exposure), 
                             parents_exposure)
  
  return(ret)
} # End function load_dag
################################################################################

#' Various ways to select an adjustment set out of many
#'
#' @param dat A named list of tibbles containing the variables of interest. A list.
#' @param res_dag Result of the call to [load_dag()]. A named list.
#' @param strategy Strategy to adopt to select the adjustment set.
#' The currently available strategies are: `first`, `smallest`, `largest`, `random`,
#' and `minimize_missings`. A string.
#'
#' @return An adjustment set. A vector.
#'
#' @export
select_adjustment_set <- function(dat, meta, res_dag, strategy) {
  all_as <- res_dag$adjustment_sets
  lengths_as <- lapply(all_as, length) |>
    unname() |>
    unlist()
  
  ret <- switch(strategy,
    "first" = all_as[[1]], 
    "smallest" = all_as[[which.min(lengths_as)]], 
    "largest" = all_as[[which.max(lengths_as)]], 
    "random" = sample(all_as, size = 1), 
    "minimize_missings" = all_as[[myphd::minimize_missings(
      dat = dat, 
      meta = meta, 
      adjustment_sets = all_as
    )]]
  )
  
  return(ret)
} # End function select_adjustment_set
################################################################################

#' Load data necessary for down-streams analyses
#'
#' @param ids_other_covars IDs for additional covariate data. A vector.
#' @param res_dag Result of the call to [load_dag()]. A list.
#'
#' @return A named list of exposures, covariates, and outcomes. A list.
#'
#' @export
rq_load_data <- function(res_dag) {
  rq <- Sys.getenv("TAR_PROJECT")
  rq <- switch(rq, 
               "rq01" = "rq1", 
               "rq1" = "rq1", 
               "rq02" = "rq2", 
               "rq2" = "rq2", 
               "rq03" = "rq3", 
               "rq3" = "rq3")
  
  # Load data request
  params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
  dat_request <- load_dat_request()
  
  # Create one dataset for covariates, one for exposures, and one for outcomes
  dat <- list()
  warning("The following exposures were not found:")
  cat(setdiff(
    params_dat$variables[[rq]]$exposures, 
    colnames(dat_request$dat)
  ), sep = "\n")
  dat$exposures <- dat_request$dat |>
    dplyr::select(dplyr::any_of(c(
      params_dat$variables$identifier, 
      params_dat$variables[[rq]]$exposures
    )))
  dat$outcome <- dat_request$dat |>
    dplyr::select(params_dat$variables$identifier, 
                  params_dat$variables[[rq]]$outcome)
  adj_set <- select_adjustment_set(dat = dat_request$dat, 
                                   meta = dat_request$meta, 
                                   res_dag = res_dag, 
                                   strategy = params_dat$variables$strategy_select_adj_set)
  # Add precision covariates
  adj_set <- unique(c(adj_set, 
                      res_dag$prec_covars))
  mapping_covars <- dat_request$meta[dat_request$meta$dag %in% adj_set, 
                                     ]$variable |>
    as.character()
  dat$adjustment_set <- adj_set
  dat$mapping_covariates <- mapping_covars
  dat$covariates <- dat_request$dat |>
    dplyr::select(params_dat$variables$identifier, 
                  dplyr::all_of(mapping_covars))
  cols_to_season <- c("hs_date_neu", "e3_cbirth")
  dat$covariates <- myphd::convert_time_season(dat = dat$covariates, 
                                               cols = cols_to_season) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(cols_to_season), 
                                \(x) factor(x)))
  
  return(dat)
} # End function rq_load_data
################################################################################

#' Pre-process data for research questions
#'
#' @param dat A named list of tibbles containing the variables of interest. A list.
#'
#' @return A named list of pre-processed tibbles containing the variables of interest.
#' A list.
#'
#' @export
rq_prepare_data <- function(dat) {
  rq <- Sys.getenv("TAR_PROJECT")
  rq <- switch(rq, 
               "rq01" = "rq1", 
               "rq1" = "rq1", 
               "rq02" = "rq2", 
               "rq2" = "rq2", 
               "rq03" = "rq3", 
               "rq3" = "rq3")
  params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
  steps_exposures <- params_dat$steps[[rq]]$preproc_exposures
  steps_covars <- params_dat$steps[[rq]]$preproc_covars
  
  # Process exposures
  dat$exposures <- myphd::extract_cohort(dat = dat$exposures, 
                                         id_var = params_dat$variables$identifier)
  dat$exposures <- myphd::preproc_data(dat = dat$exposures, 
                                       dic_steps = steps_exposures, 
                                       id_var = params_dat$variables$identifier, 
                                       by_var = "cohort")
  dat$exposures <- dplyr::select(dat$exposures, 
                                 -dplyr::any_of("cohort"))
  
  # Process covariates
  dat$covariates <- myphd::preproc_data(dat = dat$covariates, 
                                        dic_steps = steps_covars, 
                                        id_var = params_dat$variables$identifier, 
                                        by_var = "cohort")
  
  return(dat)
} # End function rq_prepare_data
################################################################################

#' Estimate weights and explore covariance balance
#'
#' @param dat A named list of tibbles containing the variables of interest. A list.
#'
#' @return A named list containing estimated weights and results of
#' balance exploration. A list.
#'
#' @export
rq_estimate_weights <- function(dat, save_results, parallel, workers) {
  rq <- Sys.getenv("TAR_PROJECT")
  rq <- switch(rq, 
               "rq01" = "rq1", 
               "rq1" = "rq1", 
               "rq02" = "rq2", 
               "rq2" = "rq2", 
               "rq03" = "rq3", 
               "rq3" = "rq3")
  params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
  outcome <- params_dat$variables[[rq]]$outcome
  params_ana <- params_analyses()[[rq]]
  
  # Step 1: estimate weights for covariate balance
  list_exposures <- names(dat$exposures)
  list_exposures <- setdiff(list_exposures, 
                            params_dat$variables$identifier)
  list_covariates <- setdiff(colnames(dat$covariates), 
                             params_dat$variables$identifier)
  ## Loop over exposures
  if (parallel == TRUE) {
    future::plan(future::multisession, 
                 workers = workers)
  } else {
    future::plan(future::sequential())
  }
  progressr::with_progress({
    p <- progressr::progressor(steps = length(list_exposures))
    
    estimated_weights <- furrr::future_map(list_exposures, function(x, p) {
      p()
      tmp <- suppressWarnings(suppressMessages(myphd::estimate_weights(
        dat = dplyr::full_join(dplyr::select(dat$exposures, 
                                             dplyr::all_of(c(x, 
                                                             params_dat$variables$identifier))), 
                               dat$covariates, 
                               by = params_dat$variables$identifier) |>
          dplyr::select(-params_dat$variables$identifier), 
        exposure = x, 
        covariates = list_covariates, 
        method = params_ana$method_weightit, 
        method_args = list(
          use_kernel = params_ana$use_kernel, 
          sl_discrete = params_ana$sl_discrete, 
          sl_lib = params_ana$sl_lib
        ))
      )) # End estimation weights current exposure
      ret <- suppressMessages(WeightIt::trim(tmp$weights, 
                                             params_ana$weights_trim))
    }, 
    .options = furrr::furrr_options(
      seed = TRUE
    ), 
    p = p) # End loop over exposures to estimate weights
  }) # End progress bar
  future::plan(future::sequential)
  names(estimated_weights) <- list_exposures
  path_save_weights <- paste0(
    Sys.getenv("path_store_res"), 
    "weights_exposure_model/"
  )
  if (!dir.exists(path_save_weights)) {
    dir.create(path_save_weights)
  }
  qs::qsave(x = estimated_weights, 
            file = paste0(
              path_save_weights, 
              rq, "_", 
              params_ana$method_weightit, 
              ".qs"
            ), 
            nthreads = 4)
  ##############################################################################
  
  # Step 2: explore balance
  if (parallel == TRUE) {
    future::plan(future::multisession, 
                 workers = 2)
  } else {
    future::plan(future::sequential())
  }
  balance <- furrr::future_map(names(estimated_weights), function(x) {
    myphd::explore_balance(exposure = strsplit(x, split = "_")[[1]][2],
                           covariates = estimated_weights[[x]]$covs |>
                             colnames(),
                           weights = estimated_weights[[x]])
  }, 
  .options = furrr::furrr_options(
    seed = TRUE
  ))
  future::plan(future::sequential)
  names(balance) <- names(estimated_weights)
  
  ## Save results
  if (save_results) {
    if (parallel == TRUE) {
      future::plan(future::multisession, 
                   workers = 2)
    } else {
      future::plan(future::sequential())
    }
    furrr::future_map(names(balance), function(x) {
      ### bal.plot
      .path <- paste0(
        "results/figures/",
        Sys.getenv("TAR_PROJECT"),
        "/balplot_",
        params_ana$method_weightit, "_",
        strsplit(x, split = "_")[[1]][2],
        ".pdf"
      )
      ggplot2::ggsave(.path,
                      gridExtra::marrangeGrob(grobs = balance[[x]]$graph,
                                              nrow = 1,
                                              ncol = 1),
                      dpi = 480, type = "cairo")
      
      ### bal.tab
      .path <- paste0(
        "results/tables/",
        Sys.getenv("TAR_PROJECT"),
        "/baltab_",
        params_ana$method_weightit, "_",
        strsplit(x, split = "_")[[1]][2],
        ".docx"
      )
      tab <- balance[[x]]$tab$Balance |>
        as.data.frame() |>
        tibble::rownames_to_column(var = "variable")
      colnames(tab) <- c("variable", "type",
                         "unadj. correlation", "adj. correlation",
                         "threshold", "adj. KS")
      tab <- tab |>
        dplyr::arrange(dplyr::desc(`adj. correlation`),
                       variable) |>
        dplyr::mutate(type = dplyr::recode(
          type, "Contin." = "continuous",
          "Binary" = "binary"
        )) |>
        gt::gt(groupname_col = "type") |>
        gt::tab_options(row_group.as_column = TRUE) |>
        gt::fmt_number(decimals = 3) |>
        gt::tab_header(paste0("Balance statistics: ",
                              strsplit(x, split = "_")[[1]][2]))
      gt::gtsave(tab, filename = .path)
    }, 
    .options = furrr::furrr_options(
      seed = TRUE
    ))
    future::plan(future::sequential)
    
    ### love.plot
    .path <- paste0(
      "results/figures/",
      Sys.getenv("TAR_PROJECT"),
      "/loveplot_",
      params_ana$method_weightit,
      ".pdf"
    )
    ggplot2::ggsave(.path,
                    gridExtra::marrangeGrob(grobs = sapply(balance, `[[`, "love"),
                                            nrow = 1,
                                            ncol = 1),
                    dpi = 480, height = 6, type = "cairo")
  } # End save_results
  
  return(list(
    estimated_weights = estimated_weights, 
    balance = balance
  ))
} # End function rq_estimate_weights
################################################################################

#' Title
#'
#' @param dat
#' @param weights
#'
#' @return
#'
#' @export
rq_fit_model_weighted <- function(dat, weights, parallel, workers) {
  rq <- Sys.getenv("TAR_PROJECT")
  params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
  outcome <- params_dat$variables[[rq]]$outcome
  steps_outcome <- params_dat$steps[[rq]]$preproc_outcome
  params_ana <- params_analyses()[[rq]]
  
  # Process outcome
  if (params_ana$method_marginal == "super") {
    steps_outcome$bound$do <- TRUE
  } else {
    steps_outcome$bound$do <- FALSE
  }
  dat$outcome <- myphd::extract_cohort(dat = dat$outcome, 
                                       id_var = params_dat$variables$identifier)
  dat$outcome <- myphd::preproc_data(dat = dat$outcome, 
                                     outcome = outcome, 
                                     dic_steps = steps_outcome, 
                                     id_var = params_dat$variables$identifier, 
                                     by_var = "cohort")
  dat$outcome <- dat$outcome |>
    dplyr::select(-cohort)
  idxs_missing_outcome <- which(is.na(dat$outcome[[outcome]]))
  
  # Fit model(s) using estimated weights
  list_exposures <- names(dat$exposures)
  list_exposures <- setdiff(list_exposures, 
                            params_dat$variables$identifier)
  list_covariates <- setdiff(colnames(dat$covariates), 
                             params_dat$variables$identifier)
  dat_merged <- dplyr::full_join(dat$covariates, dat$outcome, 
                                 by = params_dat$variables$identifier)
  if (is.null(weights)) {
    weights <- qs::qread(
      file = paste0(
        Sys.getenv("path_store_res"), 
        "weights_exposure_model/", 
        params_ana$method_weightit, 
        ".qs"
      ), 
      nthreads = 4
    )
  } # End check if weights are not provided
  
  ## Loop over each exposure
  if (parallel == TRUE) {
    future::plan(future::multisession, 
                 workers = workers)
  } else {
    future::plan(future::sequential())
  }
  progressr::with_progress({
    p <- progressr::progressor(steps = length(list_exposures))
    
    fits <- furrr::future_map(list_exposures, function(exposure, p) {
      p()
      
      dat_analysis <- dplyr::full_join(dplyr::select(dat$exposures, 
                                                     dplyr::all_of(c(exposure, 
                                                                     params_dat$variables$identifier))), 
                                       dat_merged, 
                                       by = params_dat$variables$identifier) |>
        dplyr::select(-params_dat$variables$identifier) |>
        dplyr::filter(!is.na(.data[[outcome]]))
      
      if (length(idxs_missing_outcome) > 0) {
        weights_exposure <- weights[[exposure]]$weights[-idxs_missing_outcome]
      } else {
        weights_exposure <- weights[[exposure]]$weights
      }
      
      fit <- myphd::fit_model_weighted(
        dat = dat_analysis, 
        outcome = outcome, 
        exposure = exposure, 
        covariates = list_covariates, 
        weights = weights_exposure, 
        method = params_ana$method_marginal, 
        method_args = list(
          family = params_ana$family_marginal, 
          add_inter_exposure = params_ana$add_inter_exposure, 
          add_splines_exposure = params_ana$add_splines_exposure, 
          df_splines = params_ana$df_splines, 
          threshold_smooth = params_ana$threshold_smooth, 
          threshold_k = params_ana$threshold_k
        )
      ) # End fit current exposure
      
      return(list(
        fit = fit$fit, 
        dat = dat_analysis, 
        weights = weights_exposure
      ))
    }, 
    .options = furrr::furrr_options(
      seed = TRUE
    ), 
    p = p) # End loop over exposures to fit weighted models
  }) # End progress bar
  future::plan(future::sequential)
  names(fits) <- list_exposures
  
  return(list(
    fits = fits
  ))
} # End function rq_fit_model_weighted
################################################################################

#' Title
#'
#' @param fits 
#'
#' @return
#'
#' @export
rq_estimate_marginal_effects <- function(fits, parallel, workers) {
  rq <- Sys.getenv("TAR_PROJECT")
  params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
  
  # Loop over the fitted models to estimate marginal effects
  if (parallel == TRUE) {
    future::plan(future::multisession, 
                 workers = workers)
  } else {
    future::plan(future::sequential())
  }
  progressr::with_progress({
    p <- progressr::progressor(steps = length(list_exposures))
    
    ret <- furrr::future_map(seq_along(fits), function(idx, p) {
      p()
      
      exposure <- names(fits)[[idx]]
      mod <- fits[[exposure]]$fit
      dat <- fits[[exposure]]$dat
      weights <- fits[[exposure]]$weights
      
      ############################################################################
      # G-computation (ADRF)
      ## Values of exposure for counterfactual predictions, based on quantiles
      values <- with(dat, seq(
        quantile(get(exposure), 0.1), 
        quantile(get(exposure), 0.9), 
        length.out = 50
      ))
      gcomp <- eval(
        parse(
          text = glue::glue(
            "marginaleffects::avg_predictions(
            model = mod, 
            variables = list(
              {exposure} = values
            ), 
            wts = weights
          )", 
            exposure = exposure
          )
        )
      ) # End G-computation (avg_predictions)
      
      # Plot ADRF
      adrf <- gcomp |>
        ggplot2::ggplot(ggplot2::aes(x = .data[[exposure]])) +
        ggplot2::geom_point(ggplot2::aes(y = estimate)) +
        ggplot2::geom_line(ggplot2::aes(y = estimate), 
                           linewidth = 0.2) +
        ggplot2::geom_ribbon(ggplot2::aes(
          ymin = conf.low, 
          ymax = conf.high
        ), alpha = 0.2) +
        ggplot2::geom_rug(
          mapping = ggplot2::aes(x = .data[[exposure]]), 
          data = dat, 
          inherit.aes = FALSE
        ) +
        ggplot2::scale_x_continuous(limits = c(
          values[1], 
          values[length(values)]
        )) +
        ggplot2::labs(x = exposure, y = "E[Y|A]") +
        ggplot2::theme_minimal()
      ############################################################################
      
      ############################################################################
      # Slopes (AMEF)
      weights_repeated = rep(weights, 
                             times = length(values))
      slopes <- eval(
        parse(
          text = glue::glue(
            "marginaleffects::avg_slopes(
            model = mod, 
            variables = {glue::double_quote(exposure)}, 
            newdata = marginaleffects::datagridcf({exposure} = values), 
            by = {glue::double_quote(exposure)}, 
            wts = weights_repeated
          )", 
            exposure = exposure
          )
        )
      ) # End slopes (avg_slopes)
      
      # Plot AMEF
      amef <- slopes |>
        ggplot2::ggplot(ggplot2::aes(x = .data[[exposure]])) +
        ggplot2::geom_point(ggplot2::aes(y = estimate)) +
        ggplot2::geom_line(ggplot2::aes(y = estimate), 
                           linewidth = 0.2) +
        ggplot2::geom_ribbon(ggplot2::aes(
          ymin = conf.low, 
          ymax = conf.high
        ), alpha = 0.2) +
        ggplot2::geom_hline(yintercept = 0, 
                            linetype = "dashed") +
        ggplot2::geom_rug(
          mapping = ggplot2::aes(x = .data[[exposure]]), 
          data = dat, 
          inherit.aes = FALSE
        ) +
        ggplot2::scale_x_continuous(limits = c(
          values[1], 
          values[length(values)]
        )) +
        ggplot2::labs(x = exposure, y = "dE[Y|A]/dA") +
        ggplot2::theme_minimal()
      ############################################################################
      
      ############################################################################
      #
      ############################################################################
      
      return(list(
        gcomp = gcomp, 
        adrf = adrf, 
        slopes = slopes, 
        amef = amef
      ))
    }, 
    .options = furrr::furrr_options(
      seed = TRUE
    ), 
    p = p) # End loop over fitted models
  }) # End progress bar
  future::plan(future::sequential)
  names(ret) <- names(fits)
  
  return(list(
    marginal_effects = ret
  ))
} # End function rq_estimate_marginal_effects
################################################################################

#' Fit models using `lmtp`
#'
#' @param dat A named list of tibbles containing the variables of interest. A list.
#' @param shift_exposure Whether to shift the exposure variables or not. A logical. 
#'
#' @return A named list containing the results of the call to `lmtp`, for each exposure.
#'
#' @export
run_mtp <- function(dat, shift_exposure) {
  rq <- Sys.getenv("TAR_PROJECT")
  params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
  outcome <- params_dat$variables[[rq]]$outcome
  steps_outcome <- params_dat$steps[[rq]]$preproc_outcome
  params_ana <- params_analyses()[[rq]]
  
  # Process outcome
  dat$outcome <- myphd::extract_cohort(dat = dat$outcome, 
                                       id_var = params_dat$variables$identifier)
  dat$outcome <- myphd::preproc_data(dat = dat$outcome, 
                                     outcome = outcome, 
                                     dic_steps = steps_outcome, 
                                     id_var = params_dat$variables$identifier, 
                                     by_var = "cohort")
  dat$outcome <- dat$outcome |>
    dplyr::select(-cohort)
  
  # Run lmtp
  if (shift_exposure == TRUE) {
    shift_func <- switch(params_ana$shift_type, 
                         "mul" = mtp_mul, 
                         "add" = mtp_add)
  } else {
    shift_func = NULL
  }
  list_exposures <- names(dat$exposures)
  list_exposures <- setdiff(list_exposures, 
                            params_dat$variables$identifier)
  dat_merged <- dplyr::full_join(dat$covariates, dat$outcome, 
                                 by = params_dat$variables$identifier) |>
    dplyr::mutate(cens = as.integer(!is.na(.data[[outcome]])))
  baseline <- dat$covariates |>
    dplyr::select(-params_dat$variables$identifier) |>
    names()
  
  # Loop over each exposure
  res <- lapply(list_exposures, function(exposure) {
    dat_analysis <- dplyr::full_join(dplyr::select(dat$exposures, 
                                                   dplyr::all_of(
                                                     c(params_dat$variables$identifier, 
                                                     exposure)
                                                   )), 
                                     dat_merged, 
                                     by = params_dat$variables$identifier) |>
      dplyr::select(-params_dat$variables$identifier)
    
    if (params_ana$estimator == "tmle") {
      warning("In lmtp_tmle, some arguments have to be defined.")
      lmtp::lmtp_tmle(data = dat_analysis, 
                      trt = c(exposure), 
                      outcome = outcome, 
                      baseline = baseline, 
                      cens = c("cens"), 
                      shift = shift_func, 
                      k = params_ana$k, 
                      mtp = TRUE, 
                      outcome_type = "continuous", 
                      #bounds = , 
                      learners_outcome = params_ana$learners_outcome, 
                      learners_trt = params_ana$learners_trt, 
                      folds = params_ana$folds, 
                      #weights = , 
                      #.bound = , 
                      .trim = params_ana$.trim, 
                      #.learners_outcome_folds = params_ana$.learners_outcome_folds, 
                      #.learners_trt_folds = params_ana$.learners_trt_folds, 
                      .return_full_fits = TRUE)
    } else if (params_ana$estimator == "sdr") {
      warning("In lmtp_sdr, some arguments have to be defined.")
      lmtp::lmtp_sdr(data = dat_analysis, 
                     trt = c(exposure), 
                     outcome = outcome, 
                     baseline = baseline, 
                     #time_vary = , 
                     cens = c("cens"), 
                     shift = shift_func, 
                     #shifted = , 
                     k = params_ana$k, 
                     mtp = TRUE, 
                     outcome_type = "continuous", 
                     #id = , 
                     #bounds = , 
                     learners_outcome = params_ana$learners_outcome, 
                     learners_trt = params_ana$learners_trt, 
                     folds = params_ana$folds, 
                     #weights = , 
                     #.bound = , 
                     .trim = params_ana$.trim, 
                     #.learners_outcome_folds = , 
                     #.learners_trt_folds = , 
                     .return_full_fits = TRUE)
    }
  })
  names(res) <- list_exposures
  
  return(res)
} # End function run_mtp
################################################################################
