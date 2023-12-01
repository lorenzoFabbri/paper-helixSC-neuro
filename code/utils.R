#' Load and process DAG
#'
#' @param dags A list of DAGs. A list.
#' @param exposure A string defining the exposure of interest. A string.
#' @param outcome A string defining the outcome of interest. A string.
#' @param params_dag A list of parameters for `dagitty`. A list.
#'
#' @returns A named list with DAG and adjustment sets.
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
                      "rq3" = "marker_to_out",
                      "rq04" = "marker_to_chem",
                      "rq4" = "marker_to_chem"
  )
  
  dag <- dags[[which_dag]]
  
  # Find the adjustment set(s) of interest
  adjustment_set <- dagitty::adjustmentSets(
    x = dag,
    exposure = exposure,
    outcome = outcome,
    type = params_dag$type,
    effect = params_dag$effect
  )
  
  # Identify precision covariates to add to adjustment set
  prec_covars <- dagitty::parents(
    x = dag,
    v = outcome
  )
  parents_exposure <- dagitty::parents(
    x = dag,
    v = exposure
  )
  prec_covars <- setdiff(
    setdiff(
      prec_covars,
      exposure
    ),
    parents_exposure
  )
  
  return(list(
    dag = dag,
    adjustment_sets = adjustment_set,
    prec_covars = prec_covars
  ))
} # End function load_dag
################################################################################

#' Various ways to select an adjustment set out of many
#'
#' @param dat A named list of dataframes containing the
#' variables of interest. A list.
#' @param meta A dataframe of metadata for `dat`. A dataframe.
#' @param res_dag Result of the call to [load_dag()]. A named list.
#' @param strategy Strategy to adopt to select the adjustment set.
#' The currently available strategies are: `first`, `smallest`, `largest`, `random`,
#' and `minimize_missings`. A string.
#'
#' @returns An adjustment set.
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
                  adjustment_sets = all_as,
                  by_var = "cohort"
                )]]
  )
  
  return(ret)
} # End function select_adjustment_set
################################################################################

#' Load data necessary for down-streams analyses
#'
#' @param res_dag Result of the call to [load_dag()]. A list.
#'
#' @returns A named list of exposures, covariates, and outcomes.
#'
#' @export
rq_load_data <- function(res_dag) {
  rq <- Sys.getenv("TAR_PROJECT")
  rq <- switch(rq,
               "rq01" = "rq1",
               "rq02" = "rq2",
               "rq03" = "rq3",
               "rq04" = "rq4",
               rq
  )
  
  # Load data request
  params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
  dat_request <- load_dat_request()
  
  dat <- list()
  ## Eventually load also steroid data
  if (Sys.getenv("TAR_PROJECT") %in% c(
    "rq02", "rq03", "rq04", "rq2", "rq3", "rq4"
  )) {
    metabolites <- load_steroids()
    
    # Subjects with metabolites available but not in HELIX data
    ids_metabs <- setdiff(
      metabolites$metabolome[[params_dat$variables$identifier]],
      dat_request$dat[[params_dat$variables$identifier]]
    )
    l_ids_metabs <- length(ids_metabs)
    if (l_ids_metabs > 0) {
      warning(
        glue::glue(
          "Mismatch IDs metabolomics and HELIX (n={l_ids_metabs}):\n{ids_metabs}",
          ids_metabs = paste(ids_metabs, collapse = ", ")
        ),
        call. = TRUE
      )
    }
    
    dat$metab_desc <- metabolites[["desc"]]
    dat$metab_desc <- dat$metab_desc |>
      dplyr::rename_with(.fn = ~ gsub("_cdesc", "", .x, fixed = TRUE))
    dat$lods <- metabolites[["loq"]]
    colnames(dat$lods) <- c("var", "val")
    dat_request$dat <- tidylog::inner_join(
      dat_request$dat,
      metabolites$metabolome,
      by = params_dat$variables$identifier
    )
    dat_request$dat$cohort <- droplevels(dat_request$dat$cohort)
    dat$metab_desc <- dat$metab_desc |>
      tidylog::filter(.data[[params_dat$variables$identifier]] %in%
                        dat_request$dat[[params_dat$variables$identifier]])
    dat$metab_desc <- dat$metab_desc[match(
      dat_request$dat[[params_dat$variables$identifier]],
      dat$metab_desc[[params_dat$variables$identifier]]
    ), ]
    dat$metab_desc <- tibble::as_tibble(dat$metab_desc)
    assertthat::assert_that(
      identical(
        dat_request$dat[[params_dat$variables$identifier]],
        dat$metab_desc[[params_dat$variables$identifier]]
      ),
      msg = "Mismatch order rows description metabolites and data request."
    )
  } # End check if need to load metabolites
  
  # Create one dataset for covariates, one for exposures, and one for outcomes
  miss_exps <- setdiff(
    params_dat$variables[[rq]]$exposures,
    colnames(dat_request$dat)
  )
  l_miss_exps <- length(miss_exps)
  if (l_miss_exps > 0) {
    warning(
      glue::glue(
        "Missing exposures (n={l_miss_exps}):\n{miss_exps}",
        miss_exps = paste(miss_exps, collapse = ", ")
      ),
      call. = TRUE
    )
  }
  
  ## Exposures
  dat$exposures <- dat_request$dat |>
    tidylog::select(
      params_dat$variables$identifier,
      dplyr::any_of(params_dat$variables[[rq]]$exposures)
    )
  
  ## Outcome
  dat$outcome <- dat_request$dat |>
    tidylog::select(
      params_dat$variables$identifier,
      dplyr::any_of(params_dat$variables[[rq]]$outcome)
    )
  
  ## Description of metabolites
  if (rq == "rq2") {
    mets_to_select <- params_dat$variables[[rq]]$outcome
  } else if (rq %in% c("rq3", "rq4")) {
    mets_to_select <- params_dat$variables[[rq]]$exposures
  }
  if (rq %in% c("rq2", "rq3", "rq4")) {
    dat$metab_desc <- dat$metab_desc |>
      tidylog::select(
        params_dat$variables$identifier,
        dplyr::all_of(mets_to_select)
      )
  } else {
    dat$metab_desc <- NULL
  }
  
  ## Covariates
  if (length(res_dag$adjustment_sets) > 1) {
    adj_set <- select_adjustment_set(
      dat = dat_request$dat,
      meta = dat_request$meta,
      res_dag = res_dag,
      strategy = params_dat$variables$strategy_select_adj_set
    )
  } else {
    adj_set <- res_dag$adjustment_sets
  } # End choice adjustment set
  ### Add precision covariates
  adj_set <- c(adj_set, res_dag$prec_covars) |>
    unlist() |>
    unique()
  mapping_covars <-
    dat_request$meta[dat_request$meta$dag %in% adj_set, ]$variable |>
    as.character()
  dat$adjustment_set <- c(adj_set)
  dat$mapping_covariates <- mapping_covars
  dat$covariates <- dat_request$dat |>
    tidylog::select(
      params_dat$variables$identifier,
      dplyr::all_of(mapping_covars)
    )
  cols_to_season <- c("hs_date_neu", "e3_cbirth")
  dat$covariates <-
    myphd::convert_time_season(
      dat = dat$covariates,
      cols = cols_to_season
    ) |>
    tidylog::mutate(dplyr::across(
      dplyr::any_of(cols_to_season),
      \(x) factor(x)
    ))
  
  ## Metadata
  warning(
    "Add meta to dat.",
    call. = TRUE
  )
  dat$meta <- dat_request$meta
  
  # Checks
  if (!is.null(dat$metab_desc)) {
    count_values <- dat$metab_desc |>
      tidylog::select(-HelixID) |>
      tidyr::pivot_longer(cols = dplyr::everything()) |>
      dplyr::count(name, value) |>
      tidylog::filter(value %in% c(2, 4))
    count_not_values <- dat$metab_desc |>
      tidylog::select(-HelixID) |>
      tidyr::pivot_longer(cols = dplyr::everything()) |>
      dplyr::count(name, value) |>
      tidylog::filter(value %in% c(3))
    if (rq %in% c("rq3", "rq4")) {
      sum_na <- sum(is.na(dat$exposures))
    } else if (rq %in% c("rq2")) {
      sum_na <- sum(is.na(dat$outcome))
    }
    assertthat::assert_that(
      sum(count_values$n) + sum(count_not_values$n) == sum_na,
      msg = "Mismatch in the number of missing values for the metabolome."
    )
  } # End check number of missings in metabolome
  
  return(dat)
} # End function rq_load_data
################################################################################

#' Pre-process data for research questions
#'
#' @param dat A named list of dataframes containing the
#' variables of interest. A list.
#'
#' @returns A named list of pre-processed dataframes
#' containing the variables of interest.
#'
#' @export
rq_prepare_data <- function(dat, filter_panel, type_sample_hcp, is_sa = FALSE) {
  rq <- Sys.getenv("TAR_PROJECT")
  rq <- switch(rq,
               "rq01" = "rq1",
               "rq02" = "rq2",
               "rq03" = "rq3",
               "rq04" = "rq4",
               rq
  )
  params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
  steps_covars <- params_dat$steps[[rq]]$preproc_covars
  steps_exposures <- params_dat$steps[[rq]]$preproc_exposures
  steps_outcome <- params_dat$steps[[rq]]$preproc_outcome
  params_ana <- params_analyses()[[rq]]
  # Create folders to store results
  invisible(lapply(c("figures"), function(x) {
    path_save_res <- paste0(
      "results/", x, "/",
      dplyr::case_when(
        filter_panel == TRUE ~ paste0(rq, "_HCP"),
        is_sa == TRUE ~ paste0(rq, "_SA"),
        .default = rq
      )
    )
    if (!dir.exists(path_save_res)) {
      dir.create(path_save_res)
    }
  }))
  
  ##############################################################################
  # Eventually estimate weights for selection into child panel
  if (rq == "rq2" & filter_panel == TRUE) {
    # Load CP data and pre-process chemicals
    if (is.null(type_sample_hcp)) {
      type_sample_hcp <- params_dat$variables$type_sample_hcp
    }
    dat_cp <- load_cp_data(which_sample = type_sample_hcp) |>
      myphd::extract_cohort(
        id_var = params_dat$variables$identifier
      ) |>
      myphd::preproc_data(
        covariates = NULL,
        outcome = NULL,
        dic_steps = steps_exposures,
        id_var = params_dat$variables$identifier,
        by_var = "cohort"
      ) |>
      tidylog::select(-cohort)
    
    # Estimate selection weights
    dat$selection_weights <- myphd::estimate_selection_weights(
      dat = myphd::preproc_data(
        dat = dat$covariates,
        covariates = NULL,
        outcome = NULL,
        dic_steps = steps_covars,
        id_var = params_dat$variables$identifier,
        by_var = "cohort"
      ),
      idxs_selected = dat_cp[[params_dat$variables$identifier]],
      id_var = params_dat$variables$identifier,
      filter_out = list(
        cohort = c("MOBA")
      ),
      list_covars = params_dat$variables$selection_covariates_names,
      method = "glm",
      method_args = list(
        stabilize = params_ana$stabilize,
        by = NULL,
        sl_lib = params_ana$sl_lib,
        sl_discrete = params_ana$sl_discrete,
        use_kernel = params_ana$use_kernel,
        family_link = params_ana$family_link_weightit
      ),
      trim_weights = ifelse(
        !is.null(params_ana$weights_trim),
        TRUE, FALSE
      ),
      threshold_trim = params_ana$weights_trim
    ) # End estimation selection weights
    
    # Filter out subjects not in HCP
    dat <- lapply(dat, function(x) {
      if (!is.null(colnames(x)) & "HelixID" %in% colnames(x)) {
        x |>
          tidylog::filter(
            HelixID %in% dat_cp[[params_dat$variables$identifier]]
          ) |>
          droplevels()
      } else {
        x
      }
    }) # End loop filtering HCP
    
    # Replace chemicals with those in HCP
    dat_cp <- dat_cp[match(dat$covariates$HelixID, dat_cp$HelixID), ] |>
      tibble::as_tibble()
    dat$exposures <- dat_cp
  } # End if for filter panel weights
  ##############################################################################
  
  ##############################################################################
  # Process covariates
  dat$covariates <- myphd::preproc_data(
    dat = dat$covariates,
    covariates = NULL,
    outcome = NULL,
    dic_steps = steps_covars,
    id_var = params_dat$variables$identifier,
    by_var = "cohort"
  )
  ## Check if some factors have only one level and drop them
  dat$covariates <- dat$covariates |>
    tidylog::select(
      dplyr::where(\(x) dplyr::n_distinct(x) > 1)
    )
  ##############################################################################
  
  ##############################################################################
  # Process exposures
  dat$exposures <- myphd::preproc_data(
    dat = myphd::extract_cohort(
      dat = dat$exposures,
      id_var = params_dat$variables$identifier
    ),
    dat_desc = myphd::extract_cohort(
      dat = dat$metab_desc,
      id_var = params_dat$variables$identifier
    ),
    covariates = dat$covariates,
    outcome = NULL,
    dat_llodq = dat$lods,
    dic_steps = steps_exposures,
    id_var = params_dat$variables$identifier,
    by_var = "cohort"
  ) |>
    tidylog::select(-cohort)
  ## Eventually add scores and pre-process them
  if (rq %in% c("rq3", "rq4")) {
    cols_to_remove <- setdiff(
      colnames(dat$exposures),
      c(params_dat$variables$identifier,
        "F", "cortisol_production", "CortisoneE",
        "X5bTHS", "X5aTHB", "X5bTHB",
        "Etio", "X17HP", "PT")
    )
    dat$exposures <- create_steroid_scores(dat = dat$exposures)
    
    # Pre-process scores
    dat$exposures <- myphd::handle_transformation(
      dat = myphd::extract_cohort(
        dat$exposures,
        id_var = "HelixID"
      ),
      id_var = params_dat$variables$identifier,
      by_var = "cohort",
      transformation_fun = log
    ) |>
      tidylog::select(-cohort) |>
      tidylog::select(-dplyr::all_of(cols_to_remove))
  }
  ##############################################################################
  
  ##############################################################################
  # Process outcome
  dat$outcome <- myphd::preproc_data(
    dat = myphd::extract_cohort(
      dat = dat$outcome,
      id_var = params_dat$variables$identifier
    ),
    dat_desc = myphd::extract_cohort(
      dat = dat$metab_desc,
      id_var = params_dat$variables$identifier
    ),
    covariates = dat$covariates,
    outcome = outcome,
    dat_llodq = dat$lods,
    dic_steps = steps_outcome,
    id_var = params_dat$variables$identifier,
    by_var = "cohort"
  ) |>
    tidylog::select(-cohort)
  ## Eventually add scores and pre-process them
  if (rq == "rq2") {
    cols_to_remove <- setdiff(
      colnames(dat$outcome),
      c(params_dat$variables$identifier,
        "F", "cortisol_production", "CortisoneE",
        "X5bTHS", "X5aTHB", "X5bTHB",
        "Etio", "X17HP", "PT")
    )
    dat$outcome <- create_steroid_scores(dat = dat$outcome)
    
    # Pre-process scores
    dat$outcome <- myphd::handle_transformation(
      dat = myphd::extract_cohort(
        dat$outcome,
        id_var = "HelixID"
      ),
      id_var = params_dat$variables$identifier,
      by_var = "cohort",
      transformation_fun = log
    ) |>
      tidylog::select(-cohort) |>
      tidylog::select(-dplyr::all_of(cols_to_remove))
  }
  ##############################################################################
  
  return(dat)
} # End function rq_prepare_data
################################################################################

#' Estimate weights and explore covariates balance
#'
#' @param dat A named list of dataframes containing
#' the variables of interest. A list.
#' @param by
#' @param save_results Whether to save figures and tables to file. A logical.
#' @param parallel Whether to perform steps in parallel. A logical.
#' @param workers Optional number of workers to use. An integer.
#'
#' @returns A named list containing estimated weights and results of
#' balance exploration.
#'
#' @export
rq_estimate_weights <- function(dat, by = NULL,
                                include_selection_weights,
                                save_results,
                                parallel, workers = NULL) {
  rq <- Sys.getenv("TAR_PROJECT")
  rq <- switch(rq,
               "rq01" = "rq1",
               "rq02" = "rq2",
               "rq03" = "rq3",
               "rq04" = "rq4",
               rq
  )
  params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
  params_ana <- params_analyses()[[rq]]
  
  ##############################################################################
  # Step 0: decide whether to include selection weights or not
  if (rq == "rq2" & include_selection_weights == TRUE) {
    s.weights <- c(dat$selection_weights$selection_weights)
  } else {
    s.weights <- NULL
  }
  ##############################################################################
  
  ##############################################################################
  # Step 1: estimate weights for covariate balance
  if (rq %in% c("rq3", "rq4")) {
    list_exposures <- vars_of_interest()$new_metabolites
  } else {
    list_exposures <- names(dat$exposures)
  }
  list_exposures <- setdiff(
    list_exposures,
    params_dat$variables$identifier
  )
  list_covariates <- setdiff(
    colnames(dat$covariates),
    params_dat$variables$identifier
  )
  dat_analysis <- tidylog::full_join(
    dat$covariates,
    dat$exposures,
    by = params_dat$variables$identifier
  )
  
  ## Loop over exposures
  if (parallel == TRUE) {
    future::plan(future::multisession,
                 workers = workers
    )
  }
  progressr::with_progress({
    p <- progressr::progressor(steps = length(list_exposures))
    
    estimated_weights <-
      furrr::future_map(list_exposures, function(x, p) {
        p()
        
        tmp <-
          suppressWarnings(suppressMessages(
            myphd::estimate_weights(
              dat = dat_analysis |>
                tidylog::select(dplyr::all_of(
                  c(
                    params_dat$variables$identifier,
                    x,
                    list_covariates
                  )
                )),
              exposure = x,
              covariates = list_covariates,
              s.weights = s.weights,
              id_var = params_dat$variables$identifier,
              method = params_ana$method_weightit,
              method_args = list(
                stabilize = params_ana$stabilize,
                by = by,
                sl_lib = params_ana$sl_lib,
                sl_discrete = params_ana$sl_discrete,
                use_kernel = params_ana$use_kernel,
                family_link = NULL
              )
            )
          )) # End estimation weights current exposure
        # Eventually trim weights
        if (!is.null(params_ana$weights_trim)) {
          if (rq == "rq4") {
            weights_trim <- 0.8
          } else {
            weights_trim <- params_ana$weights_trim
          }
          ret <- suppressMessages(WeightIt::trim(
            tmp$weights,
            at = weights_trim,
            lower = TRUE
          ))
        } else {
          ret <- tmp$weights
        }
        # Include IDs for safer merging during model fitting
        ret[[params_dat$variables$identifier]] <-
          dat_analysis[[params_dat$variables$identifier]]
        
        return(ret)
      },
      .options = furrr::furrr_options(seed = TRUE),
      p = p
      ) # End loop over exposures to estimate weights
  }) # End progress bar
  names(estimated_weights) <- list_exposures
  
  if (rq == "rq2" & include_selection_weights == TRUE) {
    assertthat::assert_that(
      identical(estimated_weights[[1]]$HelixID, dat$selection_weights$HelixID),
      msg = "Something went wrong when computing selection weights."
    )
  }
  ##############################################################################
  
  ##############################################################################
  # Step 2: explore balance
  balance <-
    furrr::future_map(names(estimated_weights), function(x) {
      myphd::explore_balance(
        exposure = ifelse(rq %in% c("rq1", "rq2"),
                          strsplit(x, split = "_")[[1]][2],
                          x
        ),
        covariates = estimated_weights[[x]]$covs |>
          colnames(),
        weights = estimated_weights[[x]],
        type_exposure = "continuous",
        threshold_cor = 0.1
      )[c("exposure", "tab", "love")]
    },
    .options = furrr::furrr_options(seed = TRUE)
    ) # End loop explore balance
  names(balance) <- names(estimated_weights)
  
  ## Save results
  if (save_results) {
    ### love.plot
    .path.rq <- dplyr::case_when(
      include_selection_weights == TRUE ~ paste0(rq, "_HCP"),
      !is.null(by) ~ paste0(rq, "_SA"),
      .default = rq
    )
    .path <- paste0(
      "results/figures/",
      .path.rq,
      "/loveplot_",
      ifelse(
        !is.null(by),
        paste0(by, "_"), ""
      ),
      params_ana$method_weightit,
      ".pdf"
    )
    ggplot2::ggsave(
      .path,
      gridExtra::marrangeGrob(
        grobs = lapply(balance, `[[`, "love"),
        nrow = 1,
        ncol = 1
      ),
      dpi = 360,
      height = 6
    )
  } # End save_results
  ##############################################################################
  
  balance <- lapply(balance, function(x) {
    x$love <- NULL
  })
  return(list(
    estimated_weights = estimated_weights,
    balance = balance
  ))
} # End function rq_estimate_weights
################################################################################

#' Fit models with balancing weights
#'
#' @param dat A named list of dataframes containing
#' the variables of interest. A list.
#' @param outcome Name of the outcome of interest. A string.
#' @param by
#' @param weights Estimated weights resulting from a call to
#' [rq_estimate_weights()]. A list.
#' @param parallel Whether to perform steps in parallel. A logical.
#' @param workers Optional number of workers to use. An integer.
#'
#' @returns A named list containing the fitted models.
#'
#' @export
rq_fit_model_weighted <- function(dat, outcome,
                                  by = c(), is_panel = FALSE,
                                  weights,
                                  parallel, workers = NULL) {
  rq <- Sys.getenv("TAR_PROJECT")
  params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
  params_ana <- params_analyses()[[rq]]
  
  # Check whether outcome is still available after pre-processing
  if (!outcome %in% colnames(dat$outcome)) {
    warning(glue::glue("The outcome {outcome} was not found."),
            call. = TRUE
    )
    return()
  }
  
  # Eventually add control for denominator of outcome
  if (rq == "rq2") {
    den <- switch(outcome,
                  "cortisol_metabolism" = "F",
                  "cortisone_metabolism" = "CortisoneE",
                  "X11bHSD" = "cortisol_production",
                  "global_reductase_f" = "F",
                  "global_reductase_e" = "CortisoneE",
                  "cyp3a4" = "F",
                  "X11hydroxylase" = "X5bTHS",
                  "X17hydroxylase" = c("X5aTHB", "X5bTHB"),
                  "X5a_reductase" = "Etio",
                  "lyase" = c("X17HP", "PT"),
                  "global_adrenal_function" = c("X17HP", "PT"),
                  NULL
    )
    dat$covariates <- dat$covariates |>
      tidylog::full_join(
        dat$outcome |>
          tidylog::select(
            HelixID,
            dplyr::all_of(den)
          ),
        by = params_dat$variables$identifier
      )
  }
  
  # Process outcome
  dat$outcome <- dat$outcome |>
    tidylog::select(dplyr::all_of(c(
      params_dat$variables$identifier,
      outcome
    )))
  if (rq %in% c("rq3", "rq4")) {
    list_exposures <- vars_of_interest()$new_metabolites
  } else {
    list_exposures <- names(dat$exposures)
  }
  list_exposures <- setdiff(
    list_exposures,
    params_dat$variables$identifier
  )
  list_covariates <- setdiff(
    colnames(dat$covariates),
    params_dat$variables$identifier
  )
  dat_analysis <- purrr::reduce(
    list(dat$covariates, dat$exposures, dat$outcome),
    tidylog::full_join,
    by = params_dat$variables$identifier
  )
  
  idxs_missing_outcome <- which(is.na(dat_analysis[[outcome]]))
  ids_missing_outcome <- dat_analysis[idxs_missing_outcome, ][[params_dat$variables$identifier]]
  
  # Fit model(s) using estimated weights
  ## Loop over each exposure
  if (parallel == TRUE) {
    future::plan(future::multisession,
                 workers = workers
    )
  } else {
    future::plan(future::sequential())
  }
  progressr::with_progress({
    p <- progressr::progressor(steps = length(list_exposures))
    
    fits <-
      furrr::future_map(list_exposures, function(exposure, p) {
        p()
        
        # Select columns of interest and make sure order rows same in
        # dataset and estimated weights
        dat_tmp <- dat_analysis |>
          tidylog::select(dplyr::all_of(
            c(
              params_dat$variables$identifier,
              list_covariates,
              exposure,
              outcome
            )
          ))
        assertthat::assert_that(
          identical(
            dat_tmp[[params_dat$variables$identifier]],
            weights[[exposure]][[params_dat$variables$identifier]]
          ),
          msg = "The order of the rows changed between weights estimation
                and outcome model fitting."
        )
        
        fit <- myphd::fit_model_weighted(
          dat = dat_tmp,
          outcome = outcome,
          exposure = exposure,
          covariates = list_covariates,
          id_var = params_dat$variables$identifier,
          weights = weights[[exposure]]$weights,
          method = params_ana$method_marginal,
          method_args = list(
            family = params_ana$family_marginal,
            add_inter_exposure = params_ana$add_inter_exposure,
            add_inter_exposure_specific = by,
            add_splines_exposure = params_ana$add_splines_exposure,
            df_splines = params_ana$df_splines,
            threshold_smooth = params_ana$threshold_smooth,
            threshold_k = params_ana$threshold_k
          )
        ) # End fit current exposure
        .path.rq <- dplyr::case_when(
          is_panel == TRUE ~ paste0(rq, "_HCP"),
          !is_empty(by) ~ paste0(rq, "_SA"),
          .default = rq
        )
        path_save_res <- glue::glue(
          "results/figures/{.path.rq}/model_check_out_{outcome}_{exposure}.png"
        )
        check_mod_out <- myphd::check_model(
          model = fit$fit,
          path_save_res = path_save_res
        )
        
        return(list(
          fit = fit$fit,
          dat = dat_tmp,
          weights = weights[[exposure]]$weights
        ))
      },
      .options = furrr::furrr_options(seed = TRUE),
      p = p
      ) # End loop over exposures to fit weighted models
  }) # End progress bar
  future::plan(future::sequential)
  names(fits) <- list_exposures
  
  return(list(fits = fits))
} # End function rq_fit_model_weighted
################################################################################

#' Estimate marginal effects
#'
#' @param fits A list of fitted models. Results from a call to
#' [rq_fit_model_weighted()]. A list.
#' @param by
#' @param parallel Whether to perform steps in parallel. A logical.
#' @param workers Optional number of workers to use. An integer.
#'
#' @returns A list of estimated marginal effects. Specifically, the
#' average dose-response function, the average marginal effect function,
#' and the results of average comparisons.
#'
#' @export
rq_estimate_marginal_effects <- function(fits,
                                         by = NULL, is_hcp = FALSE,
                                         parallel,
                                         workers = NULL) {
  rq <- Sys.getenv("TAR_PROJECT")
  params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
  params_ana <- params_analyses()[[rq]]
  
  old_by <- by
  if (is.null(by)) {
    by <- TRUE
  } else {
    by <- glue::double_quote(by)
  }
  
  # Loop over the fitted models to estimate marginal effects
  if (parallel == TRUE) {
    future::plan(future::multisession,
                 workers = workers
    )
  } else {
    future::plan(future::sequential())
  }
  
  ret <- furrr::future_map(seq_along(fits), function(idx) {
    exposure <- names(fits)[[idx]]
    mod <- fits[[exposure]]$fit
    dat <- fits[[exposure]]$dat
    weights <- fits[[exposure]]$weights
    
    ########################################################################
    # Covariance matrix for all steps
    if (is_hcp == FALSE) {
      vcov <- sandwich::vcovCL(
        x = mod,
        cluster = ~ cohort,
        type = "HC3"
      )
    } else {
      vcov <- sandwich::vcovCL(
        x = mod,
        cluster = ~ cohort,
        type = "HC1"
      )
    } # End choice/computation of covariance matrix
    
    # G-computation (ADRF)
    ## Values of exposure for counterfactual predictions, based on quantiles
    values <- with(dat, seq(
      quantile(get(exposure), params_ana$type_avg_comparison[1]),
      quantile(get(exposure), params_ana$type_avg_comparison[2]),
      length.out = 50
    ))
    gcomp <- eval(parse(
      text = glue::glue(
        "marginaleffects::avg_predictions(
            model = mod,
            variables = list(
              {exposure} = values
            ),
            wts = weights,
            vcov = vcov
          )",
        exposure = exposure
      )
    )) |> # End G-computation (avg_predictions)
      marginaleffects::tidy()
    ########################################################################
    
    ########################################################################
    # Slopes (AMEF)
    # weights_repeated <- rep(weights,
    #                         times = length(values)
    # )
    # slopes <- eval(parse(
    #   text = glue::glue(
    #     "marginaleffects::avg_slopes(
    #       model = mod,
    #       variables = {glue::double_quote(exposure)},
    #       newdata = marginaleffects::datagridcf({exposure} = values),
    #       by = {glue::double_quote(exposure)},
    #       wts = weights_repeated,
    #       vcov = {glue::double_quote(vcov)}
    #     )",
    #     exposure = exposure,
    #     vcov = "HC3"
    #   )
    # )) # End slopes (avg_slopes)
    ########################################################################
    
    ########################################################################
    # Comparisons (marginal estimates)
    ## Create dataframe with `high` and `low` values for exposure
    df_comparisons <- myphd::create_df_marginal_comparisons(
      dat = dat,
      var = exposure,
      percentiles = params_ana$type_avg_comparison,
      by_var = "cohort"
    )
    
    avg_comp <- eval(parse(
      text = glue::glue(
        "marginaleffects::avg_comparisons(
            model = mod,
            variables = list(
              {exposure} = df_comparisons
            ),
            by = {by},
            wts = weights,
            vcov = vcov
          )",
        exposure = exposure
      )
    )) # End marginal estimates (avg_comparisons)
    
    if (is_hcp == FALSE) {
      avg_comp <- avg_comp |>
        marginaleffects::tidy() |>
        tidylog::rename(
          variable = term,
          se = std.error
        )
    } else {
      # Since the sample size is small, use simulation-based inference
      avg_comp <- marginaleffects::inferences(
        x = avg_comp,
        method = "simulation",
        R = 299
      ) |>
        marginaleffects::tidy() |>
        tidylog::rename(
          variable = term,
          se = std.error
        )
    }
    
    avg_comp_hyp <- NULL
    if (!is.null(old_by)) {
      avg_comp_hyp <- eval(parse(
        text = glue::glue(
          "marginaleffects::avg_comparisons(
            model = mod,
            variables = list(
              {exposure} = df_comparisons
            ),
            by = {by},
            hypothesis = {hypothesis},
            wts = weights,
            vcov = vcov
          )",
          exposure = exposure,
          hypothesis = glue::double_quote("pairwise")
        )
      )) |> # End marginal estimates (avg_comparisons)
        marginaleffects::tidy() |>
        tidylog::rename(
          variable = term,
          se = std.error
        )
    }
    ########################################################################
    
    return(list(
      gcomp = gcomp,
      #adrf = adrf,
      #slopes = slopes,
      #amef = amef,
      comparisons = avg_comp,
      hypothesis = avg_comp_hyp
    ))
  },
  .options = furrr::furrr_options(seed = TRUE)
  ) # End loop over fitted models
  future::plan(future::sequential)
  names(ret) <- names(fits)
  
  return(list(marginal_effects = ret))
} # End function rq_estimate_marginal_effects
################################################################################

#' Fit models using `lmtp`
#'
#' @param dat A named list of tibbles containing the variables of interest. A list.
#' @param shift_exposure Whether to shift the exposure variables or not. A logical.
#'
#' @returns A named list containing the results of the call to `lmtp`, for each exposure.
#'
#' @export
run_mtp <- function(dat, shift_exposure) {
  rq <- Sys.getenv("TAR_PROJECT")
  params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
  outcome <- params_dat$variables[[rq]]$outcome
  steps_outcome <- params_dat$steps[[rq]]$preproc_outcome
  params_ana <- params_analyses()[[rq]]
  
  # Process outcome
  dat$outcome <- myphd::extract_cohort(
    dat = dat$outcome,
    id_var = params_dat$variables$identifier
  )
  dat$outcome <- myphd::preproc_data(
    dat = dat$outcome,
    outcome = outcome,
    dic_steps = steps_outcome,
    id_var = params_dat$variables$identifier,
    by_var = "cohort"
  )
  dat$outcome <- dat$outcome |>
    tidylog::select(-cohort)
  
  # Run lmtp
  if (shift_exposure == TRUE) {
    shift_func <- switch(params_ana$shift_type,
                         "mul" = mtp_mul,
                         "add" = mtp_add
    )
  } else {
    shift_func <- NULL
  }
  list_exposures <- names(dat$exposures)
  list_exposures <- setdiff(
    list_exposures,
    params_dat$variables$identifier
  )
  dat_merged <- tidylog::full_join(dat$covariates, dat$outcome,
                                   by = params_dat$variables$identifier
  ) |>
    tidylog::mutate(cens = as.integer(!is.na(.data[[outcome]])))
  baseline <- dat$covariates |>
    tidylog::select(-params_dat$variables$identifier) |>
    names()
  
  # Loop over each exposure
  res <- lapply(list_exposures, function(exposure) {
    dat_analysis <- tidylog::full_join(
      tidylog::select(
        dat$exposures,
        dplyr::all_of(
          c(
            params_dat$variables$identifier,
            exposure
          )
        )
      ),
      dat_merged,
      by = params_dat$variables$identifier
    ) |>
      tidylog::select(-params_dat$variables$identifier)
    
    if (params_ana$estimator == "tmle") {
      warning("In lmtp_tmle, some arguments have to be defined.")
      lmtp::lmtp_tmle(
        data = dat_analysis,
        trt = c(exposure),
        outcome = outcome,
        baseline = baseline,
        cens = c("cens"),
        shift = shift_func,
        k = params_ana$k,
        mtp = TRUE,
        outcome_type = "continuous",
        # bounds = ,
        learners_outcome = params_ana$learners_outcome,
        learners_trt = params_ana$learners_trt,
        folds = params_ana$folds,
        # weights = ,
        # .bound = ,
        .trim = params_ana$.trim,
        # .learners_outcome_folds = params_ana$.learners_outcome_folds,
        # .learners_trt_folds = params_ana$.learners_trt_folds,
        .return_full_fits = TRUE
      )
    } else if (params_ana$estimator == "sdr") {
      warning("In lmtp_sdr, some arguments have to be defined.")
      lmtp::lmtp_sdr(
        data = dat_analysis,
        trt = c(exposure),
        outcome = outcome,
        baseline = baseline,
        # time_vary = ,
        cens = c("cens"),
        shift = shift_func,
        # shifted = ,
        k = params_ana$k,
        mtp = TRUE,
        outcome_type = "continuous",
        # id = ,
        # bounds = ,
        learners_outcome = params_ana$learners_outcome,
        learners_trt = params_ana$learners_trt,
        folds = params_ana$folds,
        # weights = ,
        # .bound = ,
        .trim = params_ana$.trim,
        # .learners_outcome_folds = ,
        # .learners_trt_folds = ,
        .return_full_fits = TRUE
      )
    }
  })
  names(res) <- list_exposures
  
  return(res)
} # End function run_mtp
################################################################################
