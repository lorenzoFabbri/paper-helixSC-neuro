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
                      "rq1" = "chem_to_out")
  
  dag <- dags[[which_dag]]
  ret$dag <- dag
  
  # Find the adjustment set(s) of interest
  adjustment_set <- dagitty::adjustmentSets(x = dag, 
                                            exposure = exposure, 
                                            outcome = outcome, 
                                            type = params_dag$type, 
                                            effect = params_dag$effect)
  ret$adjustment_sets <- adjustment_set
  
  return(ret)
} # End function dag
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
} # End function selection adjustment set
################################################################################

#' Load data necessary for down-streams analyses
#'
#' @param ids_other_covars IDs for additional covariate data. A vector.
#' @param res_dag Result of the call to [load_dag()]. A list.
#'
#' @return A named list of exposures, covariates, and outcomes. A list.
#'
#' @export
rq_load_data <- function(ids_other_covars, res_dag) {
  rq <- Sys.getenv("TAR_PROJECT")
  
  # Load data request
  params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
  dat_request <- load_dat_request(paths = params_dat$paths)
  
  # Load eventual other covariates
  if (!is.null(ids_other_covars)) {
    other_covars <- lapply(which_covars, function(.path) {})
  }
  
  # Create one dataset for covariates, one for exposures, and one for outcomes
  dat <- list()
  dat$exposures <- dat_request$dat |>
    dplyr::select(params_dat$variables$identifier, 
                  dplyr::contains(params_dat$variables[[rq]]$exposures, 
                                  ignore.case = TRUE))
  dat$outcome <- dat_request$dat |>
    dplyr::select(params_dat$variables$identifier, 
                  params_dat$variables[[rq]]$outcome)
  adj_set <- select_adjustment_set(dat = dat_request$dat, 
                                   meta = dat_request$meta, 
                                   res_dag = res_dag, 
                                   strategy = params_dat$variables$strategy_select_adj_set)
  mapping_covars <- dat_request$meta[dat_request$meta$dag %in% adj_set, 
                                     ]$variable |>
    as.character()
  dat$adjustment_set <- adj_set
  dat$mapping_covariates <- mapping_covars
  dat$covariates <- dat_request$dat |>
    dplyr::select(params_dat$variables$identifier, 
                  dplyr::all_of(mapping_covars))
  
  return(dat)
} # End function load data
################################################################################

#' Title
#'
#' @param dat
#'
#' @return
#'
#' @export
run_marginal_effects <- function(dat) {
  rq <- Sys.getenv("TAR_PROJECT")
  params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
  id_var <- params_dat$variables$identifier
  outcome <- params_dat$variables[[rq]]$outcome
  steps_outcome <- params_dat$variables$preproc_outcome
  params_ana <- params_analyses()[[rq]]
  
  # Step 1a: estimate weights for covariate balance
  list_exposures <- dat$exposures |>
    dplyr::select(-dplyr::any_of("HelixID")) |>
    colnames()
  list_covariates <- setdiff(colnames(dat$covariates), id_var)
  estimated_weights <- lapply(list_exposures, function(x) {
    tmp <- suppressWarnings(myphd::estimate_weights(
      dat = dplyr::inner_join(dat$exposures, 
                              dat$covariates, 
                              by = id_var), 
      exposure = x, 
      covariates = list_covariates, 
      method = params_ana$method_weightit, 
      method_args = list(
        use_kernel = params_ana$use_kernel, 
        sl_discrete = params_ana$sl_discrete, 
        sl_lib = params_ana$sl_lib
      ))
    )
    WeightIt::trim(tmp$weights, params_ana$weights_trim)
  }) # End loop over exposures to estimate weights
  names(estimated_weights) <- list_exposures
  ##############################################################################
  
  # Step 1b: explore balance
  balance <- lapply(names(estimated_weights), function(x) {
    myphd::explore_balance(exposure = strsplit(x, split = "_")[[1]][2], 
                           covariates = list_covariates, 
                           weights = estimated_weights[[x]])
  })
  names(balance) <- names(estimated_weights)
  ## Save results
  lapply(names(balance), function(x) {
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
                    dpi = 480)
    
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
  })
  
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
                  dpi = 480, height = 6)
  ##############################################################################
  
  # Step 2: fit model(s) using estimated weights
  
  # Step 3: estimate marginal effects
  
  return(list(
    estimated_weights = estimated_weights, 
    balance = balance
  ))
} # End function run_marginal_effects
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
  steps_outcome <- params_dat$variables$preproc_outcome
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
    dat_analysis <- dplyr::bind_cols(dplyr::select(dat$exposures, 
                                                   dplyr::all_of(exposure)), 
                                     dat_merged) |>
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
} # End function run analysis mtp
################################################################################
