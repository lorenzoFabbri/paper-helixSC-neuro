#' Load and process DAG
#'
#' @param dags A list of DAGs. A list.
#' @param exposure A string defining the exposure of interest. A string.
#' @param outcome A string defining the outcome of interest. A string.
#' @param params_dag A list of parameters for `dagitty`. A list.
#'
#' @return A named list with DAG and adjustment sets. A list.
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

#' Title
#'
#' @param dat 
#' @param res_dag 
#' @param strategy 
#'
#' @return
#' @export
select_adjustment_set <- function(dat, meta, res_dag, strategy) {
  warning("When all the covariates are available, ", 
          "replace `any_of` with `all_of`.")
  
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

#' Load data
#'
#' @param ids_other_covars IDs for additional covariate data. A vector.
#' @param res_dag Result of call to `load_dag`. A list.
#'
#' @return A named list of exposures, covariates, and outcomes. A list.
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
  mapping_covars <- dat_request$meta[dat_request$meta$dag %in% adj_set, ]$variable
  dat$adjustment_set <- adj_set
  dat$mapping_covariates <- mapping_covars
  warning("When all the covariates are available, ", 
          "replace `any_of` with `all_of`.")
  dat$covariates <- dat_request$dat |>
    dplyr::select(params_dat$variables$identifier, 
                  dplyr::any_of(mapping_covars))
  
  return(dat)
} # End function load data
################################################################################

#' Title
#'
#' @param
#'
#' @return
#' @export
run_mtp <- function(dat) {
  rq <- Sys.getenv("TAR_PROJECT")
  params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
  outcome <- params_dat$variables[[rq]]$outcome
  steps_outcome <- params_dat$variables$preproc_outcome
  params_ana <- params_analyses()[[rq]]
  
  # Process outcome
  dat$outcome <- myphd::preproc_data(dat = dat$outcome, 
                                     outcome = outcome, 
                                     dic_steps = steps_outcome)
  
  # Run lmtp
  shift_func <- switch(params_ana$shift_type, 
                       "mul" = mtp_mul, 
                       "add" = mtp_add)
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
    }
  })
  names(res) <- list_exposures
  
  return(res)
} # End function run analysis mtp
################################################################################
