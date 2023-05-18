#' Load and process DAG for RQ1
#'
#' @param dags A list of DAGs. A list.
#' @param exposure A string defining the exposure of interest. A string.
#' @param outcome A string defining the outcome of interest. A string.
#' @param params_dag A list of parameters for `dagitty`. A list.
#'
#' @return A named list with DAG and adjustment sets. A list.
#' @export
rq1_dag <- function(dags, exposure, outcome, params_dag) {
  ret <- list()
  
  dag <- dags$chem_to_out
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

#' Load data for RQ1
#'
#' @param ids_other_covars IDs for additional covariate data. A vector.
#' @param res_dag Result of call to `rq1_dag`. A list.
#'
#' @return A named list of exposures, covariates, and outcomes. A list.
#' @export
rq1_load_data <- function(ids_other_covars, res_dag) {
  # Load data request
  params_dat <- params()
  dat_request <- load_dat_request()
  
  # Load eventual other covariates
  if (!is.null(ids_other_covars)) {
    other_covars <- lapply(which_covars, function(.path) {})
  }
  
  # Create one dataset for covariates, one for exposures, and one for outcomes
  dat <- list()
  
  dat$exposures <- dat_request$dat |>
    dplyr::select(params_dat$variables$identifier, 
                  dplyr::contains(params_dat$variables$rq1$exposures, 
                                  ignore.case = TRUE))
  dat$outcome <- dat_request$dat |>
    dplyr::select(params_dat$variables$identifier, 
                  params_dat$variables$rq1$outcome)
  
  adj_set <- res_dag$adjustment_sets[[1]]
  mapping_covars <- dat_request$meta[dat_request$meta$dag %in% adj_set, 
                                     ]$variable
  warning("When all the covariates are available, ", 
          "replace `any_of` with `all_of`.")
  dat$covariates <- dat_request$dat |>
    dplyr::select(params_dat$variables$identifier, 
                  dplyr::any_of(mapping_covars))
  
  return(dat)
} # End function load data
################################################################################

#' Pre-process data for RQ1
#'
#' @param dat 
#' @param metadat 
#'
#' @return
#' @export
rq1_prepare_data <- function(dat, metadat) {
} # End function prepare data
################################################################################

#' Describe population for RQ1
#'
#' @param dat 
#' @param metadat 
#'
#' @return
#' @export
rq1_describe_data <- function(dat, metadat) {
} # End function describe data
################################################################################

#' Title
#'
#' @param params_analyses 
#'
#' @return
#' @export
rq1_run_mtp <- function(params_analyses) {
} # End function run analysis mtp
################################################################################

#' Title
#'
#' @param params_analyses 
#'
#' @return
#' @export
rq1_run_out_neg_control <- function(params_analyses) {
} # End function run analysis negative control
################################################################################

#' Title
#'
#' @param params_analyses 
#'
#' @return
#' @export
rq1_run_analyses <- function(params_analyses) {
} # End function run analyses
