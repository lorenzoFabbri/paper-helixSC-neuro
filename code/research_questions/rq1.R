# Script for research question 1 of causal roadmap

#' Load and process DAG for RQ1
#'
#' @param dags A list of DAGs. A list.
#' @param exposure A string defining the exposure of interest. A string.
#' @param outcome A string defining the outcome of interest. A string.
#' @param params_dag A list of parameters for `dagitty`. A list.
#'
#' @return A list.
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

rq1_load_data <- function(params, which_expo, which_covars, which_outcome) {
  # Load exposures and covariates for time point of interest
  dat_from_expo <- load_exposome(path_in = params$paths[[which_expo]])
  
  # Load eventual other covariates
  other_covars <- lapply(which_covars, function(.path) {
    
  })
  
  # Load outcomes
  
  # Create one dataset for covariates, one for exposures, and one for outcomes
  dat <- list()
  
  dat$exposures <- exposures
  dat$covariates <- covariates
  dat$outcomes <- outcomes
  
  return(dat)
} # End function load data

rq1_prepare_data <- function(dat, metadat) {
} # End function prepare data

################################################################################

rq1_describe_data <- function(dat, metadat) {
  
} # End function describe data

################################################################################

rq1_run_mtp <- function(params_analyses) {
} # End function run analysis mtp

rq1_run_out_neg_control <- function(params_analyses) {
} # End function run analysis negative control

################################################################################

rq1_run_analyses <- function(params_analyses) {
} # End function run analyses
