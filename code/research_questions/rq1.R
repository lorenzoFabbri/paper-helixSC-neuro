#' Pre-process data for RQ1
#'
#' @param dat
#'
#' @return
#' @export
rq1_prepare_data <- function(dat) {
  params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
  steps_exposures <- params_dat$variables$preproc_exposures
  steps_covars <- params_dat$variables$preproc_covars
  
  # Process exposures
  dat$exposures <- myphd::extract_cohort(dat = dat$exposures, 
                                         id_var = params_dat$variables$identifier)
  dat$exposures <- myphd::preproc_data(dat = dat$exposures, 
                                       dic_steps = steps_exposures, 
                                       id_var = params_dat$variables$identifier, 
                                       by_var = "cohort")
  
  # Process covariates
  warning("Some covariates are modified by name. ", 
          "This is not reproducible.")
  # dat$covariates$h_folic_t1 <- as.integer(dat$covariates$h_folic_t1)
  # dat$covariates$e3_ses <- as.integer(dat$covariates$e3_ses)
  # dat$covariates$e3_asmokyn_p <- as.integer(dat$covariates$e3_asmokyn_p)
  dat$covariates <- myphd::preproc_data(dat = dat$covariates, 
                                        dic_steps = steps_covars, 
                                        id_var = params_dat$variables$identifier, 
                                        by_var = "cohort")
  
  return(dat)
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
