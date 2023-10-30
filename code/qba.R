#' Title
#'
#' @param mod 
#' @param exposure 
#' @param confounder 
#' @param path_save 
#'
#' @return
#' @export
use_sensemakr <- function(mod, exposure, confounder, path_save) {
  mod_sensitivity <- sensemakr::sensemakr(
    model = mod,
    treatment = exposure,
    benchmark_covariates = confounder,
    kd = 1:3,
    ky = 1:3,
    q = 1,
    alpha = 0.05,
    reduce = TRUE
  )
  
  return(list(
    sensitivity = mod_sensitivity
  ))
} # End function use_sensemakr
################################################################################
