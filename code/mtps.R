#' Additive shift function
#'
#' @param dat A dataset containing the variable to be shifted. A tibble.
#' @param trt The name of the variable to be shifted. A string.
#'
#' @return
#'
#' @export
mtp_add <- function(dat, trt) {
  params_ana <- params_analyses()[[Sys.getenv("TAR_PROJECT")]]
  
  dd <- dat[[trt]] + params_ana$shift_amount
  
  return(dd)
}

#' Multiplicative shift function
#'
#' @param dat A dataset containing the variable to be shifted. A tibble.
#' @param trt The name of the variable to be shifted. A string.
#'
#' @return
#'
#' @export
mtp_mul <- function(dat, trt) {
  params_ana <- params_analyses()[[Sys.getenv("TAR_PROJECT")]]
  
  dd <- dat[[trt]] * params_ana$shift_amount
  
  return(dd)
}
