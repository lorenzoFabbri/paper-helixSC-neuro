#' Title
#'
#' @param dat 
#' @param trt 
#'
#' @return
#' @export
mtp_add <- function(dat, trt) {
  params_ana <- params_analyses()[[Sys.getenv("TAR_PROJECT")]]
  
  dd <- dat[[trt]] + params_ana$shift_amount
  #dd[dd < params_ana$shift_lower_bound] <- params_ana$shift_lower_bound
  #dd[dd > params_ana$shift_upper_bound] <- params_ana$shift_upper_bound
  
  return(dd)
}

#' Title
#'
#' @param dat 
#' @param trt 
#'
#' @return
#' @export
mtp_mul <- function(dat, trt) {
  params_ana <- params_analyses()[[Sys.getenv("TAR_PROJECT")]]
  
  dd <- dat[[trt]] * params_ana$shift_amount
  #dd[dd < params_ana$shift_lower_bound] <- params_ana$shift_lower_bound
  #dd[dd > params_ana$shift_upper_bound] <- params_ana$shift_upper_bound
  
  return(dd)
}
