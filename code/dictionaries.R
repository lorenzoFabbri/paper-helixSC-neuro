# Script containing dictionaries (e.g., exposures)

################################################################################
# Parameters for running analyses
################################################################################
params_analyses <- function() {
  # Common parameters
  learners_outcome <- c("SL.mean", "SL.glm", 
                        "SL.gam", "SL.hal9001", "SL.bart")
  learners_exposure <- learners_outcome
  estimator <- "tmle"
  folds <- 10
  folds_outcome <- 10
  folds_exposure <- 10
  density_ratio_trim <- 0.995
  markov_assumption <- Inf
  
  rq1 <- list(
    learners_outcome = learners_outcome, 
    learners_trt = learners_exposure, 
    estimator = estimator, 
    folds = folds, 
    .learners_outcome_folds = folds_outcome, 
    .learners_trt_folds = folds_exposure, 
    .trim = density_ratio_trim, 
    k = markov_assumption
  ) # End dictionary parameters RQ1
  rq2 <- rq1
  rq3 <- rq1
  
  ret <- list(
    rq1 = rq1, 
    rq2 = rq2, 
    rq3 = rq3
  )
  
  return(ret)
}
