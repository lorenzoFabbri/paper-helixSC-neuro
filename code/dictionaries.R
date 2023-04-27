# Script containing dictionaries (e.g., exposures)

################################################################################
# Parameters for entire paper
################################################################################
params <- function() {
  paths <- list(
    # Raw
    path_covariates_post = "", 
    path_covariates_preg = "", 
    path_snps = "", 
    path_exposures_backtrans = "~/mounts/PROJECTES/HELIX_OMICS/data_raw/exposome.backtransf/child/preg_8y/20180901/20180919_v2_3_backtransformed.Rdata", 
    path_exposures_post_raw = "~/mounts/PROJECTES/HELIX_OMICS/data_final/exposome/child/preg_8y/v2_3_20180601/exppostnatal_raw_v2_3.RData", 
    path_exposures_preg_raw = "~/mounts/PROJECTES/HELIX_OMICS/data_final/exposome/child/preg_8y/v2_3_20180601/exppregnancy_raw_v2_3.RData", 
    path_exposures_post_final = "~/mounts/PROJECTES/HELIX_OMICS/data_final/exposome/child/preg_8y/v2_3_20180601/exppostnatal_v2_3.RData", 
    path_exposures_preg_final = "~/mounts/PROJECTES/HELIX_OMICS/data_final/exposome/child/preg_8y/v2_3_20180601/exppregnancy_v2_3.RData", 
    path_metabolome_serum = "~/mounts/PROJECTES/HELIX_OMICS/data_final/metab/child/8y/serum.urine_Biocrates.NMR_QChelix_20170101/metab_serum_subcohort_v3.RData", 
    path_metabolome_urine = "~/mounts/PROJECTES/HELIX_OMICS/data_final/metab/child/8y/serum.urine_Biocrates.NMR_QChelix_20170101/metab_urine_subcohort_v3.RData", 
    
    # Processed during analyses
    path_exposures_post_procme = "~/mounts/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF_phd/data/data_paper3/processed/exposures/", 
    path_exposures_preg_procme = "~/mounts/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF_phd/data/data_paper3/processed/exposures/", 
    path_metabolome_serum = "~/mounts/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF_phd/data/data_paper3/processed/omics/", 
    path_metabolome_urine = "~/mounts/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF_phd/data/data_paper3/processed/omics/"
  )
  
  return(list(
    paths = paths
  ))
}

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
