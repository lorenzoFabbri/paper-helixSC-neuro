# Script containing dictionaries (e.g., exposures)

################################################################################
# Parameters for entire paper
################################################################################
params <- function() {
  common_path <- "~/mounts/PROJECTES/HELIX_OMICS/"
  paths <- list(
    # Raw
    path_covariates_post = "", 
    path_covariates_preg = "", 
    path_snps = "", 
    path_exposures_backtrans = "data_raw/exposome.backtransf/child/preg_8y/20180901/20180919_v2_3_backtransformed.Rdata", 
    path_exposures_post_raw = "data_final/exposome/child/preg_8y/v2_3_20180601/exppostnatal_raw_v2_3.RData", 
    path_exposures_preg_raw = "data_final/exposome/child/preg_8y/v2_3_20180601/exppregnancy_raw_v2_3.RData", 
    path_exposures_post_final = "data_final/exposome/child/preg_8y/v2_3_20180601/exppostnatal_v2_3.RData", 
    path_exposures_preg_final = "data_final/exposome/child/preg_8y/v2_3_20180601/exppregnancy_v2_3.RData", 
    path_metabolome_serum = "data_final/metab/child/8y/serum.urine_Biocrates.NMR_QChelix_20170101/metab_serum_subcohort_v3.RData", 
    path_metabolome_urine = "data_final/metab/child/8y/serum.urine_Biocrates.NMR_QChelix_20170101/metab_urine_subcohort_v3.RData", 
    path_dat_request = "DATA_PREVIOUS_MIGRATION/lorenzoF_phd/data/data_paper3/requests/AP136/HELIX_AP_136_request_updated15may.2023.csv",  
    
    # Processed during analyses
    path_exposures_post_procme = "DATA_PREVIOUS_MIGRATION/lorenzoF_phd/data/data_paper3/processed/exposures/", 
    path_exposures_preg_procme = "DATA_PREVIOUS_MIGRATION/lorenzoF_phd/data/data_paper3/processed/exposures/", 
    path_metabolome_serum = "DATA_PREVIOUS_MIGRATION/lorenzoF_phd/data/data_paper3/processed/omics/", 
    path_metabolome_urine = "DATA_PREVIOUS_MIGRATION/lorenzoF_phd/data/data_paper3/processed/omics/"
  )
  paths <- lapply(paths, function(x) {
    paste0(common_path, x)
  })
  ##############################################################################
  
  variables <- list(
    identifier = "HelixID", 
    rq1 = list(
      outcome = "hs_ADHD_raw", 
      exposures = "cadj"
    )
  )
  ##############################################################################
  
  variables_desc_population <- list(
    general = c("hs_bf", 
                "hs_tob", "e3_asmokyn_p", 
                "h_pass_smok_4m", "h_pass_smok_8m", "h_pass_smok_6m", 
                "h_pass_smok_1y", "h_pass_smok_2y", "h_pass_smok_3yr", 
                "h_pass_smok_4_5yr", "h_pass_smok_7y", "h_pass_smok_8y", 
                "cohort", "h_ethnicity_c", "h_ethnicity_m"), 
    anthropometric = c("e3_bw", "e3_gac", "hs_age_years", "e3_sex", 
                       "hs_c_height", "hs_c_weight"), 
    outcomes = c("hs_fam_car", "hs_c_room", "hs_neuro_diag", 
                 variables$rq1$outcome), 
    sep = c("hs_wrk_m", "FAS_score", "e3_edumc", 
            "e3_marital", "e3_ses", "e3_edupc", "e3_edufc"), 
    biomarkers = c("hs_dift_mealblood")
  )
  ##############################################################################
  
  return(list(
    paths = paths, 
    variables = variables, 
    variables_desc_population = variables_desc_population
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
