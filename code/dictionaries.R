# Script containing dictionaries (e.g., exposures)

################################################################################
# Parameters for entire paper
################################################################################
params <- function(is_hpc) {
  if (is_hpc == TRUE) {
    common_path <- "../../../../"
  } else {
    common_path <- "~/mounts/PROJECTES/HELIX_OMICS/"
  }
  
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
    path_dat_request = "DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/requests/AP136/HELIX_AP_136_request_updated12jun.2023.csv", 
    path_all_steroids = "DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/requests/AP136/steroids/", 
    
    # Processed during analyses
    path_exposures_post_procme = "DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/processed/exposures/", 
    path_exposures_preg_procme = "DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/processed/exposures/", 
    path_metabolome_serum = "DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/processed/omics/", 
    path_metabolome_urine = "DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/processed/omics/"
  )
  paths <- lapply(paths, function(x) {
    paste0(common_path, x)
  })
  ##############################################################################
  
  clinical_outcome <- "hs_hitrtse"
  variables <- list(
    identifier = "HelixID", 
    strategy_select_adj_set = "smallest", 
    strategy_loq_urine = "div2", 
    creatinine_threshold = 10, 
    ############################################################################
    rq01 = list(
      outcome = clinical_outcome, 
      outcome_negative = "", 
      exposures = "cadj"
    ), 
    rq1 = list(
      outcome = clinical_outcome, 
      outcome_negative = "", 
      exposures = "_cadj"
    ), # End options RQ1
    ############################################################################
    rq02 = list(
      outcome = "metab", 
      outcome_negative = "", 
      exposures = "cadj"
    ), 
    rq2 = list(
      outcome = "metab", 
      outcome_negative = "", 
      exposures = "_cadj"
    ), # End options RQ2
    ############################################################################
    rq03 = list(
      outcome = clinical_outcome, 
      outcome_negative = "", 
      exposures = "metab"
    ), 
    rq3 = list(
      outcome = clinical_outcome, 
      outcome_negative = "", 
      exposures = "_metab"
    ), # End options RQ3
    ############################################################################
    preproc_exposures = list(
      missings = list(
        do = TRUE, 
        threshold_within = 40, 
        threshold_overall = 40
      ), 
      standardization = list(
        do = TRUE, 
        center_fun = mean, 
        scale_fun = sd
      )
    ), # End preproc_exposures
    ############################################################################
    preproc_outcome = list(
      bound = list(
        do = FALSE
      )
    ), # End preproc_outcome
    ############################################################################
    preproc_covars = list(
      missings = list(
        do = TRUE, 
        threshold_within = 40, 
        threshold_overall = 40
      )
    ) # End preproc_covars
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
                 clinical_outcome), 
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
  rq1 <- list(
    # lmtp
    learners_outcome = c("SL.mean", "SL.glm"), 
    learners_trt = c("SL.glm"), 
    estimator = "sdr", 
    folds = 3, 
    .learners_outcome_folds = 1, 
    .learners_trt_folds = 1, 
    .trim = 0.995, 
    k = Inf, 
    shift_type = "mul", 
    shift_amount = 0.01, 
    ############################################################################
    # WeightIt, Cobalt, marginaleffects
    method_weightit = "super", 
    weights_trim = 0.90, 
    use_kernel = TRUE, 
    sl_discrete = FALSE, 
    sl_lib = c("SL.glm", 
               "SL.gam", "SL.glmnet"), 
    method_marginal = "gam", 
    #family_marginal = mgcv::ocat(R = 36), 
    family_marginal = gaussian(link = "identity"), 
    add_inter_exposure = TRUE, 
    add_splines_exposure = TRUE, 
    df_splines = 3, 
    threshold_smooth = 10, 
    threshold_k = 3
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
