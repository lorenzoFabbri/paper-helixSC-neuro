# Script containing dictionaries

################################################################################
# Parameters for entire paper
################################################################################
params <- function(is_hpc) {
  if (is_hpc == TRUE) {
    common_path <- "../../../../"
  } else {
    common_path <- "~/mounts/rstudioserver/PROJECTES/HELIX_OMICS/"
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
  
  chemicals <- c(
    "hs_mep_", 
    "hs_mibp_", 
    "hs_mnbp_", 
    "hs_mbzp_", 
    "hs_mehp_", 
    "hs_mehhp_", 
    "hs_meohp_", 
    "hs_mecpp_", 
    "hs_ohminp_", 
    "hs_oxominp_", 
    "hs_mepa_", 
    "hs_etpa_", 
    "hs_prpa_", 
    "hs_bpa_", 
    "hs_bupa_", 
    "hs_oxbe_", 
    "hs_trcs_", 
    "hs_dmp_", 
    "hs_dmtp_", 
    "hs_dmdtp_", 
    "hs_dep_", 
    "hs_detp_", 
    "hs_dedtp_"
  ) # End list of chemicals
  chemicals <- paste0(chemicals, "c")
  metabolites <- c(
    "Cortisone_E"
  ) # End list of metabolites
  clinical_outcome <- "hs_hitrtse"
  variables <- list(
    identifier = "HelixID", 
    strategy_select_adj_set = "smallest", 
    creatinine_name = "hs_creatinine_cg", 
    creatinine_covariates_names = list(
      numerical = c("hs_age_years", "hs_c_weight", "hs_c_height"), 
      categorical = c("e3_sex", "h_ethnicity_spiro")
    ), 
    strategy_loq_urine = "div2", 
    creatinine_threshold = 10, 
    ############################################################################
    rq1 = list(
      outcome = clinical_outcome, 
      outcome_negative = "", 
      exposures = chemicals
    ), # End options RQ1
    rq2 = list(
      outcome = metabolites, 
      outcome_negative = "", 
      exposures = chemicals
    ), # End options RQ2
    rq3 = list(
      outcome = clinical_outcome, 
      outcome_negative = "", 
      exposures = metabolites
    ) # End options RQ3
  )
  ##############################################################################
  
  steps <- list(
    rq2 = list(
      preproc_exposures = list(
        missings = list(
          threshold_within = 40, 
          threshold_overall = 40
        ), 
        creatinine = list(
          method = "cas", 
          method_fit_args = list(
            family = gaussian(link = "identity")
          )
        )#, 
        # standardization = list(
        #   center_fun = median, 
        #   scale_fun = IQR
        # )
      ), # End preproc_exposures
      preproc_outcome = list(
        llodq = list(
          method = "div2"
        ), 
        missings = list(
          threshold_within = 40, 
          threshold_overall = 40
        ), 
        creatinine = list(
          method = "cas", 
          method_fit_args = list(
            family = gaussian(link = "identity")
          )
        )#, 
        # standardization = list(
        #   center_fun = median, 
        #   scale_fun = IQR
        # )
      ), # End preproc_outcome
      preproc_covars = list(
        missings = list(
          threshold_within = 40, 
          threshold_overall = 40
        )
      ) # End preproc_covars
    ), # End steps RQ2
    ############################################################################
    rq3 = list(
      preproc_exposures = list(
        llodq = list(
          method = "div2"
        ), 
        missings = list(
          threshold_within = 40, 
          threshold_overall = 40
        ), 
        creatinine = list(
          method = "cas", 
          method_fit_args = list(
            family = gaussian(link = "identity")
          )
        ), 
        standardization = list(
          center_fun = median, 
          scale_fun = IQR
        )
      ), # End preproc_exposures
      preproc_outcome = list(
        standardization = list(
          center_fun = median, 
          scale_fun = IQR
        )
      ), # End preproc_outcome
      preproc_covars = list(
        missings = list(
          threshold_within = 40, 
          threshold_overall = 40
        )
      ) # End preproc_covars
    ) # End steps RQ3
    ############################################################################
  ) # End list of steps
  ##############################################################################
  
  return(list(
    paths = paths, 
    variables = variables, 
    steps = steps
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
    method_weightit = "energy", 
    weights_trim = 0.90, 
    use_kernel = TRUE, 
    sl_discrete = FALSE, 
    sl_lib = c("SL.glm", 
               "SL.gam", "SL.glmnet"), 
    method_marginal = "glm", 
    #family_marginal = mgcv::ocat(R = 36), 
    family_marginal = gaussian(link = "identity"), 
    add_inter_exposure = FALSE, 
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
