# Script containing dictionaries

################################################################################
# Parameters for entire paper
################################################################################
vars_of_interest <- function(append_to_chem = NULL) {
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
  )
  chemicals <- paste0(chemicals, append_to_chem)
  metabolites <- c(
    "F",
    "X20aDHF",
    "X20bDHF",
    "X5bDHF",
    "X5aTHF",
    "X5bTHF",
    "X6OHF",
    "X5a20acortol",
    "X5a20bcortol",
    "X5b20acortol",
    "X5b20bcortol",
    "X11OHAndros",
    "CortisoneE",
    "X20aDHE",
    "X20bDHE",
    "X5aTHE",
    "X5bTHE",
    "X6OHE",
    "X5b20acortolone",
    "X5b20bcortolone",
    "X5aTHB",
    "X5bTHB",
    "A",
    "X17DOcortolone",
    "S",
    "X5bDHS",
    "X5bTHS",
    "X17HP",
    "PT",
    "T",
    "Andros",
    "Etio",
    "AED"
  )
  new_metabolites <- c(
    "cortisol_production",
    "cortisol_metabolism",
    "cortisone_production",
    "cortisone_metabolism",
    "X11bHSD",
    #"global_reductase_f",
    #"global_reductase_e",
    #"cyp3a4",
    "corticosterone_production"
    #"X11deoxycortisol_production",
    #"X11hydroxylase",
    #"X17hydroxylase",
    #"androgens_production",
    #"X5a_reductase",
    #"lyase",
    #"global_adrenal_function"
  )
  outcomes <- c(
    "hs_hitrtse"
    #"hs_dcolors3"
  )
  
  return(list(
    chemicals = chemicals,
    metabolites = metabolites,
    new_metabolites = new_metabolites,
    outcomes = outcomes
  ))
} # End function vars_of_interest

################################################################################
# Parameters for entire paper
################################################################################
params <- function(is_hpc) {
  if (is_hpc == TRUE) {
    common_path <- "../../../../"
  } else {
    common_path <-
      "~/mounts/rstudioserver/"
  }
  
  paths <- list(
    path_dat_request = "data/data_paper3/requests/AP136/HELIX_AP_136_request_updated12jun.2023.csv",
    path_all_steroids = "data/data_paper3/requests/AP136/steroids/",
    path_cp_data = "data/data_paper3/requests/AP136/biomarker_children panel_database_20171220.dta"
  )
  paths <- lapply(paths, function(x) {
    paste0(common_path, x)
  })
  ##############################################################################
  
  chemicals <- vars_of_interest(append_to_chem = "c")$chemicals
  metabolites <- vars_of_interest()$metabolites
  clinical_outcomes <- vars_of_interest()$outcomes
  creatinine_covariates_names <- list(
    numerical = c(
      "hs_age_years", "hs_c_weight", "hs_c_height",
      "FAS_score"
    ),
    categorical = c(
      "e3_sex", "h_ethnicity_c",
      "cohort",
      "hs_sample_c",
      "hs_tob"
    )
  )
  selection_covariates_names <- c(
    "hs_age_years", "e3_sex", "h_ethnicity_c", "h_ethnicity_m",
    #"hs_c_weight", "hs_c_height",
    "FAS_score", "hs_finance", "hs_tob",
    "hs_head_circ",
    "cohort", "hs_date_neu"
  )
  
  variables <- list(
    identifier = "HelixID",
    strategy_select_adj_set = "minimize_missings",
    selection_covariates_names = selection_covariates_names,
    type_sample_hcp = 6,
    ############################################################################
    rq1 = list(
      outcome = clinical_outcomes,
      outcome_negative = "",
      exposures = chemicals
    ), # End options RQ1
    rq2 = list(
      outcome = metabolites,
      outcome_negative = "",
      exposures = chemicals
    ), # End options RQ2
    rq3 = list(
      outcome = clinical_outcomes,
      outcome_negative = "",
      exposures = metabolites
    ), # End options RQ3
    rq4 = list(
      outcome = chemicals,
      outcome_negative = "",
      exposures = metabolites
    ) # End options RQ4
  )
  ##############################################################################
  
  threshold_within_lod <- 30
  threshold_overall_lod <- 20
  threshold_within_nan <- 40
  threshold_overall_nan <- 30
  steps <- list(
    rq1 = list(
      preproc_exposures = list(
        missings = list(
          threshold_within = threshold_within_nan,
          threshold_overall = threshold_overall_nan,
          use_additional_covariates = FALSE,
          selected_covariates = NULL,
          method_imputation = "vim.knn",
          k = 5
        )
        # creatinine = list(
        #   method = "cas",
        #   method_fit_args = list(family = gaussian(link = "identity")),
        #   creatinine_covariates_names = creatinine_covariates_names,
        #   creatinine_name = "hs_creatinine_cg",
        #   path_save_res = "results/figures/rq2/model_check_creatinine_exp.png"
        # )
      ), # End preproc_exposures
      preproc_outcome = list(
        missings = list(
          threshold_within = threshold_within_nan,
          threshold_overall = threshold_overall_nan,
          use_additional_covariates = FALSE,
          selected_covariates = NULL,
          method_imputation = "vim.knn",
          k = 5
        ),
        transform = list(
          transformation_fun = log
        )
      ), # End preproc_outcome
      preproc_covars = list(
        missings = list(
          threshold_within = threshold_within_nan,
          threshold_overall = threshold_overall_nan,
          use_additional_covariates = FALSE,
          selected_covariates = NULL,
          method_imputation = "vim.knn",
          k = 5
        )
      ) # End preproc_covars
    ), # End steps RQ1
    ############################################################################
    rq2 = list(
      preproc_exposures = list(
        missings = list(
          threshold_within = threshold_within_nan,
          threshold_overall = threshold_overall_nan,
          use_additional_covariates = FALSE,
          selected_covariates = NULL,
          method_imputation = "vim.knn",
          k = 5
        )
        # creatinine = list(
        #   method = "cas",
        #   method_fit_args = list(family = gaussian(link = "identity")),
        #   creatinine_covariates_names = creatinine_covariates_names,
        #   creatinine_name = "hs_creatinine_cg",
        #   path_save_res = "results/figures/rq2/model_check_creatinine_exp.png"
        # )
      ), # End preproc_exposures
      preproc_outcome = list(
        llodq = list(
          id_val = c(2, 4),
          method = "replace",
          divide_by = 2,
          creatinine_threshold = NULL,
          threshold_within = threshold_within_lod,
          threshold_overall = threshold_overall_lod,
          tune_sigma = NULL
        ),
        missings = list(
          threshold_within = threshold_within_nan,
          threshold_overall = threshold_overall_nan,
          use_additional_covariates = FALSE,
          selected_covariates = NULL,
          method_imputation = "vim.knn",
          k = 5
        )
        # creatinine = list(
        #   method = "cas",
        #   method_fit_args = list(family = gaussian(link = "identity")),
        #   creatinine_covariates_names = creatinine_covariates_names,
        #   creatinine_name = "hs_creatinine_cg",
        #   path_save_res = "results/figures/rq2/model_check_creatinine_out.png"
        # )
      ), # End preproc_outcome
      preproc_covars = list(
        missings = list(
          threshold_within = threshold_within_nan,
          threshold_overall = threshold_overall_nan,
          use_additional_covariates = FALSE,
          selected_covariates = NULL,
          method_imputation = "vim.knn",
          k = 5
        )
      ) # End preproc_covars
    ), # End steps RQ2
    ############################################################################
    rq3 = list(
      preproc_exposures = list(
        llodq = list(
          id_val = c(2, 4),
          method = "replace",
          divide_by = 2,
          creatinine_threshold = NULL,
          threshold_within = threshold_within_lod,
          threshold_overall = threshold_overall_lod,
          tune_sigma = NULL
        ),
        missings = list(
          threshold_within = threshold_within_nan,
          threshold_overall = threshold_overall_nan,
          use_additional_covariates = FALSE,
          selected_covariates = NULL,
          method_imputation = "vim.knn",
          k = 5
        )
        # creatinine = list(
        #   method = "cas",
        #   method_fit_args = list(family = gaussian(link = "identity")),
        #   creatinine_covariates_names = creatinine_covariates_names,
        #   creatinine_name = "hs_creatinine_cg",
        #   path_save_res = "results/figures/rq3/model_check_creatinine_exp.png"
        # )
      ), # End preproc_exposures
      preproc_outcome = list(
        missings = list(
          threshold_within = threshold_within_nan,
          threshold_overall = threshold_overall_nan,
          use_additional_covariates = FALSE,
          selected_covariates = NULL,
          method_imputation = "vim.knn",
          k = 5
        ),
        transform = list(
          transformation_fun = log
        )
      ), # End preproc_outcome
      preproc_covars = list(
        missings = list(
          threshold_within = threshold_within_nan,
          threshold_overall = threshold_overall_nan,
          use_additional_covariates = FALSE,
          selected_covariates = NULL,
          method_imputation = "vim.knn",
          k = 5
        )
      ) # End preproc_covars
    ), # End steps RQ3
    ############################################################################
    rq4 = list(
      preproc_exposures = list(
        llodq = list(
          id_val = c(2, 4),
          method = "replace",
          divide_by = 2,
          creatinine_threshold = NULL,
          threshold_within = threshold_within_lod,
          threshold_overall = threshold_overall_lod,
          tune_sigma = NULL
        ),
        missings = list(
          threshold_within = threshold_within_nan,
          threshold_overall = threshold_overall_nan,
          use_additional_covariates = FALSE,
          selected_covariates = NULL,
          method_imputation = "vim.knn",
          k = 5
        )
        # creatinine = list(
        #   method = "cas",
        #   method_fit_args = list(family = gaussian(link = "identity")),
        #   creatinine_covariates_names = creatinine_covariates_names,
        #   creatinine_name = "hs_creatinine_cg",
        #   path_save_res = "results/figures/rq3/model_check_creatinine_exp.png"
        # )
      ), # End preproc_exposures
      preproc_outcome = list(
        missings = list(
          threshold_within = threshold_within_nan,
          threshold_overall = threshold_overall_nan,
          use_additional_covariates = FALSE,
          selected_covariates = NULL,
          method_imputation = "vim.knn",
          k = 5
        ),
        # creatinine = list(
        #   method = "cas",
        #   method_fit_args = list(family = gaussian(link = "identity")),
        #   creatinine_covariates_names = creatinine_covariates_names,
        #   creatinine_name = "hs_creatinine_cg",
        #   path_save_res = "results/figures/rq2/model_check_creatinine_exp.png"
        # )
        transform = list(
          transformation_fun = log
        )
      ), # End preproc_outcome
      preproc_covars = list(
        missings = list(
          threshold_within = threshold_within_nan,
          threshold_overall = threshold_overall_nan,
          use_additional_covariates = FALSE,
          selected_covariates = NULL,
          method_imputation = "vim.knn",
          k = 5
        )
      ) # End preproc_covars
    ) # End steps RQ4
    ############################################################################
  ) # End list of steps
  ##############################################################################
  
  return(list(
    paths = paths,
    variables = variables,
    steps = steps
  ))
} # End function params

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
    stabilize = TRUE,
    weights_trim = 0.9,
    sl_lib = c(
      "SL.glm",
      "SL.gam", "SL.glmnet"
    ),
    sl_discrete = FALSE,
    use_kernel = TRUE,
    method_marginal = "glm",
    family_marginal = gaussian(link = "identity"),
    add_inter_exposure = FALSE,
    add_splines_exposure = TRUE,
    df_splines = 3,
    threshold_smooth = 10,
    threshold_k = 3,
    type_avg_comparison = c(0.1, 0.9)
  ) # End dictionary parameters RQ1
  rq2 <- rq1
  rq3 <- rq1
  rq4 <- rq1
  
  ret <- list(
    rq1 = rq1,
    rq2 = rq2,
    rq3 = rq3,
    rq4 = rq4
  )
  
  return(ret)
} # End function params_analyses
