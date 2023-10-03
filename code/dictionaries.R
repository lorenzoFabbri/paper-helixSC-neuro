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
    "Etio"
  )
  outcomes <- c("hs_hitrtse")

  return(list(
    chemicals = chemicals,
    metabolites = metabolites,
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
      "~/mounts/rstudioserver/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF"
  }

  paths <- list(
    path_dat_request = "data/data_paper3/requests/AP136/HELIX_AP_136_request_updated12jun.2023.csv",
    path_all_steroids = "data/data_paper3/requests/AP136/steroids/"
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
      "e3_sex", "h_ethnicity_spiro",
      "cohort",
      "hs_sample_c"
    )
  )

  variables <- list(
    identifier = "HelixID",
    strategy_select_adj_set = "smallest",
    ############################################################################
    rq1 = list(
      outcome = clinical_outcomes,
      outcome_negative = "",
      exposures = chemicals
    ),
    # End options RQ1
    rq2 = list(
      outcome = metabolites,
      outcome_negative = "",
      exposures = chemicals
    ),
    # End options RQ2
    rq3 = list(
      outcome = clinical_outcomes,
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
          threshold_overall = 30,
          use_additional_covariates = FALSE,
          selected_covariates = NULL,
          method_imputation = "vim.knn",
          k = 5
        ),
        creatinine = list(
          method = "cas",
          method_fit_args = list(family = gaussian(link = "identity")),
          creatinine_covariates_names = creatinine_covariates_names,
          creatinine_name = "hs_creatinine_cg",
          path_save_res = "results/figures/rq2/model_check_creatinine_exp.png"
        )
      ),
      # End preproc_exposures
      preproc_outcome = list(
        llodq = list(
          id_val = 2,
          method = "replace",
          creatinine_threshold = NULL,
          threshold_within = 10,
          threshold_overall = 10,
          tune_sigma = NULL
        ),
        missings = list(
          threshold_within = 40,
          threshold_overall = 30,
          use_additional_covariates = FALSE,
          selected_covariates = NULL,
          method_imputation = "vim.knn",
          k = 5
        ),
        creatinine = list(
          method = "cas",
          method_fit_args = list(family = gaussian(link = "identity")),
          creatinine_covariates_names = creatinine_covariates_names,
          creatinine_name = "hs_creatinine_cg",
          path_save_res = "results/figures/rq2/model_check_creatinine_out.png"
        ),
        transform = list(
          transformation_fun = log
        )
      ),
      # End preproc_outcome
      preproc_covars = list(
        missings = list(
          threshold_within = 40,
          threshold_overall = 30,
          use_additional_covariates = FALSE,
          selected_covariates = NULL,
          method_imputation = "vim.knn",
          k = 5
        )
      ) # End preproc_covars
    ),
    # End steps RQ2
    ############################################################################
    rq3 = list(
      preproc_exposures = list(
        llodq = list(
          id_val = 2,
          method = "replace",
          creatinine_threshold = NULL,
          threshold_within = 10,
          threshold_overall = 10,
          tune_sigma = NULL
        ),
        missings = list(
          threshold_within = 40,
          threshold_overall = 30,
          use_additional_covariates = FALSE,
          selected_covariates = NULL,
          method_imputation = "vim.knn",
          k = 5
        ),
        creatinine = list(
          method = "cas",
          method_fit_args = list(family = gaussian(link = "identity")),
          creatinine_covariates_names = creatinine_covariates_names,
          creatinine_name = "hs_creatinine_cg",
          path_save_res = "results/figures/rq3/model_check_creatinine_exp.png"
        )
      ),
      # End preproc_exposures
      preproc_outcome = list(
        missings = list(
          threshold_within = 40,
          threshold_overall = 30,
          use_additional_covariates = FALSE,
          selected_covariates = NULL,
          method_imputation = "vim.knn",
          k = 5
        ),
        transform = list(
          transformation_fun = log
        )
      ),
      # End preproc_outcome
      preproc_covars = list(
        missings = list(
          threshold_within = 40,
          threshold_overall = 30,
          use_additional_covariates = FALSE,
          selected_covariates = NULL,
          method_imputation = "vim.knn",
          k = 5
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
    by = NULL,
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
    add_inter_exposure_specific = c(),
    add_splines_exposure = TRUE,
    df_splines = 3,
    threshold_smooth = 10,
    threshold_k = 3,
    type_avg_comparison = c(0.1, 0.9)
  ) # End dictionary parameters RQ1
  rq2 <- rq1
  rq3 <- rq1

  ret <- list(
    rq1 = rq1,
    rq2 = rq2,
    rq3 = rq3
  )

  return(ret)
} # End function params_analyses
