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

################################################################################
# Labels for HELIX variables
################################################################################
process_one_sheet <- function(sheet_id) {
  url <- "https://docs.google.com/spreadsheets/d/1aETxd2bzjhkbOGDLRgEPVjaWu1iyOt0dBGNV2QHTEv4/edit?usp=sharing"
  google_sheet <- googlesheets4::read_sheet(ss = url, sheet = sheet_id) |>
    dplyr::rename(var_name = `Variable name`, 
                  label = Description, 
                  type = Type, 
                  period = `Period (prenatal, postnatal)`) |>
    dplyr::select(var_name, label, type, period) |>
    dplyr::mutate(type = as.character(tolower(type))) |>
    dplyr::mutate(type = dplyr::recode(type, 
      float = "numerical", 
      decimal = "numerical", 
      double = "numerical", 
      numeric = "numerical", 
      byte = "integer", 
      int = "integer", 
      dichotomus = "integer", 
      string = "categorical", 
      bool = "categorical", 
      boolean = "categorical"
    ))
  
  # Some variables from the codebooks have the wrong type
  google_sheet[google_sheet$var_name == "hs_tob", "type"] <- "categorical"
  google_sheet[google_sheet$var_name == "e3_edum", "type"] <- "categorical"
  google_sheet[google_sheet$var_name == "e3_eduf", "type"] <- "categorical"
  google_sheet[google_sheet$var_name == "e3_ses", "type"] <- "categorical"
  google_sheet[google_sheet$var_name == "hs_neuro_diag", "type"] <- "categorical"
  google_sheet[google_sheet$var_name == "hs_qual_test", "type"] <- "categorical"
  google_sheet[google_sheet$var_name == "hs_rest_nth", "type"] <- "categorical"
  google_sheet[google_sheet$var_name == "hs_mood", "type"] <- "categorical"
  google_sheet[google_sheet$var_name == "hs_temp", "type"] <- "categorical"
  google_sheet[google_sheet$var_name == "hs_noise", "type"] <- "categorical"
  google_sheet[google_sheet$var_name == "hs_wtr_hm", "type"] <- "categorical"
  google_sheet[google_sheet$var_name == "e3_bw", "type"] <- "numerical"
  google_sheet[google_sheet$var_name == "e3_bhc", "type"] <- "numerical"
  google_sheet[google_sheet$var_name == "e3_bl", "type"] <- "numerical"
  google_sheet[google_sheet$var_name == "e3_alcpreg_g", "type"] <- "integer"
  google_sheet[google_sheet$var_name == "hs_total_fish", "type"] <- "integer"
  google_sheet[google_sheet$var_name == "hs_total_veg", "type"] <- "integer"
  google_sheet[google_sheet$var_name == "hs_total_fruits", "type"] <- "integer"
  google_sheet[google_sheet$var_name == "hs_fastfood", "type"] <- "integer"
  google_sheet[google_sheet$var_name == "hs_org_food", "type"] <- "integer"
  google_sheet[google_sheet$var_name == "hs_canfish", "type"] <- "integer"
  
  # Rename exposures with wrong name
  google_sheet[google_sheet$var_name == "hs_dmdtp_mrawadj", 
               "var_name"] <- "hs_dmdtp_madj"
  google_sheet[google_sheet$var_name == "hs_dedtp_mrawadj", 
               "var_name"] <- "hs_dedtp_madj"
  
  return(google_sheet)
}

dict_vars_labels <- function() {
  vars_rq1 <- process_one_sheet(sheet_id = "RQ1")
  vars_rq2 <- process_one_sheet(sheet_id = "RQ2")
  vars_rq3 <- process_one_sheet(sheet_id = "RQ3")
  
  return(list(
    vars_rq1 = vars_rq1, 
    vars_rq2 = vars_rq2, 
    vars_rq3 = vars_rq3
  ))
}

################################################################################
# Mapping between DAGs nodes and HELIX data
################################################################################
dict_mapping_vars <- function() {
  ##############################################################################
  outcomes <- list(
    raven = c("hs_correct_raven", 
              "hs_time_raven"), 
    nback = c("hs_ncncolors1", 
              "hs_nfacolors1", 
              "hs_nhitcolors1", 
              "hs_nmisscolors1", 
              "hs_hitrt_meancolors1", 
              "hs_ncncolors2", 
              "hs_nfacolors2", 
              "hs_nhitcolors2", 
              "hs_nmisscolors2", 
              "hs_hitrt_meancolors2", 
              "hs_ncncolors3", 
              "hs_nfacolors3", 
              "hs_nhitcolors3", 
              "hs_nmisscolors3", 
              "hs_hitrt_meancolors3", 
              "hs_ncnnumeros1", 
              "hs_nfanumeros1", 
              "hs_nhitnumeros1", 
              "hs_nmissnumeros1", 
              "hs_hitrt_meannumeros1", 
              "hs_ncnnumeros2", 
              "hs_nfanumeros2", 
              "hs_nhitnumeros2", 
              "hs_nmissnumeros2", 
              "hs_hitrt_meannumeros2", 
              "hs_ncnnumeros3", 
              "hs_nfanumeros3", 
              "hs_nhitnumeros3", 
              "hs_nmissnumeros3", 
              "hs_hitrt_meannumeros3", 
              "hs_accuracy_colors1", 
              "hs_dcolors1", 
              "hs_accuracy_colors2", 
              "hs_dcolors2", 
              "hs_accuracy_colors3", 
              "hs_dcolors3", 
              "hs_accuracy_numeros1", 
              "hs_dnumeros1", 
              "hs_accuracy_numeros2", 
              "hs_dnumeros2", 
              "hs_accuracy_numeros3", 
              "hs_dnumeros3"), 
    ant = c("hs_zeros", 
            "hs_ones", 
            "hs_missings", 
            "hs_comisions", 
            "hs_congruent_mean", 
            "hs_congruent_median", 
            "hs_incongruent_mean", 
            "hs_incongruent_median", 
            "hs_center_mean", 
            "hs_center_median", 
            "hs_double_mean", 
            "hs_double_median", 
            "hs_no_mean", 
            "hs_no_median", 
            "hs_spatial_mean", 
            "hs_spatial_median", 
            "hs_center_cong_mean", 
            "hs_center_cong_median", 
            "hs_double_cong_mean", 
            "hs_double_cong_median", 
            "hs_no_cong_mean", 
            "hs_no_cong_median", 
            "hs_spatial_cong_mean", 
            "hs_spatial_cong_median", 
            "hs_accuracy_ANT", 
            "hs_alerting", 
            "hs_orienting", 
            "hs_conflict", 
            "hs_orienting_endogenous", 
            "hs_orienting_exogenous", 
            "hs_hitrt2_mean", 
            "hs_hitrt2_median", 
            "hs_hitrtse"), 
    cbcl = c("hs_zeros", 
             "hs_ones", 
             "hs_missings", 
             "hs_comisions", 
             "hs_congruent_mean", 
             "hs_congruent_median", 
             "hs_incongruent_mean", 
             "hs_incongruent_median", 
             "hs_center_mean", 
             "hs_center_median", 
             "hs_double_mean", 
             "hs_double_median", 
             "hs_no_mean", 
             "hs_no_median", 
             "hs_spatial_mean", 
             "hs_spatial_median", 
             "hs_center_cong_mean", 
             "hs_center_cong_median", 
             "hs_double_cong_mean", 
             "hs_double_cong_median", 
             "hs_no_cong_mean", 
             "hs_no_cong_median", 
             "hs_spatial_cong_mean", 
             "hs_spatial_cong_median", 
             "hs_accuracy_ANT", 
             "hs_alerting", 
             "hs_orienting", 
             "hs_conflict", 
             "hs_orienting_endogenous", 
             "hs_orienting_exogenous", 
             "hs_hitrt2_mean", 
             "hs_hitrt2_median", 
             "hs_hitrtse"), 
    conners = c("hs_Oposic_raw", 
                "hs_Cognit_raw", 
                "hs_Hyper_raw", 
                "hs_ADHD_raw"), 
    pfactors = c("GPF", 
                 "INT", 
                 "EXT")
  ) # END dictionary outcomes
  
  ##############################################################################
  exposures <- list(
    mep = "hs_mep_cadj", 
    mibp = "hs_mibp_cadj", 
    mnbp = "hs_mnbp_cadj", 
    mbzp = "hs_mbzp_cadj", 
    mehp = "hs_mehp_cadj", 
    mehhp = "hs_mehhp_cadj", 
    meohp = "hs_meohp_cadj", 
    mecpp = "hs_mecpp_cadj", 
    ohminp = "hs_ohminp_cadj", 
    oxominp = "hs_oxominp_cadj", 
    mepa = "hs_mepa_cadj", 
    etpa = "hs_etpa_cadj", 
    prpa = "hs_prpa_cadj", 
    bpa = "hs_bpa_cadj", 
    bupa = "hs_bupa_cadj", 
    oxbe = "hs_oxbe_cadj", 
    trcs = "hs_trcs_cadj", 
    dmp = "hs_dmp_cadj", 
    dmtp = "hs_dmtp_cadj", 
    dmdtp = "hs_dmdtp_cadj", 
    dep = "hs_dep_cadj", 
    detp = "hs_detp_cadj", 
    dedtp = "hs_dedtp_cadj"
  ) # END dictionary exposures (EDCs)
  
  ##############################################################################
  vars_dag_rq1 <- list(
    age_child = c("hs_age_years"), 
    airPollution_child = c("hs_no2_wk_hs_t", 
                           "hs_nox_wk_hs_t", 
                           "hs_pm10_wk_hs_t", 
                           "hs_pm25_wk_hs_t"), 
    airPollution_preg = c("h_no2_ratio_t1", 
                          "h_no2_ratio_t2", 
                          "h_no2_ratio_t3", 
                          "h_nox_ratio_t1", 
                          "h_nox_ratio_t2", 
                          "h_nox_ratio_t3", 
                          "h_pm10_ratio_t1", 
                          "h_pm10_ratio_t2", 
                          "h_pm10_ratio_t3", 
                          "h_pm25_ratio_t1", 
                          "h_pm25_ratio_t2", 
                          "h_pm25_ratio_t3"), 
    breastfeeding = c("hs_bf", 
                      "hs_bfdur"), 
    bw = c("e3_bw", 
           "e3_bhc", 
           "e3_bl"), 
    child_diet = c("hs_total_fish", 
                   "hs_total_veg", 
                   "hs_total_fruits", 
                   "hs_fastfood", 
                   "hs_org_food", 
                   "hs_canfish"), 
    child_smoking = c("hs_tob", 
                      "hs_smk_parents"), 
    familySEP = c("hs_wrk_m", 
                  "hs_finance", 
                  "FAS_score"), 
    gestational_age = c("e3_gac", 
                        "weeks_sample_age"), 
    ############################################################################
    intelligence_SNPs = c("NULL"), 
    ############################################################################
    maternalAlcohol_preg = c("e3_alcpreg_yn", 
                             "e3_alcpreg_g"), 
    maternalDiet_preg = c("h_fish_preg", 
                          "h_veg_preg", 
                          "h_fruit_preg", 
                          "h_fastfood_preg"), 
    maternalSEP_preg = c("e3_edum", 
                         "e3_ses"), 
    maternalSmoking_preg = c("e3_psmokt1", 
                             "e3_psmokt2", 
                             "e3_psmokt3", 
                             "e3_psmokt2_t3", 
                             "e3_psmokanyt", 
                             "e3_asmokyn_t1", 
                             "e3_asmokyn_t2", 
                             "e3_asmokyn_t3", 
                             "e3_asmokyn_t2_t3", 
                             "e3_asmokyn_p", 
                             "e3_asmokcigd_t1", 
                             "e3_asmokcigd_t2", 
                             "e3_asmokcigd_t3", 
                             "e3_asmokcigd_t2_t3", 
                             "e3_asmokcigd_p"), 
    maternal_folicAcid_preg = c("h_folic_t1"), 
    neuropsychologicalDiagnosis_child = c("hs_neuro_diag"), 
    otherChemicals_child = c("hs_as_c", 
                             "hs_cd_c", 
                             "hs_hg_c", 
                             "hs_mn_c", 
                             "hs_pb_c"), 
    otherChemicals_preg = c("hs_as_m", 
                            "hs_cd_m", 
                            "hs_hg_m", 
                            "hs_mn_m", 
                            "hs_pb_m", 
                            "hs_mep_madj", 
                            "hs_mibp_madj", 
                            "hs_mnbp_madj", 
                            "hs_mbzp_madj", 
                            "hs_mehp_madj", 
                            "hs_mehhp_madj", 
                            "hs_meohp_madj", 
                            "hs_mecpp_madj", 
                            "hs_ohminp_madj", 
                            "hs_oxominp_madj", 
                            "hs_mepa_madj", 
                            "hs_etpa_madj", 
                            "hs_prpa_madj", 
                            "hs_bpa_madj", 
                            "hs_bupa_madj", 
                            "hs_oxbe_madj", 
                            "hs_trcs_madj", 
                            "hs_dmp_madj", 
                            "hs_dmtp_madj", 
                            "hs_dmdtp_madj", 
                            "hs_dep_madj", 
                            "hs_detp_madj", 
                            "hs_dedtp_madj"), 
    paternalSEP_preg = c("e3_eduf"), 
    qualityTesting_child = c("hs_qual_test", 
                             "hs_rest_nth", 
                             "hs_mood"), 
    water_child = c("hs_wtr_hm"), 
    sex_child = c("hs_Gender"), 
    ethnicity_child = c("hs_Ethnic"), 
    ethnicity_mother = c("h_ethnicity_m"), 
    envFactors_visit = c("hs_temp", 
                         "hs_noise"), 
    season_visit = c("hs_date_neu"), 
    cohort = c("cohort"), 
    chemicals_SNPs = c("rs1057910", 
                       "rs115250492", 
                       "rs11692021", 
                       "rs12248560", 
                       "rs12330015", 
                       "rs1799807", 
                       "rs1799853", 
                       "rs1800246", 
                       "rs1803274", 
                       "rs1902023", 
                       "rs2177180", 
                       "rs3892097", 
                       "rs4244285", 
                       "rs4253690", 
                       "rs4823902", 
                       "rs5766698", 
                       "rs662", 
                       "rs705379", 
                       "rs75525202", 
                       "rs854560")
  ) # END dictionary RQ1
  
  ##############################################################################
  vars_dag_rq2 <- list(
    
  ) # END dictionary RQ2
  
  ##############################################################################
  vars_dag_rq3 <- list(
    
  ) # END dictionary RQ3
  
  return(list(
    outcomes = outcomes, 
    exposures = exposures, 
    vars_dag_rq1 = vars_dag_rq1, 
    vars_dag_rq2 = vars_dag_rq2, 
    vars_dag_rq3 = vars_dag_rq3
  ))
}
