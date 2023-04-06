# Script containing dictionaries (e.g., exposures)

################################################################################
# Labels for HELIX variables
################################################################################
process_one_sheet <- function(sheet_id) {
  url <- "https://docs.google.com/spreadsheets/d/1aETxd2bzjhkbOGDLRgEPVjaWu1iyOt0dBGNV2QHTEv4/edit?usp=sharing"
  google_sheet <- googlesheets4::read_sheet(ss = url, sheet = sheet_id) |>
    dplyr::rename(var_name = `Variable name`, 
                  label = Description) |>
    dplyr::select(var_name, label)
  
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
  vars_dag_rq1 <- list(
    age_child = c("hs_age_years"), 
    airPollution_child = c("hs_no2_wk_hs_t", 
                           "hs_nox_wk_hs_t", 
                           "hs_pm10_wk_hs_t", 
                           "hs_pm25_wk_hs_t"), 
    airPollution_preg = c(), 
    breastfeeding = c(), 
    bw = c(), 
    child_alcohol = c(), 
    child_diet = c(), 
    child_smoking = c(), 
    edu_child = c(), 
    familySEP = c(), 
    gestational_age = c(), 
    intelligence_SNPs = c(), 
    maternalAlcohol_preg = c(), 
    maternalDiet_preg = c(), 
    maternalIodine_preg = c(), 
    maternalIron_preg = c(), 
    maternalSEP_preg = c(), 
    maternalSmoking_preg = c(), 
    maternal_folicAcid_preg = c(), 
    neuropsychologicalDiagnosis_child = c(), 
    otherChemicals_child = c(), 
    otherChemicals_preg = c(), 
    paternalSEP_preg = c(), 
    paternalSmoking_preg = c(), 
    qualityTesting_child = c(), 
    water_child = c(), 
    water_preg = c(), 
    ethnicity_child = c(), 
    ethnicity_mother = c(), 
    sex_child = c(), 
    child_depression = c(), 
    envFactors_visit = c(), 
    season_visit = c(), 
    cohort = c(), 
    chemicals_SNPs = c()
  )
}
