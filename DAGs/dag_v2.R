# DAGs based on research questions defined in the Causal Roadmap

dag_v2 <- function() {
  ##############################################################################
  chem_to_out <- ggdag::dagify(
    airPollution_child ~ cohort + familySEP + season_visit, 
    airPollution_preg ~ cohort + maternalSEP_preg + paternalSEP_preg, 
    breastfeeding ~ ethnicity_mother + maternalSEP_preg, 
    bw ~ airPollution_preg + breastfeeding + cohort + ethnicity_child + ethnicity_mother + maternalAlcohol_preg + maternalDiet_preg + maternalIodine_preg + maternalIron_preg + maternalSEP_preg + maternalSmoking_preg + maternal_folicAcid_preg + otherChemicals_preg + paternalSEP_preg + paternalSmoking_preg + sex_child, 
    chemical ~ chemicals_SNPs + child_diet + cohort + creatinine + familySEP + season_visit + time_lastMeal + water_child, 
    chemicals_SNPs ~ ethnicity_child + ethnicity_mother, 
    child_alcohol ~ age_child + ethnicity_child + familySEP + sex_child, 
    child_depression ~ age_child + airPollution_child + airPollution_preg + breastfeeding + child_alcohol + child_diet + child_smoking + cohort + ethnicity_child + ethnicity_mother + familySEP + maternalAlcohol_preg + maternalIodine_preg + maternalIron_preg + maternalSmoking_preg + maternal_folicAcid_preg + otherChemicals_child + otherChemicals_preg + paternalSEP_preg + paternalSmoking_preg + sex_child, 
    child_diet ~ age_child + cohort + ethnicity_mother + familySEP + sex_child, 
    child_smoking ~ age_child + ethnicity_child + familySEP + sex_child, 
    creatinine ~ age_child + child_diet + fatMass_child + sex_child + time_lastMeal, 
    edu_child ~ age_child + cohort + ethnicity_mother + familySEP, 
    envFactors_visit ~ season_visit, 
    familySEP ~ ethnicity_mother, 
    fatMass_child ~ age_child + airPollution_child + airPollution_preg + chemical + child_alcohol + child_diet + child_smoking + ethnicity_child + ethnicity_mother + familySEP + maternalAlcohol_preg + maternalDiet_preg + maternalSmoking_preg + otherChemicals_child + otherChemicals_preg + paternalSmoking_preg + sex_child, 
    gestational_age ~ airPollution_preg + ethnicity_child + ethnicity_mother + maternalAlcohol_preg + maternalDiet_preg + maternalSEP_preg + maternalSmoking_preg + otherChemicals_preg + paternalSEP_preg + paternalSmoking_preg, 
    intelligence ~ age_child + airPollution_child + airPollution_preg + breastfeeding + bw + chemical + child_alcohol + child_diet + child_smoking + edu_child + familySEP + gestational_age + intelligence_SNPs + maternalAlcohol_preg + maternalDiet_preg + maternalIodine_preg + maternalIron_preg + maternalSEP_preg + maternalSmoking_preg + maternal_folicAcid_preg + neuropsychologicalDiagnosis_child + otherChemicals_child + otherChemicals_preg + paternalSEP_preg + paternalSmoking_preg + qualityTesting_child + water_child + water_preg, 
    intelligence_SNPs ~ ethnicity_child + ethnicity_mother, 
    lipids ~ child_diet + ethnicity_child + fatMass_child + sex_child + time_lastMeal, 
    maternalAlcohol_preg ~ ethnicity_mother + maternalSEP_preg + paternalSEP_preg, 
    maternalDiet_preg ~ cohort + ethnicity_mother + maternalSEP_preg + paternalSEP_preg, 
    maternalIodine_preg ~ ethnicity_mother + maternalSEP_preg + paternalSEP_preg, 
    maternalIron_preg ~ ethnicity_mother + maternalSEP_preg + paternalSEP_preg, 
    maternalSEP_preg ~ ethnicity_mother, 
    maternalSmoking_preg ~ ethnicity_mother + maternalSEP_preg + paternalSEP_preg, 
    maternal_folicAcid_preg ~ ethnicity_mother + maternalSEP_preg + paternalSEP_preg, 
    neuropsychologicalDiagnosis_child ~ age_child + airPollution_child + airPollution_preg + breastfeeding + bw + child_alcohol + child_diet + child_smoking + ethnicity_child + ethnicity_mother + familySEP + gestational_age + maternalAlcohol_preg + maternalDiet_preg + maternalIodine_preg + maternalIron_preg + maternalSEP_preg + maternalSmoking_preg + maternal_folicAcid_preg + otherChemicals_child + otherChemicals_preg + paternalSEP_preg + paternalSmoking_preg + sex_child, 
    otherChemicals_child ~ child_diet + child_smoking + cohort + familySEP + water_child, 
    otherChemicals_preg ~ cohort + maternalSEP_preg + maternalSmoking_preg + paternalSEP_preg + paternalSmoking_preg + water_preg, 
    paternalSmoking_preg ~ maternalSEP_preg + paternalSEP_preg, 
    qualityTesting_child ~ child_depression + envFactors_visit, 
    water_child ~ familySEP, 
    water_preg ~ cohort + ethnicity_mother + maternalSEP_preg + paternalSEP_preg, 
    exposure = "chemical", 
    outcome = "intelligence"
  )
  ##############################################################################
  
  ##############################################################################
  chem_to_marker <- ggdag::dagify(
    
  )
  ##############################################################################
  
  ##############################################################################
  marker_to_out <- ggdag::dagify(
    
  )
  ##############################################################################
  
  return(list(
    chem_to_out = chem_to_out, 
    chem_to_marker = chem_to_marker, 
    marker_to_out = marker_to_out)
    )
  
}
