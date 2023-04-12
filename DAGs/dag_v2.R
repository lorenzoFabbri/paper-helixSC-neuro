# DAGs based on research questions defined in the Causal Roadmap

dag_v2 <- function() {
  ##############################################################################
  chem_to_out <- ggdag::dagify(
    airPollution_child ~ cohort + familySEP + season_visit, 
    airPollution_preg ~ cohort + maternalSEP_preg + paternalSEP_preg, 
    breastfeeding ~ ethnicity_mother + maternalSEP_preg, 
    bw ~ airPollution_preg + breastfeeding + cohort + ethnicity_child + ethnicity_mother + maternalAlcohol_preg + maternalDiet_preg + maternalSEP_preg + maternalSmoking_preg + maternal_folicAcid_preg + otherChemicals_preg + paternalSEP_preg + sex_child, 
    chemical ~ chemicals_SNPs + child_diet + cohort + familySEP + season_visit + time_lastMeal + water_child, 
    chemicals_SNPs ~ ethnicity_child + ethnicity_mother, 
    child_diet ~ age_child + cohort + ethnicity_mother + familySEP + sex_child, 
    child_smoking ~ age_child + ethnicity_child + familySEP + sex_child, 
    creatinine ~ age_child + child_diet + fatMass_child + sex_child + time_lastMeal, 
    envFactors_visit ~ season_visit, 
    familySEP ~ ethnicity_mother, 
    fatMass_child ~ age_child + airPollution_child + airPollution_preg + chemical + child_diet + child_smoking + ethnicity_child + ethnicity_mother + familySEP + maternalAlcohol_preg + maternalDiet_preg + maternalSmoking_preg + otherChemicals_child + otherChemicals_preg + sex_child, 
    gestational_age ~ airPollution_preg + ethnicity_child + ethnicity_mother + maternalAlcohol_preg + maternalDiet_preg + maternalSEP_preg + maternalSmoking_preg + otherChemicals_preg + paternalSEP_preg, 
    intelligence ~ age_child + airPollution_child + airPollution_preg + breastfeeding + bw + chemical + child_diet + child_smoking + familySEP + gestational_age + intelligence_SNPs + maternalAlcohol_preg + maternalDiet_preg + maternalSEP_preg + maternalSmoking_preg + maternal_folicAcid_preg + neuropsychologicalDiagnosis_child + otherChemicals_child + otherChemicals_preg + paternalSEP_preg + qualityTesting_child + water_child, 
    intelligence_SNPs ~ ethnicity_child + ethnicity_mother, 
    lipids ~ child_diet + ethnicity_child + fatMass_child + sex_child + time_lastMeal, 
    maternalAlcohol_preg ~ ethnicity_mother + maternalSEP_preg + paternalSEP_preg, 
    maternalDiet_preg ~ cohort + ethnicity_mother + maternalSEP_preg + paternalSEP_preg, 
    maternalSEP_preg ~ ethnicity_mother, 
    maternalSmoking_preg ~ ethnicity_mother + maternalSEP_preg + paternalSEP_preg, 
    maternal_folicAcid_preg ~ ethnicity_mother + maternalSEP_preg + paternalSEP_preg, 
    neuropsychologicalDiagnosis_child ~ age_child + airPollution_child + airPollution_preg + breastfeeding + bw + child_diet + child_smoking + ethnicity_child + ethnicity_mother + familySEP + gestational_age + maternalAlcohol_preg + maternalDiet_preg + maternalSEP_preg + maternalSmoking_preg + maternal_folicAcid_preg + otherChemicals_child + otherChemicals_preg + paternalSEP_preg + sex_child, 
    otherChemicals_child ~ child_diet + child_smoking + cohort + familySEP + water_child, 
    otherChemicals_preg ~ cohort + maternalSEP_preg + maternalSmoking_preg + paternalSEP_preg, 
    qualityTesting_child ~ envFactors_visit, 
    water_child ~ familySEP, 
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
    airPollution_child ~ cohort + familySEP + season_visit,
    airPollution_preg ~ cohort + maternalSEP_preg + paternalSEP_preg,
    biomarker ~ age_child + airPollution_child + chemical + child_diet + child_smoking + cohort + creatinine + ethnicity_child + ethnicity_mother + familySEP + fatMass_child + otherChemicals_child + season_visit + sex_child + time_lastMeal,
    breastfeeding ~ ethnicity_mother + maternalSEP_preg,
    bw ~ airPollution_preg + breastfeeding + cohort + ethnicity_child + ethnicity_mother + maternalAlcohol_preg + maternalDiet_preg + maternalSEP_preg + maternalSmoking_preg + maternal_folicAcid_preg + otherChemicals_preg + paternalSEP_preg + sex_child,
    chemical ~ chemicals_SNPs + child_diet + cohort + familySEP + season_visit + time_lastMeal + water_child,
    chemicals_SNPs ~ ethnicity_child + ethnicity_mother,
    child_diet ~ age_child + cohort + ethnicity_mother + familySEP + sex_child,
    child_smoking ~ age_child + ethnicity_child + familySEP + sex_child,
    creatinine ~ age_child + child_diet + fatMass_child + sex_child + time_lastMeal,
    envFactors_visit ~ season_visit,
    familySEP ~ ethnicity_mother,
    fatMass_child ~ age_child + airPollution_child + airPollution_preg + chemical + child_diet + child_smoking + ethnicity_child + ethnicity_mother + familySEP + maternalAlcohol_preg + maternalDiet_preg + maternalSmoking_preg + otherChemicals_child + otherChemicals_preg + sex_child,
    gestational_age ~ airPollution_preg + ethnicity_child + ethnicity_mother + maternalAlcohol_preg + maternalDiet_preg + maternalSEP_preg + maternalSmoking_preg + otherChemicals_preg + paternalSEP_preg,
    intelligence ~ age_child + airPollution_child + airPollution_preg + biomarker + breastfeeding + bw + chemical + child_diet + child_smoking + familySEP + gestational_age + intelligence_SNPs + maternalAlcohol_preg + maternalDiet_preg + maternalSEP_preg + maternalSmoking_preg + maternal_folicAcid_preg + neuropsychologicalDiagnosis_child + otherChemicals_child + otherChemicals_preg + paternalSEP_preg + qualityTesting_child + water_child,
    intelligence_SNPs ~ ethnicity_child + ethnicity_mother,
    lipids ~ child_diet + ethnicity_child + fatMass_child + sex_child + time_lastMeal,
    maternalAlcohol_preg ~ ethnicity_mother + maternalSEP_preg + paternalSEP_preg,
    maternalDiet_preg ~ cohort + ethnicity_mother + maternalSEP_preg + paternalSEP_preg,
    maternalSEP_preg ~ ethnicity_mother,
    maternalSmoking_preg ~ ethnicity_mother + maternalSEP_preg + paternalSEP_preg,
    maternal_folicAcid_preg ~ ethnicity_mother + maternalSEP_preg + paternalSEP_preg,
    neuropsychologicalDiagnosis_child ~ age_child + airPollution_child + airPollution_preg + breastfeeding + bw + child_diet + child_smoking + ethnicity_child + ethnicity_mother + familySEP + gestational_age + maternalAlcohol_preg + maternalDiet_preg + maternalSEP_preg + maternalSmoking_preg + maternal_folicAcid_preg + otherChemicals_child + otherChemicals_preg + paternalSEP_preg + sex_child,
    otherChemicals_child ~ child_diet + child_smoking + cohort + familySEP + water_child,
    otherChemicals_preg ~ cohort + maternalSEP_preg + maternalSmoking_preg + paternalSEP_preg,
    qualityTesting_child ~ envFactors_visit,
    water_child ~ familySEP,
    exposure = "biomarker", 
    outcome = "intelligence"
  )
  ##############################################################################
  
  return(list(
    chem_to_out = chem_to_out, 
    chem_to_marker = chem_to_marker, 
    marker_to_out = marker_to_out)
    )
  
}
