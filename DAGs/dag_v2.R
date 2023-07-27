# DAGs based on research questions defined in the Causal Roadmap

dags <- function() {
  ##############################################################################
  chem_to_marker <- ggdag::dagify(
    biomarker ~ age_child + characteristics_child + chemical + child_diet + child_smoking + cohort + creatinine + ethnicity_child + ethnicity_mother + familySEP + season_visit + sex_child + time_lastMeal,
    breastfeeding ~ ethnicity_mother + maternalSEP_preg + paternalSEP_preg,
    bw ~ cohort + ethnicity_child + ethnicity_mother + gestational_age + maternalAlcohol_preg + maternalSEP_preg + maternalSmoking_preg + paternalSEP_preg,
    characteristics_child ~ age_child + bw + child_diet + child_smoking + cohort + ethnicity_child + ethnicity_mother + familySEP + gestational_age + maternalAlcohol_preg + maternalDiet_preg + maternalSEP_preg + maternalSmoking_preg + paternalSEP_preg + sex_child,
    chemical ~ characteristics_child + child_diet + cohort + creatinine + ethnicity_child + familySEP + season_visit + sex_child + time_lastMeal,
    child_diet ~ cohort + ethnicity_child + ethnicity_mother + familySEP + sex_child,
    child_smoking ~ ethnicity_child + familySEP + sex_child,
    creatinine ~ age_child + characteristics_child + child_smoking + ethnicity_child + familySEP + sex_child,
    familySEP ~ ethnicity_mother + maternalSEP_preg + paternalSEP_preg,
    maternalAlcohol_preg ~ ethnicity_mother + maternalSEP_preg + paternalSEP_preg,
    maternalDiet_preg ~ ethnicity_mother + maternalSEP_preg + paternalSEP_preg,
    maternalSEP_preg ~ ethnicity_mother,
    maternalSmoking_preg ~ ethnicity_mother + maternalSEP_preg + paternalSEP_preg,
    neuropsychologicalDiagnosis_child ~ maternalSEP_preg + sex_child,
    outcome ~ age_child + biomarker + breastfeeding + characteristics_child + chemical + child_diet + child_smoking + cohort + creatinine + envFactors_visit + ethnicity_child + ethnicity_mother + familySEP + maternalAlcohol_preg + maternalDiet_preg + maternalSEP_preg + maternalSmoking_preg + neuropsychologicalDiagnosis_child + paternalSEP_preg + sex_child,
    qualityTesting_child ~ envFactors_visit,
    exposure = "chemical",
    outcome = "biomarker"
  )
  ##############################################################################
  
  ##############################################################################
  marker_to_out <- ggdag::dagify(
    biomarker ~ age_child + characteristics_child + chemical + child_diet + child_smoking + cohort + creatinine + ethnicity_child + ethnicity_mother + familySEP + season_visit + sex_child + time_lastMeal,
    breastfeeding ~ ethnicity_mother + maternalSEP_preg + paternalSEP_preg,
    bw ~ cohort + ethnicity_child + ethnicity_mother + gestational_age + maternalAlcohol_preg + maternalSEP_preg + maternalSmoking_preg + paternalSEP_preg,
    characteristics_child ~ age_child + bw + child_diet + child_smoking + cohort + ethnicity_child + ethnicity_mother + familySEP + gestational_age + maternalAlcohol_preg + maternalDiet_preg + maternalSEP_preg + maternalSmoking_preg + paternalSEP_preg + sex_child,
    chemical ~ characteristics_child + child_diet + cohort + creatinine + ethnicity_child + familySEP + season_visit + sex_child + time_lastMeal,
    child_diet ~ cohort + ethnicity_child + ethnicity_mother + familySEP + sex_child,
    child_smoking ~ ethnicity_child + familySEP + sex_child,
    creatinine ~ age_child + characteristics_child + child_smoking + ethnicity_child + familySEP + sex_child,
    familySEP ~ ethnicity_mother + maternalSEP_preg + paternalSEP_preg,
    maternalAlcohol_preg ~ ethnicity_mother + maternalSEP_preg + paternalSEP_preg,
    maternalDiet_preg ~ ethnicity_mother + maternalSEP_preg + paternalSEP_preg,
    maternalSEP_preg ~ ethnicity_mother,
    maternalSmoking_preg ~ ethnicity_mother + maternalSEP_preg + paternalSEP_preg,
    neuropsychologicalDiagnosis_child ~ maternalSEP_preg + sex_child,
    outcome ~ age_child + biomarker + breastfeeding + characteristics_child + chemical + child_diet + child_smoking + cohort + creatinine + envFactors_visit + ethnicity_child + ethnicity_mother + familySEP + maternalAlcohol_preg + maternalDiet_preg + maternalSEP_preg + maternalSmoking_preg + neuropsychologicalDiagnosis_child + paternalSEP_preg + sex_child,
    qualityTesting_child ~ envFactors_visit,
    exposure = "biomarker",
    outcome = "outcome"
  )
  ##############################################################################
  
  return(list(
    chem_to_marker = chem_to_marker, 
    marker_to_out = marker_to_out)
    )
}
