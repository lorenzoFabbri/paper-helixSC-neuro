# DAGs based on research questions defined in the Causal Roadmap

dag_raw <- function() {
  ##############################################################################
  nodes <- 'dag {
    bb="0,0,1,1"
    age_child [pos="0.487,0.831"]
    biomarker [pos="0.528,0.495"]
    breastfeeding [pos="0.183,0.687"]
    bw [pos="0.083,0.775"]
    characteristics_child [pos="0.477,0.676"]
    chemical [pos="0.135,0.488"]
    child_diet [pos="0.085,0.079"]
    child_smoking [pos="0.195,0.079"]
    cohort [pos="0.575,0.686"]
    creatinine [pos="0.498,0.085"]
    envFactors_visit [pos="0.870,0.065"]
    ethnicity_child [pos="0.492,0.912"]
    ethnicity_mother [pos="0.590,0.915"]
    familySEP [pos="0.873,0.683"]
    gestational_age [pos="0.080,0.684"]
    maternalAlcohol_preg [pos="0.209,0.258"]
    maternalDiet_preg [pos="0.086,0.157"]
    maternalSEP_preg [pos="0.879,0.766"]
    maternalSmoking_preg [pos="0.200,0.170"]
    neuropsychologicalDiagnosis_child [pos="0.876,0.247"]
    outcome [pos="0.894,0.492"]
    paternalSEP_preg [pos="0.883,0.855"]
    qualityTesting_child [pos="0.868,0.147"]
    season_visit [pos="0.780,0.064"]
    sex_child [pos="0.478,0.754"]
    time_lastMeal [pos="0.506,0.172"]
  }'
  ##############################################################################
  
  ##############################################################################
  chem_to_marker <- 'dag {
    bb="0,0,1,1"
    age_child [pos="0.487,0.831"]
    biomarker [outcome,pos="0.520,0.530"]
    breastfeeding [pos="0.183,0.687"]
    bw [pos="0.083,0.775"]
    characteristics_child [pos="0.477,0.676"]
    chemical [exposure,pos="0.135,0.488"]
    child_diet [pos="0.085,0.079"]
    child_smoking [pos="0.195,0.079"]
    cohort [pos="0.611,0.690"]
    creatinine [pos="0.498,0.085"]
    envFactors_visit [pos="0.870,0.065"]
    ethnicity_child [pos="0.492,0.912"]
    ethnicity_mother [pos="0.614,0.911"]
    familySEP [pos="0.873,0.683"]
    gestational_age [pos="0.080,0.684"]
    maternalAlcohol_preg [pos="0.209,0.258"]
    maternalDiet_preg [pos="0.086,0.157"]
    maternalSEP_preg [pos="0.917,0.777"]
    maternalSmoking_preg [pos="0.200,0.170"]
    neuropsychologicalDiagnosis_child [pos="0.876,0.247"]
    outcome [pos="0.894,0.492"]
    paternalSEP_preg [pos="0.927,0.866"]
    qualityTesting_child [pos="0.868,0.147"]
    season_visit [pos="0.780,0.064"]
    sex_child [pos="0.478,0.754"]
    time_lastMeal [pos="0.506,0.172"]
    age_child -> biomarker
    age_child -> characteristics_child
    age_child -> creatinine
    age_child -> outcome
    biomarker -> outcome
    breastfeeding -> outcome
    bw -> characteristics_child
    characteristics_child -> biomarker
    characteristics_child -> chemical
    characteristics_child -> creatinine
    characteristics_child -> outcome
    chemical -> biomarker
    chemical -> outcome
    child_diet -> biomarker
    child_diet -> characteristics_child
    child_diet -> chemical
    child_diet -> outcome
    child_smoking -> biomarker
    child_smoking -> characteristics_child
    child_smoking -> creatinine
    child_smoking -> outcome
    cohort -> biomarker
    cohort -> bw
    cohort -> characteristics_child
    cohort -> chemical
    cohort -> child_diet
    cohort -> outcome
    creatinine -> biomarker
    creatinine -> chemical
    creatinine -> outcome
    envFactors_visit -> outcome
    envFactors_visit -> qualityTesting_child
    ethnicity_child -> biomarker
    ethnicity_child -> bw
    ethnicity_child -> characteristics_child
    ethnicity_child -> chemical
    ethnicity_child -> child_diet
    ethnicity_child -> child_smoking
    ethnicity_child -> creatinine
    ethnicity_child -> outcome
    ethnicity_mother -> biomarker
    ethnicity_mother -> breastfeeding
    ethnicity_mother -> bw
    ethnicity_mother -> characteristics_child
    ethnicity_mother -> child_diet
    ethnicity_mother -> familySEP
    ethnicity_mother -> maternalAlcohol_preg
    ethnicity_mother -> maternalDiet_preg
    ethnicity_mother -> maternalSEP_preg
    ethnicity_mother -> maternalSmoking_preg
    ethnicity_mother -> outcome
    familySEP -> biomarker
    familySEP -> characteristics_child
    familySEP -> chemical
    familySEP -> child_diet
    familySEP -> child_smoking
    familySEP -> creatinine
    familySEP -> outcome
    gestational_age -> bw
    gestational_age -> characteristics_child
    maternalAlcohol_preg -> bw
    maternalAlcohol_preg -> characteristics_child
    maternalAlcohol_preg -> outcome
    maternalDiet_preg -> characteristics_child
    maternalDiet_preg -> outcome
    maternalSEP_preg -> breastfeeding
    maternalSEP_preg -> bw
    maternalSEP_preg -> characteristics_child
    maternalSEP_preg -> familySEP
    maternalSEP_preg -> maternalAlcohol_preg
    maternalSEP_preg -> maternalDiet_preg
    maternalSEP_preg -> maternalSmoking_preg
    maternalSEP_preg -> neuropsychologicalDiagnosis_child
    maternalSEP_preg -> outcome
    maternalSmoking_preg -> bw
    maternalSmoking_preg -> characteristics_child
    maternalSmoking_preg -> outcome
    neuropsychologicalDiagnosis_child -> outcome
    paternalSEP_preg -> breastfeeding
    paternalSEP_preg -> bw
    paternalSEP_preg -> characteristics_child
    paternalSEP_preg -> familySEP
    paternalSEP_preg -> maternalAlcohol_preg
    paternalSEP_preg -> maternalDiet_preg
    paternalSEP_preg -> maternalSmoking_preg
    paternalSEP_preg -> outcome
    season_visit -> biomarker
    season_visit -> chemical
    sex_child -> biomarker
    sex_child -> characteristics_child
    sex_child -> chemical
    sex_child -> child_diet
    sex_child -> child_smoking
    sex_child -> creatinine
    sex_child -> neuropsychologicalDiagnosis_child
    sex_child -> outcome
    time_lastMeal -> biomarker
    time_lastMeal -> chemical
  }'
  ##############################################################################
  
  ##############################################################################
  marker_to_out <- 'dag {
    bb="0,0,1,1"
    age_child [pos="0.487,0.831"]
    biomarker [exposure,pos="0.520,0.530"]
    breastfeeding [pos="0.183,0.687"]
    bw [pos="0.083,0.775"]
    characteristics_child [pos="0.477,0.676"]
    chemical [pos="0.135,0.488"]
    child_diet [pos="0.085,0.079"]
    child_smoking [pos="0.195,0.079"]
    cohort [pos="0.611,0.690"]
    creatinine [pos="0.498,0.085"]
    envFactors_visit [pos="0.870,0.065"]
    ethnicity_child [pos="0.492,0.912"]
    ethnicity_mother [pos="0.614,0.911"]
    familySEP [pos="0.873,0.683"]
    gestational_age [pos="0.080,0.684"]
    maternalAlcohol_preg [pos="0.209,0.258"]
    maternalDiet_preg [pos="0.086,0.157"]
    maternalSEP_preg [pos="0.917,0.777"]
    maternalSmoking_preg [pos="0.200,0.170"]
    neuropsychologicalDiagnosis_child [pos="0.876,0.247"]
    outcome [outcome,pos="0.894,0.492"]
    paternalSEP_preg [pos="0.927,0.866"]
    qualityTesting_child [pos="0.868,0.147"]
    season_visit [pos="0.780,0.064"]
    sex_child [pos="0.478,0.754"]
    time_lastMeal [pos="0.506,0.172"]
    age_child -> biomarker
    age_child -> characteristics_child
    age_child -> creatinine
    age_child -> outcome
    biomarker -> outcome
    breastfeeding -> outcome
    bw -> characteristics_child
    characteristics_child -> biomarker
    characteristics_child -> chemical
    characteristics_child -> creatinine
    characteristics_child -> outcome
    chemical -> biomarker
    chemical -> outcome
    child_diet -> biomarker
    child_diet -> characteristics_child
    child_diet -> chemical
    child_diet -> outcome
    child_smoking -> biomarker
    child_smoking -> characteristics_child
    child_smoking -> creatinine
    child_smoking -> outcome
    cohort -> biomarker
    cohort -> bw
    cohort -> characteristics_child
    cohort -> chemical
    cohort -> child_diet
    cohort -> outcome
    creatinine -> biomarker
    creatinine -> chemical
    creatinine -> outcome
    envFactors_visit -> outcome
    envFactors_visit -> qualityTesting_child
    ethnicity_child -> biomarker
    ethnicity_child -> bw
    ethnicity_child -> characteristics_child
    ethnicity_child -> chemical
    ethnicity_child -> child_diet
    ethnicity_child -> child_smoking
    ethnicity_child -> creatinine
    ethnicity_child -> outcome
    ethnicity_mother -> biomarker
    ethnicity_mother -> breastfeeding
    ethnicity_mother -> bw
    ethnicity_mother -> characteristics_child
    ethnicity_mother -> child_diet
    ethnicity_mother -> familySEP
    ethnicity_mother -> maternalAlcohol_preg
    ethnicity_mother -> maternalDiet_preg
    ethnicity_mother -> maternalSEP_preg
    ethnicity_mother -> maternalSmoking_preg
    ethnicity_mother -> outcome
    familySEP -> biomarker
    familySEP -> characteristics_child
    familySEP -> chemical
    familySEP -> child_diet
    familySEP -> child_smoking
    familySEP -> creatinine
    familySEP -> outcome
    gestational_age -> bw
    gestational_age -> characteristics_child
    maternalAlcohol_preg -> bw
    maternalAlcohol_preg -> characteristics_child
    maternalAlcohol_preg -> outcome
    maternalDiet_preg -> characteristics_child
    maternalDiet_preg -> outcome
    maternalSEP_preg -> breastfeeding
    maternalSEP_preg -> bw
    maternalSEP_preg -> characteristics_child
    maternalSEP_preg -> familySEP
    maternalSEP_preg -> maternalAlcohol_preg
    maternalSEP_preg -> maternalDiet_preg
    maternalSEP_preg -> maternalSmoking_preg
    maternalSEP_preg -> neuropsychologicalDiagnosis_child
    maternalSEP_preg -> outcome
    maternalSmoking_preg -> bw
    maternalSmoking_preg -> characteristics_child
    maternalSmoking_preg -> outcome
    neuropsychologicalDiagnosis_child -> outcome
    paternalSEP_preg -> breastfeeding
    paternalSEP_preg -> bw
    paternalSEP_preg -> characteristics_child
    paternalSEP_preg -> familySEP
    paternalSEP_preg -> maternalAlcohol_preg
    paternalSEP_preg -> maternalDiet_preg
    paternalSEP_preg -> maternalSmoking_preg
    paternalSEP_preg -> outcome
    season_visit -> biomarker
    season_visit -> chemical
    sex_child -> biomarker
    sex_child -> characteristics_child
    sex_child -> chemical
    sex_child -> child_diet
    sex_child -> child_smoking
    sex_child -> creatinine
    sex_child -> neuropsychologicalDiagnosis_child
    sex_child -> outcome
    time_lastMeal -> biomarker
    time_lastMeal -> chemical
  }'
  ##############################################################################
  
  return(list(
    nodes = nodes, 
    chem_to_marker = chem_to_marker, 
    marker_to_out = marker_to_out)
    )
  
}
