# DAGs based on research questions defined in the Causal Roadmap

dag_raw <- function() {
  ##############################################################################
  nodes <- 'dag {
  bb="0,0,1,1"
  age_child [pos="0.222,0.285"]
  airPollution_child [pos="0.175,0.940"]
  airPollution_preg [pos="0.177,0.769"]
  biomarker [pos="0.484,0.433"]
  breastfeeding [pos="0.676,0.897"]
  bw [pos="0.665,0.685"]
  characteristics_child [pos="0.133,0.308"]
  chemical [pos="0.114,0.497"]
  child_diet [pos="0.420,0.066"]
  child_smoking [pos="0.771,0.067"]
  cohort [pos="0.223,0.067"]
  envFactors_visit [pos="0.869,0.769"]
  ethnicity_child [pos="0.223,0.137"]
  ethnicity_mother [pos="0.222,0.210"]
  familySEP [pos="0.504,0.249"]
  gestational_age [pos="0.675,0.766"]
  intelligence [pos="0.861,0.502"]
  maternalAlcohol_preg [pos="0.638,0.243"]
  maternalDiet_preg [pos="0.321,0.066"]
  maternalSEP_preg [pos="0.507,0.065"]
  maternalSmoking_preg [pos="0.632,0.067"]
  neuropsychologicalDiagnosis_child [pos="0.891,0.310"]
  otherChemicals_child [pos="0.176,0.854"]
  otherChemicals_preg [pos="0.176,0.684"]
  paternalSEP_preg [pos="0.507,0.157"]
  qualityTesting_child [pos="0.888,0.227"]
  season_visit [pos="0.071,0.147"]
  sex_child [pos="0.220,0.358"]
  time_lastMeal [pos="0.287,0.684"]
  }'
  ##############################################################################
  
  ##############################################################################
  chem_to_out <- 'dag {
  bb="0,0,1,1"
  age_child [pos="0.222,0.285"]
  airPollution_child [pos="0.175,0.940"]
  airPollution_preg [pos="0.177,0.769"]
  breastfeeding [pos="0.676,0.897"]
  bw [pos="0.670,0.688"]
  characteristics_child [pos="0.133,0.308"]
  chemical [exposure,pos="0.114,0.497"]
  child_diet [pos="0.420,0.066"]
  child_smoking [pos="0.771,0.067"]
  cohort [pos="0.223,0.067"]
  envFactors_visit [pos="0.869,0.769"]
  ethnicity_child [pos="0.223,0.137"]
  ethnicity_mother [pos="0.222,0.210"]
  familySEP [pos="0.504,0.249"]
  gestational_age [pos="0.667,0.770"]
  intelligence [outcome,pos="0.861,0.502"]
  maternalAlcohol_preg [pos="0.644,0.246"]
  maternalDiet_preg [pos="0.321,0.066"]
  maternalSEP_preg [pos="0.516,0.070"]
  maternalSmoking_preg [pos="0.632,0.062"]
  neuropsychologicalDiagnosis_child [pos="0.891,0.310"]
  otherChemicals_child [pos="0.176,0.854"]
  otherChemicals_preg [pos="0.176,0.671"]
  paternalSEP_preg [pos="0.510,0.153"]
  qualityTesting_child [pos="0.888,0.227"]
  season_visit [pos="0.071,0.147"]
  sex_child [pos="0.220,0.358"]
  time_lastMeal [pos="0.287,0.684"]
  age_child -> characteristics_child
  age_child -> child_diet
  age_child -> child_smoking
  age_child -> intelligence
  age_child -> neuropsychologicalDiagnosis_child
  airPollution_child -> intelligence
  airPollution_child -> neuropsychologicalDiagnosis_child
  airPollution_preg -> bw
  airPollution_preg -> gestational_age
  airPollution_preg -> intelligence
  airPollution_preg -> neuropsychologicalDiagnosis_child
  breastfeeding -> bw
  breastfeeding -> intelligence
  breastfeeding -> neuropsychologicalDiagnosis_child
  bw -> intelligence
  bw -> neuropsychologicalDiagnosis_child
  characteristics_child -> chemical
  characteristics_child -> intelligence
  chemical -> intelligence
  child_diet -> characteristics_child
  child_diet -> chemical
  child_diet -> intelligence
  child_diet -> neuropsychologicalDiagnosis_child
  child_diet -> otherChemicals_child
  child_smoking -> characteristics_child
  child_smoking -> intelligence
  child_smoking -> neuropsychologicalDiagnosis_child
  child_smoking -> otherChemicals_child
  cohort -> airPollution_child
  cohort -> airPollution_preg
  cohort -> bw
  cohort -> characteristics_child
  cohort -> chemical
  cohort -> child_diet
  cohort -> maternalDiet_preg
  cohort -> otherChemicals_child
  cohort -> otherChemicals_preg
  envFactors_visit -> qualityTesting_child
  ethnicity_child -> bw
  ethnicity_child -> characteristics_child
  ethnicity_child -> child_smoking
  ethnicity_child -> gestational_age
  ethnicity_child -> neuropsychologicalDiagnosis_child
  ethnicity_mother -> breastfeeding
  ethnicity_mother -> bw
  ethnicity_mother -> characteristics_child
  ethnicity_mother -> child_diet
  ethnicity_mother -> familySEP
  ethnicity_mother -> gestational_age
  ethnicity_mother -> maternalAlcohol_preg
  ethnicity_mother -> maternalDiet_preg
  ethnicity_mother -> maternalSEP_preg
  ethnicity_mother -> maternalSmoking_preg
  ethnicity_mother -> neuropsychologicalDiagnosis_child
  familySEP -> airPollution_child
  familySEP -> chemical
  familySEP -> child_diet
  familySEP -> child_smoking
  familySEP -> intelligence
  familySEP -> neuropsychologicalDiagnosis_child
  familySEP -> otherChemicals_child
  gestational_age -> intelligence
  gestational_age -> neuropsychologicalDiagnosis_child
  maternalAlcohol_preg -> bw
  maternalAlcohol_preg -> gestational_age
  maternalAlcohol_preg -> intelligence
  maternalAlcohol_preg -> neuropsychologicalDiagnosis_child
  maternalDiet_preg -> bw
  maternalDiet_preg -> gestational_age
  maternalDiet_preg -> intelligence
  maternalDiet_preg -> neuropsychologicalDiagnosis_child
  maternalSEP_preg -> airPollution_preg
  maternalSEP_preg -> breastfeeding
  maternalSEP_preg -> bw
  maternalSEP_preg -> gestational_age
  maternalSEP_preg -> intelligence
  maternalSEP_preg -> maternalAlcohol_preg
  maternalSEP_preg -> maternalDiet_preg
  maternalSEP_preg -> maternalSmoking_preg
  maternalSEP_preg -> neuropsychologicalDiagnosis_child
  maternalSEP_preg -> otherChemicals_preg
  maternalSmoking_preg -> bw
  maternalSmoking_preg -> gestational_age
  maternalSmoking_preg -> intelligence
  maternalSmoking_preg -> neuropsychologicalDiagnosis_child
  maternalSmoking_preg -> otherChemicals_preg
  neuropsychologicalDiagnosis_child -> intelligence
  otherChemicals_child -> intelligence
  otherChemicals_child -> neuropsychologicalDiagnosis_child
  otherChemicals_preg -> bw
  otherChemicals_preg -> gestational_age
  otherChemicals_preg -> intelligence
  otherChemicals_preg -> neuropsychologicalDiagnosis_child
  paternalSEP_preg -> airPollution_preg
  paternalSEP_preg -> bw
  paternalSEP_preg -> gestational_age
  paternalSEP_preg -> intelligence
  paternalSEP_preg -> maternalAlcohol_preg
  paternalSEP_preg -> maternalDiet_preg
  paternalSEP_preg -> maternalSmoking_preg
  paternalSEP_preg -> neuropsychologicalDiagnosis_child
  paternalSEP_preg -> otherChemicals_preg
  qualityTesting_child -> intelligence
  season_visit -> airPollution_child
  season_visit -> chemical
  season_visit -> envFactors_visit
  sex_child -> bw
  sex_child -> characteristics_child
  sex_child -> child_diet
  sex_child -> child_smoking
  sex_child -> neuropsychologicalDiagnosis_child
  time_lastMeal -> chemical
  }'
  ##############################################################################
  
  ##############################################################################
  chem_to_marker <- ''
  ##############################################################################
  
  ##############################################################################
  marker_to_out <- 'dag {
  bb="0,0,1,1"
  age_child [pos="0.222,0.285"]
  airPollution_child [pos="0.175,0.940"]
  airPollution_preg [pos="0.179,0.774"]
  biomarker [exposure,pos="0.451,0.905"]
  breastfeeding [pos="0.676,0.897"]
  bw [pos="0.666,0.683"]
  characteristics_child [pos="0.133,0.308"]
  chemical [pos="0.154,0.493"]
  child_diet [pos="0.427,0.066"]
  child_smoking [pos="0.779,0.077"]
  cohort [pos="0.217,0.062"]
  envFactors_visit [pos="0.869,0.769"]
  ethnicity_child [pos="0.218,0.142"]
  ethnicity_mother [pos="0.222,0.210"]
  familySEP [pos="0.511,0.265"]
  gestational_age [pos="0.667,0.770"]
  intelligence [outcome,pos="0.861,0.502"]
  maternalAlcohol_preg [pos="0.644,0.246"]
  maternalDiet_preg [pos="0.317,0.081"]
  maternalSEP_preg [pos="0.516,0.070"]
  maternalSmoking_preg [pos="0.632,0.062"]
  neuropsychologicalDiagnosis_child [pos="0.891,0.310"]
  otherChemicals_child [pos="0.176,0.854"]
  otherChemicals_preg [pos="0.176,0.671"]
  paternalSEP_preg [pos="0.510,0.153"]
  qualityTesting_child [pos="0.888,0.227"]
  season_visit [pos="0.090,0.143"]
  sex_child [pos="0.226,0.372"]
  time_lastMeal [pos="0.295,0.683"]
  age_child -> biomarker
  age_child -> characteristics_child
  age_child -> child_diet
  age_child -> child_smoking
  age_child -> intelligence
  age_child -> neuropsychologicalDiagnosis_child
  airPollution_child -> biomarker
  airPollution_child -> intelligence
  airPollution_child -> neuropsychologicalDiagnosis_child
  airPollution_preg -> bw
  airPollution_preg -> gestational_age
  airPollution_preg -> intelligence
  airPollution_preg -> neuropsychologicalDiagnosis_child
  biomarker -> intelligence
  breastfeeding -> bw
  breastfeeding -> intelligence
  breastfeeding -> neuropsychologicalDiagnosis_child
  bw -> intelligence
  bw -> neuropsychologicalDiagnosis_child
  characteristics_child -> biomarker
  characteristics_child -> chemical
  characteristics_child -> intelligence
  chemical -> biomarker
  chemical -> intelligence
  child_diet -> biomarker
  child_diet -> characteristics_child
  child_diet -> chemical
  child_diet -> intelligence
  child_diet -> neuropsychologicalDiagnosis_child
  child_diet -> otherChemicals_child
  child_smoking -> biomarker
  child_smoking -> characteristics_child
  child_smoking -> intelligence
  child_smoking -> neuropsychologicalDiagnosis_child
  child_smoking -> otherChemicals_child
  cohort -> airPollution_child
  cohort -> airPollution_preg
  cohort -> biomarker
  cohort -> bw
  cohort -> characteristics_child
  cohort -> chemical
  cohort -> child_diet
  cohort -> maternalDiet_preg
  cohort -> otherChemicals_child
  cohort -> otherChemicals_preg
  envFactors_visit -> qualityTesting_child
  ethnicity_child -> biomarker
  ethnicity_child -> bw
  ethnicity_child -> characteristics_child
  ethnicity_child -> child_smoking
  ethnicity_child -> gestational_age
  ethnicity_child -> neuropsychologicalDiagnosis_child
  ethnicity_mother -> biomarker
  ethnicity_mother -> breastfeeding
  ethnicity_mother -> bw
  ethnicity_mother -> characteristics_child
  ethnicity_mother -> child_diet
  ethnicity_mother -> familySEP
  ethnicity_mother -> gestational_age
  ethnicity_mother -> maternalAlcohol_preg
  ethnicity_mother -> maternalDiet_preg
  ethnicity_mother -> maternalSEP_preg
  ethnicity_mother -> maternalSmoking_preg
  ethnicity_mother -> neuropsychologicalDiagnosis_child
  familySEP -> airPollution_child
  familySEP -> biomarker
  familySEP -> chemical
  familySEP -> child_diet
  familySEP -> child_smoking
  familySEP -> intelligence
  familySEP -> neuropsychologicalDiagnosis_child
  familySEP -> otherChemicals_child
  gestational_age -> intelligence
  gestational_age -> neuropsychologicalDiagnosis_child
  maternalAlcohol_preg -> bw
  maternalAlcohol_preg -> gestational_age
  maternalAlcohol_preg -> intelligence
  maternalAlcohol_preg -> neuropsychologicalDiagnosis_child
  maternalDiet_preg -> bw
  maternalDiet_preg -> gestational_age
  maternalDiet_preg -> intelligence
  maternalDiet_preg -> neuropsychologicalDiagnosis_child
  maternalSEP_preg -> airPollution_preg
  maternalSEP_preg -> breastfeeding
  maternalSEP_preg -> bw
  maternalSEP_preg -> gestational_age
  maternalSEP_preg -> intelligence
  maternalSEP_preg -> maternalAlcohol_preg
  maternalSEP_preg -> maternalDiet_preg
  maternalSEP_preg -> maternalSmoking_preg
  maternalSEP_preg -> neuropsychologicalDiagnosis_child
  maternalSEP_preg -> otherChemicals_preg
  maternalSmoking_preg -> bw
  maternalSmoking_preg -> gestational_age
  maternalSmoking_preg -> intelligence
  maternalSmoking_preg -> neuropsychologicalDiagnosis_child
  maternalSmoking_preg -> otherChemicals_preg
  neuropsychologicalDiagnosis_child -> intelligence
  otherChemicals_child -> biomarker
  otherChemicals_child -> intelligence
  otherChemicals_child -> neuropsychologicalDiagnosis_child
  otherChemicals_preg -> bw
  otherChemicals_preg -> gestational_age
  otherChemicals_preg -> intelligence
  otherChemicals_preg -> neuropsychologicalDiagnosis_child
  paternalSEP_preg -> airPollution_preg
  paternalSEP_preg -> bw
  paternalSEP_preg -> gestational_age
  paternalSEP_preg -> intelligence
  paternalSEP_preg -> maternalAlcohol_preg
  paternalSEP_preg -> maternalDiet_preg
  paternalSEP_preg -> maternalSmoking_preg
  paternalSEP_preg -> neuropsychologicalDiagnosis_child
  paternalSEP_preg -> otherChemicals_preg
  qualityTesting_child -> intelligence
  season_visit -> airPollution_child
  season_visit -> biomarker
  season_visit -> chemical
  season_visit -> envFactors_visit
  sex_child -> biomarker
  sex_child -> bw
  sex_child -> characteristics_child
  sex_child -> child_diet
  sex_child -> child_smoking
  sex_child -> neuropsychologicalDiagnosis_child
  time_lastMeal -> biomarker
  time_lastMeal -> chemical
  }'
  ##############################################################################
  
  return(list(
    nodes = nodes, 
    chem_to_out = chem_to_out, 
    chem_to_marker = chem_to_marker, 
    marker_to_out = marker_to_out)
    )
  
}
