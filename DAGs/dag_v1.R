# DAGs based on research questions defined in the Causal Roadmap

dag.v1 <- function() {
  ##############################################################################
  nodes <- dag{
    bb="0,0,1,1"
    age_child [pos="0.222,0.285"]
    airPollution_child [pos="0.175,0.940"]
    airPollution_preg [pos="0.177,0.769"]
    biomarker [pos="0.484,0.433"]
    bw [pos="0.665,0.685"]
    chemical [pos="0.114,0.497"]
    chemicals_SNPs [pos="0.066,0.685"]
    child_alcohol [pos="0.767,0.240"]
    child_depression [pos="0.751,0.678"]
    child_diet [pos="0.420,0.066"]
    child_smoking [pos="0.771,0.067"]
    cohort [pos="0.223,0.067"]
    creatinine [pos="0.069,0.845"]
    envFactors_visit [pos="0.869,0.769"]
    ethnicity_child [pos="0.223,0.137"]
    ethnicity_mother [pos="0.222,0.210"]
    familySEP [pos="0.504,0.249"]
    gestational_age [pos="0.675,0.766"]
    gravity [pos="0.067,0.765"]
    intelligence [pos="0.861,0.502"]
    intelligence_SNPs [pos="0.862,0.678"]
    lipids [pos="0.287,0.774"]
    maternalAlcohol_preg [pos="0.638,0.243"]
    maternalDiet_preg [pos="0.321,0.066"]
    maternalIodine_preg [pos="0.324,0.145"]
    maternalIron_preg [pos="0.325,0.225"]
    maternalSEP_preg [pos="0.507,0.065"]
    maternalSmoking_preg [pos="0.632,0.067"]
    maternal_folicAcid_preg [pos="0.324,0.301"]
    neuropsychologicalDiagnosis_child [pos="0.891,0.310"]
    otherChemicals_child [pos="0.176,0.854"]
    otherChemicals_preg [pos="0.176,0.684"]
    paternalSEP_preg [pos="0.507,0.157"]
    paternalSmoking_preg [pos="0.634,0.151"]
    qualityTesting_child [pos="0.888,0.227"]
    season_visit [pos="0.071,0.147"]
    sex_child [pos="0.220,0.358"]
    time_lastMeal [pos="0.287,0.684"]
  }
  ##############################################################################
  
  ##############################################################################
  chem_to_out <- dag{
    bb="0,0,1,1"
    age_child [pos="0.222,0.285"]
    airPollution_child [pos="0.175,0.940"]
    airPollution_preg [pos="0.177,0.769"]
    biomarker [pos="0.472,0.830"]
    bw [pos="0.670,0.688"]
    chemical [exposure,pos="0.114,0.497"]
    chemicals_SNPs [pos="0.066,0.685"]
    child_alcohol [pos="0.767,0.240"]
    child_depression [pos="0.751,0.678"]
    child_diet [pos="0.420,0.066"]
    child_smoking [pos="0.771,0.067"]
    cohort [pos="0.223,0.067"]
    creatinine [pos="0.045,0.546"]
    envFactors_visit [pos="0.869,0.769"]
    ethnicity_child [pos="0.223,0.137"]
    ethnicity_mother [pos="0.222,0.210"]
    familySEP [pos="0.504,0.249"]
    fatMass_child [pos="0.556,0.685"]
    gestational_age [pos="0.667,0.770"]
    gravity [pos="0.067,0.765"]
    intelligence [outcome,pos="0.861,0.502"]
    intelligence_SNPs [pos="0.862,0.678"]
    lipids [pos="0.403,0.816"]
    maternalAlcohol_preg [pos="0.644,0.246"]
    maternalDiet_preg [pos="0.321,0.066"]
    maternalIodine_preg [pos="0.318,0.156"]
    maternalIron_preg [pos="0.321,0.243"]
    maternalSEP_preg [pos="0.516,0.070"]
    maternalSmoking_preg [pos="0.632,0.062"]
    maternal_folicAcid_preg [pos="0.321,0.330"]
    neuropsychologicalDiagnosis_child [pos="0.891,0.310"]
    otherChemicals_child [pos="0.176,0.854"]
    otherChemicals_preg [pos="0.176,0.684"]
    paternalSEP_preg [pos="0.510,0.153"]
    paternalSmoking_preg [pos="0.634,0.151"]
    qualityTesting_child [pos="0.888,0.227"]
    season_visit [pos="0.071,0.147"]
    sex_child [pos="0.220,0.358"]
    time_lastMeal [pos="0.287,0.684"]
    age_child -> child_alcohol
    age_child -> child_depression
    age_child -> child_diet
    age_child -> child_smoking
    age_child -> fatMass_child
    age_child -> intelligence
    age_child -> neuropsychologicalDiagnosis_child
    bw -> intelligence
    bw -> neuropsychologicalDiagnosis_child
    chemical -> intelligence
    child_alcohol -> child_depression
    child_alcohol -> fatMass_child
    child_alcohol -> intelligence
    child_alcohol -> neuropsychologicalDiagnosis_child
    child_depression -> qualityTesting_child
    child_diet -> chemical
    child_diet -> child_depression
    child_diet -> fatMass_child
    child_diet -> intelligence
    child_diet -> lipids
    child_diet -> neuropsychologicalDiagnosis_child
    child_diet -> otherChemicals_child
    child_smoking -> child_depression
    child_smoking -> fatMass_child
    child_smoking -> intelligence
    child_smoking -> neuropsychologicalDiagnosis_child
    child_smoking -> otherChemicals_child
    cohort -> airPollution_child
    cohort -> airPollution_preg
    cohort -> bw
    cohort -> chemical
    cohort -> child_depression
    cohort -> child_diet
    cohort -> maternalDiet_preg
    cohort -> otherChemicals_child
    cohort -> otherChemicals_preg
    envFactors_visit -> qualityTesting_child
    ethnicity_child -> bw
    ethnicity_child -> chemicals_SNPs
    ethnicity_child -> child_alcohol
    ethnicity_child -> child_depression
    ethnicity_child -> child_smoking
    ethnicity_child -> fatMass_child
    ethnicity_child -> gestational_age
    ethnicity_child -> intelligence_SNPs
    ethnicity_child -> lipids
    ethnicity_child -> neuropsychologicalDiagnosis_child
    ethnicity_mother -> bw
    ethnicity_mother -> chemicals_SNPs
    ethnicity_mother -> child_depression
    ethnicity_mother -> child_diet
    ethnicity_mother -> familySEP
    ethnicity_mother -> fatMass_child
    ethnicity_mother -> gestational_age
    ethnicity_mother -> intelligence_SNPs
    ethnicity_mother -> maternalAlcohol_preg
    ethnicity_mother -> maternalDiet_preg
    ethnicity_mother -> maternalIodine_preg
    ethnicity_mother -> maternalIron_preg
    ethnicity_mother -> maternalSEP_preg
    ethnicity_mother -> maternalSmoking_preg
    ethnicity_mother -> maternal_folicAcid_preg
    ethnicity_mother -> neuropsychologicalDiagnosis_child
    familySEP -> airPollution_child
    familySEP -> chemical
    familySEP -> child_alcohol
    familySEP -> child_depression
    familySEP -> child_diet
    familySEP -> child_smoking
    familySEP -> fatMass_child
    familySEP -> intelligence
    familySEP -> neuropsychologicalDiagnosis_child
    familySEP -> otherChemicals_child
    fatMass_child -> lipids
    gestational_age -> intelligence
    gestational_age -> neuropsychologicalDiagnosis_child
    intelligence_SNPs -> intelligence
    maternalAlcohol_preg -> bw
    maternalAlcohol_preg -> child_depression
    maternalAlcohol_preg -> fatMass_child
    maternalAlcohol_preg -> gestational_age
    maternalAlcohol_preg -> intelligence
    maternalAlcohol_preg -> neuropsychologicalDiagnosis_child
    maternalDiet_preg -> bw
    maternalDiet_preg -> fatMass_child
    maternalDiet_preg -> gestational_age
    maternalDiet_preg -> intelligence
    maternalDiet_preg -> neuropsychologicalDiagnosis_child
    maternalIodine_preg -> bw
    maternalIodine_preg -> child_depression
    maternalIodine_preg -> intelligence
    maternalIodine_preg -> neuropsychologicalDiagnosis_child
    maternalIron_preg -> bw
    maternalIron_preg -> child_depression
    maternalIron_preg -> intelligence
    maternalIron_preg -> neuropsychologicalDiagnosis_child
    maternalSEP_preg -> airPollution_preg
    maternalSEP_preg -> bw
    maternalSEP_preg -> gestational_age
    maternalSEP_preg -> intelligence
    maternalSEP_preg -> maternalAlcohol_preg
    maternalSEP_preg -> maternalDiet_preg
    maternalSEP_preg -> maternalIodine_preg
    maternalSEP_preg -> maternalIron_preg
    maternalSEP_preg -> maternalSmoking_preg
    maternalSEP_preg -> maternal_folicAcid_preg
    maternalSEP_preg -> neuropsychologicalDiagnosis_child
    maternalSEP_preg -> otherChemicals_preg
    maternalSEP_preg -> paternalSmoking_preg
    maternalSmoking_preg -> bw
    maternalSmoking_preg -> child_depression
    maternalSmoking_preg -> fatMass_child
    maternalSmoking_preg -> gestational_age
    maternalSmoking_preg -> intelligence
    maternalSmoking_preg -> neuropsychologicalDiagnosis_child
    maternalSmoking_preg -> otherChemicals_preg
    maternal_folicAcid_preg -> bw
    maternal_folicAcid_preg -> child_depression
    maternal_folicAcid_preg -> intelligence
    maternal_folicAcid_preg -> neuropsychologicalDiagnosis_child
    neuropsychologicalDiagnosis_child -> intelligence
    paternalSEP_preg -> airPollution_preg
    paternalSEP_preg -> bw
    paternalSEP_preg -> child_depression
    paternalSEP_preg -> gestational_age
    paternalSEP_preg -> intelligence
    paternalSEP_preg -> maternalAlcohol_preg
    paternalSEP_preg -> maternalDiet_preg
    paternalSEP_preg -> maternalIodine_preg
    paternalSEP_preg -> maternalIron_preg
    paternalSEP_preg -> maternalSmoking_preg
    paternalSEP_preg -> maternal_folicAcid_preg
    paternalSEP_preg -> neuropsychologicalDiagnosis_child
    paternalSEP_preg -> otherChemicals_preg
    paternalSEP_preg -> paternalSmoking_preg
    paternalSmoking_preg -> bw
    paternalSmoking_preg -> child_depression
    paternalSmoking_preg -> fatMass_child
    paternalSmoking_preg -> gestational_age
    paternalSmoking_preg -> intelligence
    paternalSmoking_preg -> neuropsychologicalDiagnosis_child
    paternalSmoking_preg -> otherChemicals_preg
    qualityTesting_child -> intelligence
    season_visit -> airPollution_child
    season_visit -> chemical
    season_visit -> envFactors_visit
    sex_child -> bw
    sex_child -> child_alcohol
    sex_child -> child_depression
    sex_child -> child_diet
    sex_child -> child_smoking
    sex_child -> fatMass_child
    sex_child -> lipids
    sex_child -> neuropsychologicalDiagnosis_child
    time_lastMeal -> chemical
    time_lastMeal -> creatinine
    time_lastMeal -> lipids
  }
  ##############################################################################
  
  ##############################################################################
  chem_to_marker
  ##############################################################################
  
  ##############################################################################
  marker_to_out
  ##############################################################################
  
  return(list(
    nodes = nodes, 
    chem_to_out = chem_to_out, 
    chem_to_marker = chem_to_marker, 
    marker_to_out = marker_to_out)
    )
  
}
