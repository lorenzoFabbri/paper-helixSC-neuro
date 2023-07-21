source("code/dictionaries.R")
source("DAGs/dag_v2.R")
source("code/data.R")
source("code/research_questions/utils.R")
source("code/research_questions/rq1.R")

Sys.setenv(is_hpc = FALSE)
# Custom path to _targets for different research questions
path_store <- ifelse(
  Sys.getenv("is_hpc"), 
  "/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/_targets/_targetsRQ", 
  "~/mounts/rstudioserver/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/_targets/_targetsRQ"
)
Sys.setenv(path_store = path_store)
# Path to store results
path_store_res <- ifelse(
  Sys.getenv("is_hpc"), 
  "/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/results/", 
  "~/mounts/rstudioserver/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/results/"
)
Sys.setenv(path_store_res = path_store_res)
Sys.setenv(TAR_PROJECT = "rq1")
store <- paste0(path_store, "1")
################################################################################

params_dag <- list(
  type = "minimal", 
  effect = "total"
)
dag <- load_dag(dags = dags(), 
                exposure = "chemical", 
                outcome = "intelligence", 
                params_dag = params_dag)
load_dat <- rq_load_data(ids_other_covars = c(), 
                         res_dag = dag)
preproc_dat <- rq1_prepare_data(dat = load_dat)
df <- dplyr::inner_join(
  preproc_dat$outcome, 
  preproc_dat$covariates, 
  by = "HelixID"
) |>
  myphd::extract_cohort(id_var = "HelixID")
################################################################################

vars_to_desc <- c(
  "HelixID", 
  "hs_age_years", "cohort", 
  "e3_bw", "e3_gac", 
  "hs_head_circ", "hs_waist", 
  "hs_hitrtse"
)
desc_dat <- myphd::describe_data(df |>
                                   dplyr::select(dplyr::all_of(vars_to_desc)), 
                                 id_var = "HelixID", 
                                 grouping_var = "cohort")
saveRDS(desc_dat$step3, 
        file = "output/presentations/cep_internal_230726/data/tab1")
ggplot2::ggsave(filename = "output/presentations/cep_internal_230726/artwork/corr.png", 
                plot = desc_dat$step6$viz_corr_spearman)
################################################################################
