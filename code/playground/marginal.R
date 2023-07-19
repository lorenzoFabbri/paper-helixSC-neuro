source("DAGs/dag_v2.R")
source("code/dictionaries.R")
source("code/data.R")
source("code/mtps.R")
source("code/research_questions/utils.R")
source("code/research_questions/rq1.R")

Sys.setenv(is_hpc = TRUE)
path_store <- ifelse(
  Sys.getenv("is_hpc"), 
  "~/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/_targets/_targetsRQ", 
  "~/mounts/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/_targets/_targetsRQ"
)
Sys.setenv(path_store = path_store)
path_store_res <- ifelse(
  Sys.getenv("is_hpc"), 
  "~/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/results/", 
  "~/mounts/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/results/"
)
Sys.setenv(path_store_res = path_store_res)
Sys.setenv(TAR_PROJECT = "rq01")
store <- paste0(path_store, "01")
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

rq <- Sys.getenv("TAR_PROJECT")
rq <- switch(rq, 
             "rq01" = "rq1", 
             "rq1" = "rq1")
params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
id_var <- params_dat$variables$identifier
outcome <- params_dat$variables[[rq]]$outcome
steps_outcome <- params_dat$variables$preproc_outcome
params_ana <- params_analyses()[[rq]]
list_exposures <- names(preproc_dat$exposures)
list_exposures <- setdiff(list_exposures, 
                          id_var)
list_covariates <- setdiff(colnames(preproc_dat$covariates), 
                           id_var)
estimated_weights <- readRDS(file = paste0(
  Sys.getenv("path_store_res"), 
  "weights_exposure_model/", 
  params_ana$method_weightit, 
  ".rds"
))
