################################################################################
Sys.setenv(is_hpc = FALSE)
# Custom path to _targets for different research questions
path_store <- ifelse(
  Sys.getenv("is_hpc"), 
  paste0(
    "/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/",
    "data/data_paper3/_targets/_targetsRQ"
  ), 
  "~/mounts/rstudioserver/data/data_paper3/_targets/_targetsRQ"
)
Sys.setenv(path_store = path_store)
rq <- "2"
Sys.setenv(TAR_PROJECT = paste0("rq", rq))
store <- paste0(path_store, rq)
################################################################################

################################################################################
source("DAGs/dag_v2.R")
source("code/dictionaries.R")
source("code/data.R")
source("code/utils.R")

targets::tar_option_set(
  format = "qs"
)

params_dag <- list(
  type = "minimal",
  effect = "total"
)
rq <- Sys.getenv("TAR_PROJECT")
exposure <- "chemical"
outcome <- "biomarker"
tbl_outcomes <- tibble::tibble(
  name = vars_of_interest()$new_metabolites |>
    stringr::str_to_lower(),
  outcome = vars_of_interest()$new_metabolites
)
################################################################################

################################################################################
dag <- load_dag(
  dags = dags(),
  exposure = exposure,
  outcome = outcome,
  params_dag = params_dag
)
load_dat <- rq_load_data(res_dag = dag)

ret <- lapply(5:8, function(idx) {
  preproc_dat <- rq_prepare_data(
    dat = load_dat,
    filter_panel = TRUE,
    type_sample_hcp = idx
  )
  
  ww <- rq_estimate_weights(
    dat = preproc_dat,
    include_selection_weights = TRUE,
    save_results = FALSE,
    parallel = FALSE,
    workers = 10
  )
  
  res <- lapply(tbl_outcomes$outcome, function(out) {
    fits <- rq_fit_model_weighted(
      dat = preproc_dat,
      outcome = out,
      weights = ww$estimated_weights,
      parallel = FALSE,
      workers = 10
    )
    
    rq_estimate_marginal_effects(
      fits = fits$fits,
      parallel = TRUE,
      workers = 2
    )
  }) # End loop over outcomes
}) # End loop over types of sample
################################################################################
