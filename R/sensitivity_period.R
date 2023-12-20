################################################################################
Sys.setenv(is_hpc = TRUE)
parallel <- TRUE
num_workers <- 2
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
source("R/dictionaries.R")
source("R/data.R")
source("R/utils.R")

params_dag <- list(
  type = "minimal",
  effect = "total"
)
rq <- Sys.getenv("TAR_PROJECT")
exposure <- "chemical"
outcome <- "biomarker"
tbl_outcomes <- tibble::tibble(
  name = vars_of_interest(append_to_chem = NULL)$new_metabolites |>
    stringr::str_to_lower(),
  outcome = vars_of_interest(append_to_chem = NULL)$new_metabolites
)
################################################################################

################################################################################
if (parallel == TRUE) {
  future::plan(future::multisession, workers = num_workers)
}

# Load DAG and data
dag <- load_dag(
  dags = dags(),
  exposure = exposure,
  outcome = outcome,
  params_dag = params_dag
)
load_dat <- rq_load_data(res_dag = dag)

# Loop over time points
time_steps <- 5:8
progressr::with_progress({
  p <- progressr::progressor(steps = length(time_steps))
  
  ret <- furrr::future_map(time_steps, function(idx, p) {
    p()
    
    # Pre-process data
    preproc_dat <- rq_prepare_data(
      dat = load_dat,
      filter_panel = TRUE,
      type_sample_hcp = idx,
      is_sa = FALSE
    )
    # Estimate balancing weights
    ww <- rq_estimate_weights(
      dat = preproc_dat,
      by = NULL,
      include_selection_weights = TRUE,
      save_results = TRUE,
      parallel = FALSE,
      workers = 10
    )
    # Loop over outcomes
    res <- lapply(tbl_outcomes$outcome, function(out) {
      # Fit outcome model
      fits <- rq_fit_model_weighted(
        dat = preproc_dat,
        outcome = out,
        by = NULL,
        is_panel = TRUE,
        weights = ww$estimated_weights,
        parallel = FALSE,
        workers = 10
      )
      # Estimate marginal contrasts
      rq_estimate_marginal_effects(
        fits = fits$fits,
        by = NULL,
        is_hcp = TRUE,
        parallel = TRUE,
        workers = 2
      )
    }) # End loop over outcomes
    
    return(res)
  },
  .options = furrr::furrr_options(seed = TRUE),
  p = p
  ) # End loop over time steps
}) # End progress bar
################################################################################

names(ret) <- c("nu4mu5", "nu5mu6", "nu6mu7", "week")
ret_proc <- lapply(ret, function(x) {
  names(x) <- tbl_outcomes$name
  x
})

comparisons_by_chemout <- lapply(ret_proc, function(x) {
  tmp <- lapply(x, function(y) {
    lapply(y$marginal_effects, function(z) {
      marginaleffects::posterior_draws(
        x = z$comparisons,
        shape = "rvar"
      )
    }) |>
      dplyr::bind_rows()
  }) |>
    dplyr::bind_rows(.id = "outcome") |>
    tidylog::select(
      outcome, variable, estimate, rvar
    )
}) |>
  dplyr::bind_rows(.id = "time_period")
to_plot_df <- comparisons_by_chemout |>
  dplyr::group_split(outcome)
################################################################################
