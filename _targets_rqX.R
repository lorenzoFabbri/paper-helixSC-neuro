source("DAGs/dag_v2.R")
source("R/dictionaries.R")
source("R/data.R")
source("R/utils.R")

targets::tar_option_set(
  format = "qs"
)

params_dag <- list(
  type = "minimal",
  effect = "total"
)
rq <- Sys.getenv("TAR_PROJECT")
exposure <- switch(rq,
                   "rq1" = "chemical",
                   "rq2" = "chemical",
                   "rq3" = "biomarker",
                   "rq4" = "biomarker")
outcome <- switch(rq,
                  "rq1" = "outcome",
                  "rq2" = "biomarker",
                  "rq3" = "outcome",
                  "rq4" = "chemical")

if (rq == "rq2") {
  tbl_outcomes <- tibble::tibble(
    name = vars_of_interest(append_to_chem = NULL)$new_metabolites |>
      stringr::str_to_lower(),
    outcome = vars_of_interest(append_to_chem = NULL)$new_metabolites
  )
} else {
  tbl_outcomes <- tibble::tibble(
    name = params(is_hpc = Sys.getenv("is_hpc"))$variables[[rq]]$outcome |>
      stringr::str_to_lower(),
    outcome = params(is_hpc = Sys.getenv("is_hpc"))$variables[[rq]]$outcome
  )
}

list(
  targets::tar_target_raw(
    name = paste0(rq, "_dag"),
    command = expression(
      load_dag(
        dags = dags(),
        exposure = exposure,
        outcome = outcome,
        params_dag = params_dag
      )
    )
  ),
  # End dag target
  targets::tar_target_raw(
    name = paste0(rq, "_load_dat"),
    command = substitute(
      rq_load_data(res_dag = dag),
      env = list(dag = as.symbol(paste0(rq, "_dag")))
    )
  ),
  # End load_dat target
  ##############################################################################
  targets::tar_target_raw(
    name = paste0(rq, "_preproc_dat"),
    command = substitute(
      rq_prepare_data(
        dat = dat,
        filter_panel = FALSE,
        type_sample_hcp = NULL,
        is_sa = FALSE
      ),
      env = list(dat = as.symbol(paste0(rq, "_load_dat")))
    )
  ),
  # End preproc_dat target
  ##############################################################################
  targets::tar_target_raw(
    name = paste0(rq, "_weights"),
    command = substitute(
      rq_estimate_weights(
        dat = dat,
        by = NULL,
        include_selection_weights = FALSE,
        save_results = TRUE,
        parallel = FALSE,
        workers = 10
      ),
      env = list(dat = as.symbol(paste0(rq, "_preproc_dat")))
    )
  ),
  # End weights target
  tarchetypes::tar_map(
    values = tbl_outcomes,
    names = "name",
    targets::tar_target_raw(
      name = paste0(rq, "_weighted_fits"),
      command = substitute(
        rq_fit_model_weighted(
          dat = dat,
          outcome = outcome,
          by = NULL,
          is_panel = FALSE,
          weights = weights$estimated_weights,
          parallel = FALSE,
          workers = 10
        ),
        env = list(
          dat = as.symbol(paste0(rq, "_preproc_dat")),
          weights = as.symbol(paste0(rq, "_weights"))
        )
      )
    ),
    # End weighted_fits target
    targets::tar_target_raw(
      name = paste0(rq, "_marginal"),
      command = substitute(
        rq_estimate_marginal_effects(
          fits = all_fits$fits,
          by = NULL,
          is_hcp = FALSE,
          parallel = TRUE,
          workers = 4
        ),
        env = list(all_fits = as.symbol(paste0(rq, "_weighted_fits")))
      )
    ) # End marginal target
  ) # End loop over outcomes
  ##############################################################################
)
