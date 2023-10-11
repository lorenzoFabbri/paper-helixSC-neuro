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
exposure <- switch(rq, 
                   "rq1" = "chemical", 
                   "rq2" = "chemical", 
                   "rq3" = "biomarker")
outcome <- switch(rq, 
                  "rq1" = "outcome", 
                  "rq2" = "biomarker", 
                  "rq3" = "outcome")

if (rq == "rq1") {
  tbl_outcomes <- tibble::tibble(
    name = params(is_hpc = Sys.getenv("is_hpc"))$variables[[rq]]$outcome |>
      stringr::str_to_lower(), 
    outcome = params(is_hpc = Sys.getenv("is_hpc"))$variables[[rq]]$outcome
  )
} else if (rq == "rq2") {
  tbl_outcomes <- tibble::tibble(
    name = c("cortisol_production",
             "cortisol_metabolism",
             "cortisone_production",
             "X11bHSD") |>
      stringr::str_to_lower(),
    outcome = c("cortisol_production",
                "cortisol_metabolism",
                "cortisone_production",
                "X11bHSD")
  )
} else if (rq == "rq3") {
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
      rq_prepare_data(dat = dat),
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
        save_results = FALSE,
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
          fits = fits$fits,
          parallel = TRUE,
          workers = 6
        ),
        env = list(fits = as.symbol(paste0(rq, "_weighted_fits")))
      )
    ) # End marginal target
  ) # End loop over outcomes
  ##############################################################################
)
