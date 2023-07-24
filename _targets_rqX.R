source("DAGs/dag_v2.R")
source("code/dictionaries.R")
source("code/data.R")
source("code/mtps.R")
source("code/utils.R")

targets::tar_option_set(
  format = "qs"
)

params_dag <- list(
  type = "minimal", 
  effect = "total"
)
exposure <- switch(Sys.getenv("TAR_PROJECT"), 
                   "rq1" = "chemical", 
                   "rq2" = "chemical", 
                   "rq3" = "biomarker")
outcome <- switch(Sys.getenv("TAR_PROJECT"), 
                  "rq1" = "intelligence", 
                  "rq2" = "biomarker", 
                  "rq3" = "intelligence")

list(
  targets::tar_target(
    name = dag, 
    command = load_dag(dags = dags(), 
                       exposure = exposure, 
                       outcome = outcome, 
                       params_dag = params_dag)
  ), # End dag target
  targets::tar_target(
    name = load_dat, 
    command = rq_load_data(res_dag = dag)
  ), # End load_dat target
  ##############################################################################
  targets::tar_target(
    name = preproc_dat, 
    command = rq_prepare_data(dat = load_dat)
  ), # End preproc_dat target
  ##############################################################################
  targets::tar_target(
    name = weights, 
    command = rq_estimate_weights(dat = preproc_dat, 
                                  save_results = FALSE, 
                                  parallel = FALSE)
  ), # End weights target
  targets::tar_target(
    name = weighted_fits, 
    command = rq_fit_model_weighted(dat = preproc_dat, 
                                    weights = weights$estimated_weights, 
                                    parallel = FALSE)
  ), # End weighted_fits target
  targets::tar_target(
    name = marginal, 
    command = rq_estimate_marginal_effects(fits = weighted_fits$fits, 
                                           parallel = TRUE, 
                                           workers = 6)
  ) # End marginal target
  ##############################################################################
)
