source("code/dictionaries.R")
source("code/data.R")
source("DAGs/dag_v2.R")
source("code/research_questions/rq1.R")

targets::tar_option_set(
  format = "qs"
)
options(clustermq.scheduler = "multicore")

params_dag <- list(
  type = "minimal", 
  effect = "total"
)

list(
  targets::tar_target(
    name = dag, 
    command = rq1_dag(dags = dags(), 
                      exposure = "chemical", 
                      outcome = "intelligence", 
                      params_dag = params_dag)
  ), # End dag target
  targets::tar_target(
    name = load_dat, 
    command = rq1_load_data(params = params(), 
                            which_expo = "path_exposures_post_final", 
                            which_covars = c(), 
                            which_outcome = "")
  ) # End load_dat target
)
