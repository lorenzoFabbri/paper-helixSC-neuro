source("DAGs/dag_v2.R")
source("code/dictionaries.R")
source("code/data.R")
source("code/mtps.R")
source("code/research_questions/utils.R")
source("code/research_questions/rq1.R")

library(earth)
library(gam)

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
    command = load_dag(dags = dags(), 
                       exposure = "chemical", 
                       outcome = "intelligence", 
                       params_dag = params_dag)
  ), # End dag target
  ##############################################################################
  targets::tar_target(
    name = load_dat, 
    command = rq_load_data(ids_other_covars = c(), 
                           res_dag = dag)
  ), # End load_dat target
  ##############################################################################
  targets::tar_target(
    name = preproc_dat, 
    command = rq1_prepare_data(dat = load_dat)
  ), # End preproc_dat target
  ##############################################################################
  targets::tar_target(
    name = lmtp, 
    command = run_mtp(dat = preproc_dat, 
                      shift_exposure = TRUE)
  ), # End lmtp target
  ##############################################################################
  targets::tar_target(
    name = lmtp_null, 
    command = run_mtp(dat = preproc_dat, 
                      shift_exposure = FALSE)
  ) # End lmtp_null target
  ##############################################################################
)
