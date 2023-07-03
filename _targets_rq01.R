source("DAGs/dag_v2.R")
source("code/dictionaries.R")
source("code/data.R")
source("code/research_questions/rq1.R")
source("code/research_questions/utils.R")

targets::tar_option_set(
  format = "qs"
)
options(clustermq.scheduler = "multicore")

params_dag <- list(
  type = "minimal", 
  effect = "total"
)
id_var <- "HelixID"
grouping_var <- "cohort"

list(
  targets::tar_target(
    name = dag, 
    command = load_dag(dags = dags(), 
                       exposure = "chemical", 
                       outcome = "intelligence", 
                       params_dag = params_dag)
  ), # End dag target
  targets::tar_target(
    name = load_dat, 
    command = rq_load_data(ids_other_covars = c(), 
                           res_dag = dag)
  ), # End load_dat target
  ##############################################################################
  targets::tar_target(
    name = viz_miss_covars, 
    command = myphd::explore_missings(load_dat$covariates, 
                                      id_var = id_var, 
                                      grouping_var = grouping_var, 
                                      path_save = paste0(
                                        "results/figures/", 
                                        Sys.getenv("TAR_PROJECT"), 
                                        "/covars_"
                                      ))
  ), # End viz_miss_covars target
  targets::tar_target(
    name = viz_miss_exp, 
    command = myphd::explore_missings(load_dat$exposures, 
                                      id_var = id_var, 
                                      grouping_var = grouping_var, 
                                      path_save = paste0(
                                        "results/figures/", 
                                        Sys.getenv("TAR_PROJECT"), 
                                        "/exp_"
                                      ))
  ), # End viz_miss_exp target
  targets::tar_target(
    name = viz_miss_out, 
    command = myphd::explore_missings(load_dat$outcome, 
                                      id_var = id_var, 
                                      grouping_var = grouping_var, 
                                      path_save = paste0(
                                        "results/figures/", 
                                        Sys.getenv("TAR_PROJECT"), 
                                        "/out_"
                                      ))
  ), # End viz_miss_out target
  ##############################################################################
  targets::tar_target(
    name = desc_data_covars, 
    command = myphd::describe_data(dat = load_dat$covariates, 
                                   id_var = id_var, 
                                   grouping_var = grouping_var)
  ), # End desc_data_covars target
  targets::tar_target(
    name = desc_data_exp, 
    command = myphd::describe_data(dat = load_dat$exposures, 
                                   id_var = id_var, 
                                   grouping_var = grouping_var)
  ), # End desc_data_exp target
  targets::tar_target(
    name = desc_data_out, 
    command = myphd::describe_data(dat = load_dat$outcome, 
                                   id_var = id_var, 
                                   grouping_var = grouping_var)
  ), # End desc_data_out target
  ##############################################################################
  targets::tar_target(
    name = preproc_dat, 
    command = rq1_prepare_data(dat = load_dat)
  ), # End preproc_dat target
  ##############################################################################
  targets::tar_target(
    name = desc_data_covars_proc, 
    command = myphd::describe_data(dat = preproc_dat$covariates, 
                                   id_var = id_var, 
                                   grouping_var = grouping_var)
  ), # End desc_data_covars_proc target
  targets::tar_target(
    name = desc_data_exp_proc, 
    command = myphd::describe_data(dat = preproc_dat$exposures, 
                                   id_var = id_var, 
                                   grouping_var = grouping_var)
  ), # End desc_data_exp_proc target
  targets::tar_target(
    name = desc_data_out_proc, 
    command = myphd::describe_data(dat = preproc_dat$outcome, 
                                   id_var = id_var, 
                                   grouping_var = grouping_var)
  ), # End desc_data_out_proc target
  ##############################################################################
  targets::tar_target(
    name = weights, 
    command = rq_estimate_weights(dat = preproc_dat)
  ) # End weights target
)
