source("DAGs/dag_v2.R")
source("code/dictionaries.R")
source("code/data.R")
source("code/utils.R")
source("code/plot.R")

targets::tar_option_set(
  format = "qs"
)

params_dag <- list(
  type = "minimal", 
  effect = "total"
)
id_var <- "HelixID"
grouping_var <- "cohort"
exposure <- switch(Sys.getenv("TAR_PROJECT"), 
                   "rq01" = "chemical", 
                   "rq02" = "chemical", 
                   "rq03" = "biomarker")
outcome <- switch(Sys.getenv("TAR_PROJECT"), 
                  "rq01" = "outcome", 
                  "rq02" = "biomarker", 
                  "rq03" = "outcome")

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
  targets::tar_target(
    name = viz_desc_chems, 
    command = viz_desc_vars(
      dat = load_dat_request()$dat, 
      vars = vars_of_interest(append_to_chem = "cdesc")$chemicals, 
      fct_levels = c(1, 2, 3, 4), 
      is_chem = TRUE
    )
  ), # End target viz_desc_chems
  targets::tar_target(
    name = viz_desc_metabs, 
    command = viz_desc_vars(
      dat = myphd::extract_cohort(load_steroids()$desc, 
                                  id_var = id_var), 
      vars = vars_of_interest()$metabolites |>
        paste0("_cdesc"), 
      fct_levels = c(1, 2, 3, 4), 
      is_chem = FALSE
    )
  ), # End target viz_desc_metabs
  ##############################################################################
  targets::tar_target(
    name = viz_miss_covars, 
    command = myphd::explore_missings(myphd::extract_cohort(load_dat$covariates, 
                                                            id_var = id_var), 
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
    command = myphd::explore_missings(myphd::extract_cohort(load_dat$exposures, 
                                                            id_var = id_var), 
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
    command = myphd::explore_missings(myphd::extract_cohort(load_dat$outcome, 
                                                            id_var = id_var), 
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
    command = myphd::describe_data(dat = myphd::extract_cohort(load_dat$covariates, 
                                                               id_var = id_var), 
                                   id_var = id_var, 
                                   grouping_var = grouping_var)
  ), # End desc_data_covars target
  targets::tar_target(
    name = desc_data_exp, 
    command = myphd::describe_data(dat = myphd::extract_cohort(load_dat$exposures, 
                                                               id_var = id_var), 
                                   id_var = id_var, 
                                   grouping_var = grouping_var)
  ), # End desc_data_exp target
  targets::tar_target(
    name = desc_data_out, 
    command = myphd::describe_data(dat = myphd::extract_cohort(load_dat$outcome, 
                                                               id_var = id_var), 
                                   id_var = id_var, 
                                   grouping_var = grouping_var)
  ), # End desc_data_out target
  ##############################################################################
  targets::tar_target(
    name = preproc_dat, 
    command = rq_prepare_data(dat = load_dat)
  ), # End preproc_dat target
  ##############################################################################
  targets::tar_target(
    name = desc_data_covars_proc, 
    command = myphd::describe_data(dat = myphd::extract_cohort(preproc_dat$covariates, 
                                                               id_var = id_var), 
                                   id_var = id_var, 
                                   grouping_var = grouping_var)
  ), # End desc_data_covars_proc target
  targets::tar_target(
    name = desc_data_exp_proc, 
    command = myphd::describe_data(dat = myphd::extract_cohort(preproc_dat$exposures, 
                                                               id_var = id_var), 
                                   id_var = id_var, 
                                   grouping_var = grouping_var)
  ), # End desc_data_exp_proc target
  targets::tar_target(
    name = desc_data_out_proc, 
    command = myphd::describe_data(dat = myphd::extract_cohort(preproc_dat$outcome, 
                                                               id_var = id_var), 
                                   id_var = id_var, 
                                   grouping_var = grouping_var)
  ) # End desc_data_out_proc target
)
