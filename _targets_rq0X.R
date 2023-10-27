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
by_var <- "cohort"
rq <- Sys.getenv("TAR_PROJECT")
exposure <- switch(rq,
  "rq01" = "chemical",
  "rq02" = "chemical",
  "rq03" = "biomarker"
)
outcome <- switch(rq,
  "rq01" = "outcome",
  "rq02" = "biomarker",
  "rq03" = "outcome"
)

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
      rq_load_data(res_dag = dag, remove_kanc = FALSE),
      env = list(dag = as.symbol(paste0(rq, "_dag")))
    )
  ),
  # End load_dat target
  targets::tar_target_raw(
    name = paste0(rq, "_viz_desc_chems"),
    command = expression(
      viz_desc_vars(
        dat = load_dat_request()$dat,
        vars = vars_of_interest(append_to_chem = "cdesc")$chemicals,
        fct_levels = c(1, 2, 3, 4),
        is_chem = TRUE
      )
    )
  ), # End target viz_desc_chems
  targets::tar_target_raw(
    name = paste0(rq, "_viz_desc_metabs"),
    command = expression(
      viz_desc_vars(
        dat = myphd::extract_cohort(load_steroids()$desc,
          id_var = id_var
        ) |>
          tidylog::mutate(
            cohort = dplyr::case_when(
              cohort == "EDE" ~ "EDEN",
              cohort == "KAN" ~ "KANC",
              cohort == "MOB" ~ "MOBA",
              cohort == "RHE" ~ "RHEA",
              .default = cohort
            )
          ),
        vars = vars_of_interest()$metabolites |>
          paste0("_cdesc"),
        fct_levels = c(1, 2, 3, 4),
        is_chem = FALSE
      )
    )
  ), # End target viz_desc_metabs
  ##############################################################################
  targets::tar_target_raw(
    name = paste0(rq, "_viz_miss_covars"),
    command = substitute(
      myphd::explore_missings(
        myphd::extract_cohort(dat$covariates,
          id_var = id_var
        ),
        id_var = id_var,
        by_var = by_var,
        path_save = paste0(
          "results/figures/",
          rq,
          "/covars_"
        )
      ),
      env = list(
        dat = as.symbol(paste0(rq, "_load_dat"))
      )
    )
  ), # End viz_miss_covars target
  targets::tar_target_raw(
    name = paste0(rq, "_viz_miss_exp"),
    command = substitute(
      myphd::explore_missings(
        myphd::extract_cohort(dat$exposures,
          id_var = id_var
        ),
        id_var = id_var,
        by_var = by_var,
        path_save = paste0(
          "results/figures/",
          rq,
          "/exp_"
        )
      ),
      env = list(
        dat = as.symbol(paste0(rq, "_load_dat"))
      )
    )
  ), # End viz_miss_exp target
  targets::tar_target_raw(
    name = paste0(rq, "_viz_miss_out"),
    command = substitute(
      myphd::explore_missings(
        myphd::extract_cohort(dat$outcome,
          id_var = id_var
        ),
        id_var = id_var,
        by_var = by_var,
        path_save = paste0(
          "results/figures/",
          rq,
          "/out_"
        )
      ),
      env = list(
        dat = as.symbol(paste0(rq, "_load_dat"))
      )
    )
  ), # End viz_miss_out target
  ##############################################################################
  targets::tar_target_raw(
    name = paste0(rq, "_desc_data_covars"),
    command = substitute(
      myphd::describe_data(
        dat = myphd::extract_cohort(dat$covariates,
          id_var = id_var
        ),
        id_var = id_var,
        by_var = by_var
      ),
      env = list(
        dat = as.symbol(paste0(rq, "_load_dat"))
      )
    )
  ), # End desc_data_covars target
  targets::tar_target_raw(
    name = paste0(rq, "_desc_data_exp"),
    command = substitute(
      myphd::describe_data(
        dat = myphd::extract_cohort(dat$exposures,
          id_var = id_var
        ) |>
          tidylog::mutate(
            cohort = dplyr::case_when(
              cohort == "EDE" ~ "EDEN",
              cohort == "KAN" ~ "KANC",
              cohort == "MOB" ~ "MOBA",
              cohort == "RHE" ~ "RHEA",
              .default = cohort
            )
          ) |>
          tidylog::rename_with(
            .fn = \(x) gsub("hs_|_c", "", x)
          ),
        id_var = id_var,
        by_var = by_var
      ),
      env = list(
        dat = as.symbol(paste0(rq, "_load_dat"))
      )
    )
  ), # End desc_data_exp target
  targets::tar_target_raw(
    name = paste0(rq, "_desc_data_out"),
    command = substitute(
      myphd::describe_data(
        dat = myphd::extract_cohort(dat$outcome,
          id_var = id_var
        ) |>
          tidylog::mutate(
            cohort = dplyr::case_when(
              cohort == "EDE" ~ "EDEN",
              cohort == "KAN" ~ "KANC",
              cohort == "MOB" ~ "MOBA",
              cohort == "RHE" ~ "RHEA",
              .default = cohort
            )
          ) |>
          tidylog::rename_with(
            .fn = \(x) gsub("^X", "", x)
          ),
        id_var = id_var,
        by_var = by_var
      ),
      env = list(
        dat = as.symbol(paste0(rq, "_load_dat"))
      )
    )
  ), # End desc_data_out target
  ##############################################################################
  targets::tar_target_raw(
    name = paste0(rq, "_preproc_dat"),
    command = substitute(
      rq_prepare_data(dat = dat),
      env = list(
        dat = as.symbol(paste0(rq, "_load_dat"))
      )
    )
  ), # End preproc_dat target
  ##############################################################################
  targets::tar_target_raw(
    name = paste0(rq, "_desc_data_covars_proc"),
    command = substitute(
      myphd::describe_data(
        dat = myphd::extract_cohort(dat$covariates,
          id_var = id_var
        ),
        id_var = id_var,
        by_var = by_var
      ),
      env = list(
        dat = as.symbol(paste0(rq, "_preproc_dat"))
      )
    )
  ), # End desc_data_covars_proc target
  targets::tar_target_raw(
    name = paste0(rq, "_desc_data_exp_proc"),
    command = substitute(
      myphd::describe_data(
        dat = myphd::extract_cohort(dat$exposures,
          id_var = id_var
        ),
        id_var = id_var,
        by_var = by_var
      ),
      env = list(
        dat = as.symbol(paste0(rq, "_preproc_dat"))
      )
    )
  ), # End desc_data_exp_proc target
  targets::tar_target_raw(
    name = paste0(rq, "_desc_data_out_proc"),
    command = substitute(
      myphd::describe_data(
        dat = myphd::extract_cohort(dat$outcome,
          id_var = id_var
        ),
        id_var = id_var,
        by_var = by_var
      ),
      env = list(
        dat = as.symbol(paste0(rq, "_preproc_dat"))
      )
    )
  ) # End desc_data_out_proc target
)
