source("DAGs/dag_v2.R")
source("code/dictionaries.R")
source("code/data.R")
source("code/proc_res.R")

targets::tar_option_set(
  format = "qs"
)
path_store <- Sys.getenv("path_store")
num_digits_est = 3
num_digits_sig = 2
sa_var_ <- "e3_sex"

list(
  targets::tar_target(
    name = "table1",
    command = tbl_desc_pop(
      num_digits_est = num_digits_est,
      num_digits_sig = num_digits_sig
    )
  ), # End table1 target
  ##############################################################################
  targets::tar_target(
    name = "desc_chems_mets",
    command = tbl_desc_vars()
  ), # End desc_chems_mets target
  ##############################################################################
  tarchetypes::tar_map(
    list(rq_ = c("1", "2", "3", "1SA", "2SA", "3SA")),
    targets::tar_target(
      name = "balance",
      command = {
        if (!grepl("SA$", rq_)) {
          load_tidy_res_weighting(
            path_store = path_store,
            rq = rq_
          )
        }
      }#########################################################################
    ), # End balance target
    targets::tar_target(
      name = "weights",
      command = load_res_weighted_fits(
        path_store = path_store,
        rq = rq_,
        sa_var = if (grepl("SA$", rq_)) sa_var_ else NULL
      )
    ), # End weights target
    targets::tar_target(
      name = "marginal_comparisons",
      command = load_res_meffects(
        path_store = path_store,
        rq = rq_,
        sa_var = if (grepl("SA$", rq_)) sa_var_ else NULL,
        which_res = "comparisons"
      )
    ), # End marginal_comparisons target
    targets::tar_target(
      name = "marginal_gcomp",
      command = {
        if (!grepl("SA$", rq_)) {
          load_res_meffects(
            path_store = path_store,
            rq = rq_,
            sa_var = if (grepl("SA$", rq_)) sa_var_ else NULL,
            which_res = "gcomp"
          )
        }
      }#########################################################################
    ), # End marginal_comparisons target
    targets::tar_target(
      name = "marginal_hypothesis",
      command = {
        if (grepl("SA$", rq_)) {
          command = load_res_meffects(
            path_store = path_store,
            rq = rq_,
            sa_var = if (grepl("SA$", rq_)) sa_var_ else NULL,
            which_res = "hypothesis"
          )
        }
      }#########################################################################
    ) # End marginal_hypothesis target
  ), # End loop over RQs
  ##############################################################################
  # Supplementary material
  targets::tar_target(
    name = "all_dags",
    command = dags()
  ), # End dags target
  ##############################################################################
  tarchetypes::tar_map(
    list(rq = c("1", "2", "3")),
    targets::tar_target(
      name = "codebook",
      command = tidy_codebooks(rq = rq)
    ) # End codebook target
  ), # End loop over RQs
  ##############################################################################
  tarchetypes::tar_map(
    list(rq = c("1", "2", "3")),
    targets::tar_target(
      name = "overlap_quantiles",
      command = {
        targets::tar_load(
          paste0("rq", rq, "_preproc_dat"),
          store = paste0(path_store, rq)
        )
        viz_overlap_quantiles(
          dat = myphd::extract_cohort(
            dat = get(paste0("rq", rq, "_preproc_dat"))$exposures,
            id_var = "HelixID"
          ),
          id_var = "HelixID",
          group_var = "cohort",
          lower_percentile = 0.1,
          upper_percentile = 0.9,
          rq = rq
        )
      }#########################################################################
    ) # End overlap_quantiles target
  ) # End loop over RQs
  ##############################################################################
)
