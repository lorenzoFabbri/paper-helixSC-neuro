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
sa_var <- "e3_sex"

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
    list(rq = c("1", "2", "3", "1SA", "2SA", "3SA")),
    targets::tar_target(
      name = "weights",
      command = {
        targets::tar_load(
          dplyr::contains(paste0("rq", substr(rq, 1, 1), "_weighted_fits_")),
          store = paste0(path_store, rq)
        )
        tidy_res_weighted_fits(
          res_list = mget(ls(
            pattern = paste0("rq", substr(rq, 1, 1), "_weighted_fits_*")
          )),
          rq = substr(rq, 1, 1),
          sa_var = if (grepl("SA$", rq)) sa_var else NULL
        )
      }
    ), # End weights target
    targets::tar_target(
      name = "marginal_comparisons",
      command = {
        targets::tar_load(
          dplyr::contains(paste0("rq", substr(rq, 1, 1), "_marginal_")),
          store = paste0(path_store, rq)
        )
        tidy_res_meffects(
          marginal_effects = mget(ls(
            pattern = paste0("rq", substr(rq, 1, 1), "_marginal_*")
          )),
          rq = substr(rq, 1, 1),
          sa_var = if (grepl("SA$", rq)) sa_var else NULL,
          which_res = "comparisons",
          num_digits_est = num_digits_est,
          num_digits_sig = num_digits_sig
        )
      }
    ), # End marginal_comparisons target
    targets::tar_target(
      name = "marginal_hypothesis",
      command = {
        if (grepl("SA$", rq)) {
          targets::tar_load(
            dplyr::contains(paste0("rq", substr(rq, 1, 1), "_marginal_")),
            store = paste0(path_store, rq)
          )
          tidy_res_meffects(
            marginal_effects = mget(ls(
              pattern = paste0("rq", substr(rq, 1, 1), "_marginal_*")
            )),
            rq = substr(rq, 1, 1),
            sa_var = sa_var,
            which_res = "hypothesis",
            num_digits_est = num_digits_est,
            num_digits_sig = num_digits_sig
          )
        }
      }
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
  targets::tar_target(
    name = "viz_desc_chems_metabs",
    command = {
      targets::tar_load(
        dplyr::contains("rq01_viz_desc_"),
        store = paste0(path_store, "01")
      )
      lapply(
        paste0("rq01", c("_viz_desc_chems", "_viz_desc_metabs")),
        function(x) {
          get(x)$plot +
            ggplot2::theme(
              legend.position = "bottom",
              text = ggplot2::element_text(
                size = 8
              )
            )
        }
      )
    }
  ) # End viz_desc_chems_metabs target
  ##############################################################################
)
