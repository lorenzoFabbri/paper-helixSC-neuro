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
      name = "weights",
      command = {
        targets::tar_load(
          dplyr::contains(paste0("rq", substr(rq_, 1, 1), "_weighted_fits_")),
          store = paste0(path_store, rq_)
        )
        res_list <- mget(ls(
          pattern = paste0("rq", substr(rq_, 1, 1), "_weighted_fits_*")
        ))
        rq <- substr(rq_, 1, 1)
        sa_var <- if (grepl("SA$", rq_)) sa_var_ else NULL
        names_ <- gsub(paste0("rq", rq, "_weighted_fits_"),
                       "",
                       names(res_list))
        weights_ <- lapply(1:1, function(idx) {
          outcome <- gsub(paste0("rq", rq, "_weighted_fits_"),
                          "",
                          names_[idx])
          tmp <- lapply(res_list[[idx]]$fits, "[[", "weights")
          tmp <- lapply(seq_along(tmp), function(idx2) {
            ret <- tibble::tibble(tmp[[idx2]])
            colnames(ret) <- names(tmp)[idx2]
            return(ret)
          }) |>
            dplyr::bind_cols()
          # Check whether is SA and eventually add effect modifier
          if (!is.null(sa_var)) {
            tmp <- tmp |>
              tidylog::mutate(
                modifier = res_list[[1]]$fits[[1]]$dat[[sa_var]]
              ) |>
              dplyr::relocate(modifier)
          }
          tmp <- tmp |>
            tidylog::mutate(
              outcome = names_[[idx]]
            ) |>
            dplyr::relocate(outcome)
        }) |> # End loop extract balancing weights
          dplyr::bind_rows()
        if (!is.null(sa_var)) {
          if (sa_var == "e3_sex") {
            weights_ <- weights_ |>
              tidylog::mutate(
                modifier = dplyr::case_when(
                  modifier == 0 ~ "males",
                  modifier == 1 ~ "females",
                  .default = modifier
                )
              )
          } # End if for e3_sex
        }
        weights_wide <- weights_
        # Tidy data
        weights_ <- weights_ |>
          tidylog::mutate(
            outcome = gsub("_", " ", outcome)
          ) |>
          tidyr::pivot_longer(
            cols = -dplyr::any_of(c("outcome", "modifier"))
          ) |>
          tidylog::mutate(
            variable = name,
            outcome = replace(
              outcome, outcome == "x11bhsd", "11bhsd"
            ),
            variable = replace(
              variable, variable == "x11bhsd", "11bhsd"
            ),
            variable = gsub("hs_", "", variable),
            variable = gsub("_c", "", variable),
            variable = gsub("_", " ", variable),
            outcome = gsub("_", " ", outcome)
          ) |>
          tidylog::select(-name)
        if (rq %in% c("1", "2")) {
          info_edcs <- myphd::edcs_information() |>
            tibble::as_tibble()
          weights_ <- weights_ |>
            tidylog::left_join(
              info_edcs |>
                tidylog::select(chem_id, class),
              by = c("variable" = "chem_id")
            )
        } else {
          weights_ <- weights_ |>
            tidylog::mutate(
              class = c("TMP")
            )
        }
        
        list(
          dat_tbl = weights_wide,
          dat_plt = weights_
        )
      }#########################################################################
    ), # End weights target
    targets::tar_target(
      name = "marginal_comparisons",
      command = {
        targets::tar_load(
          dplyr::contains(paste0("rq", substr(rq_, 1, 1), "_marginal_")),
          store = paste0(path_store, rq_)
        )
        marginal_effects <- mget(ls(
          pattern = paste0("rq", substr(rq_, 1, 1), "_marginal_*")
        ))
        rq <- substr(rq_, 1, 1)
        sa_var <- if (grepl("SA$", rq_)) sa_var_ else NULL
        which_res <- "comparisons"
        names_ <- gsub(paste0("rq", rq, "_marginal_"),
                       "",
                       names(marginal_effects))
        ret <- lapply(seq_along(marginal_effects), function(idx) {
          outcome <- gsub(paste0("rq", rq, "_marginal_"),
                          "",
                          names(marginal_effects[idx]))
          if (outcome != "x11bhsd") {
            outcome <- gsub("hs", "", outcome)
          } else {
            outcome <- "11bhsd"
          }
          # Table for one outcome and all exposures
          x <- marginal_effects[[idx]]
          if (length(x$marginal_effects) == 0) return(NULL)
          df <- lapply(x$marginal_effects, "[[", which_res) |>
            dplyr::bind_rows() |>
            tidylog::mutate(
              outcome = outcome,
              variable = gsub("hs_", "", variable),
              variable = gsub("_c", "", variable)
            ) |>
            tidylog::select(-dplyr::any_of(
              c("contrast", "statistic")
            ))
          if (which_res == "hypothesis") {
            df$variable <- names(x$marginal_effects)
          }
          return(df)
        }) # End loop over results of marginal effects
        names(ret) <- names_
        # Tidy
        all_res <- purrr::reduce(ret, dplyr::bind_rows) |>
          tidylog::mutate(
            variable = gsub("hs_", "", variable),
            variable = gsub("_c", "", variable),
            variable = gsub("_", " ", variable),
            outcome = gsub("_", " ", outcome)
          )
        if (!is.null(sa_var) & which_res != "hypothesis") {
          all_res <- all_res |>
            tidylog::rename_with(
              ~ c("modifier"), dplyr::all_of(c(sa_var))
            )
        }
        if (rq %in% c("1", "2")) {
          info_edcs <- myphd::edcs_information() |>
            tibble::as_tibble()
          df <- all_res |>
            tidylog::left_join(
              info_edcs |>
                tidylog::select(chem_id, class),
              by = c("variable" = "chem_id")
            ) |>
            tidylog::mutate(
              outcome = replace(
                outcome, outcome == "x11bhsd", "11bhsd"
              )
            )
        } else {
          df <- all_res |>
            tidylog::mutate(
              class = c("TMP"),
              variable = replace(
                variable, variable == "x11bhsd", "11bhsd"
              )
            )
        }
        df <- df |>
          tidylog::mutate(
            dplyr::across(
              c("class", "variable"),
              \(x) {
                x = factor(
                  x, levels = sort(unique(x))
                )
              }
            ),
            dplyr::across(
              dplyr::where(is.character),
              \(x) stringr::str_trim(x, side = "both")
            )
          )
        names_ <- unique(df$outcome)
        if (!is.null(sa_var) & which_res != "hypothesis") {
          if (sa_var == "e3_sex") {
            df <- df |>
              tidylog::mutate(
                modifier = dplyr::case_when(
                  modifier == 0 ~ "males",
                  modifier == 1 ~ "females",
                  .default = modifier
                )
              )
          } # End if for e3_sex
        }
        
        return(df)
      }#########################################################################
    ), # End marginal_comparisons target
    targets::tar_target(
      name = "marginal_hypothesis",
      command = {
        if (grepl("SA$", rq_)) {
          targets::tar_load(
            dplyr::contains(paste0("rq", substr(rq_, 1, 1), "_marginal_")),
            store = paste0(path_store, rq_)
          )
          marginal_effects <- mget(ls(
            pattern = paste0("rq", substr(rq_, 1, 1), "_marginal_*")
          ))
          rq <- substr(rq_, 1, 1)
          sa_var <- if (grepl("SA$", rq_)) sa_var_ else NULL
          which_res <- "hypothesis"
          names_ <- gsub(paste0("rq", rq, "_marginal_"),
                         "",
                         names(marginal_effects))
          ret <- lapply(seq_along(marginal_effects), function(idx) {
            outcome <- gsub(paste0("rq", rq, "_marginal_"),
                            "",
                            names(marginal_effects[idx]))
            if (outcome != "x11bhsd") {
              outcome <- gsub("hs", "", outcome)
            } else {
              outcome <- "11bhsd"
            }
            # Table for one outcome and all exposures
            x <- marginal_effects[[idx]]
            if (length(x$marginal_effects) == 0) return(NULL)
            df <- lapply(x$marginal_effects, "[[", which_res) |>
              dplyr::bind_rows() |>
              tidylog::mutate(
                outcome = outcome,
                variable = gsub("hs_", "", variable),
                variable = gsub("_c", "", variable)
              ) |>
              tidylog::select(-dplyr::any_of(
                c("contrast", "statistic")
              ))
            if (which_res == "hypothesis") {
              df$variable <- names(x$marginal_effects)
            }
            return(df)
          }) # End loop over results of marginal effects
          names(ret) <- names_
          # Tidy
          all_res <- purrr::reduce(ret, dplyr::bind_rows) |>
            tidylog::mutate(
              variable = gsub("hs_", "", variable),
              variable = gsub("_c", "", variable),
              variable = gsub("_", " ", variable),
              outcome = gsub("_", " ", outcome)
            )
          if (!is.null(sa_var) & which_res != "hypothesis") {
            all_res <- all_res |>
              tidylog::rename_with(
                ~ c("modifier"), dplyr::all_of(c(sa_var))
              )
          }
          if (rq %in% c("1", "2")) {
            info_edcs <- myphd::edcs_information() |>
              tibble::as_tibble()
            df <- all_res |>
              tidylog::left_join(
                info_edcs |>
                  tidylog::select(chem_id, class),
                by = c("variable" = "chem_id")
              ) |>
              tidylog::mutate(
                outcome = replace(
                  outcome, outcome == "x11bhsd", "11bhsd"
                )
              )
          } else {
            df <- all_res |>
              tidylog::mutate(
                class = c("TMP"),
                variable = replace(
                  variable, variable == "x11bhsd", "11bhsd"
                )
              )
          }
          df <- df |>
            tidylog::mutate(
              dplyr::across(
                c("class", "variable"),
                \(x) {
                  x = factor(
                    x, levels = sort(unique(x))
                  )
                }
              ),
              dplyr::across(
                dplyr::where(is.character),
                \(x) stringr::str_trim(x, side = "both")
              )
            )
          names_ <- unique(df$outcome)
          if (!is.null(sa_var) & which_res != "hypothesis") {
            if (sa_var == "e3_sex") {
              df <- df |>
                tidylog::mutate(
                  modifier = dplyr::case_when(
                    modifier == 0 ~ "males",
                    modifier == 1 ~ "females",
                    .default = modifier
                  )
                )
            } # End if for e3_sex
          }
          
          return(df)
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
  ) # End loop over RQs
  ##############################################################################
)
