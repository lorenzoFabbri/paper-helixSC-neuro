################################################################################
Sys.setenv(is_hpc = TRUE)
# Custom path to _targets for different research questions
path_store <- ifelse(
  Sys.getenv("is_hpc"), 
  paste0(
    "/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/",
    "data/data_paper3/_targets/_targetsRQ"
  ), 
  "~/mounts/rstudioserver/data/data_paper3/_targets/_targetsRQ"
)
Sys.setenv(path_store = path_store)
rq <- "2"
Sys.setenv(TAR_PROJECT = paste0("rq", rq))
store <- paste0(path_store, rq)
################################################################################

################################################################################
source("DAGs/dag_v2.R")
source("code/dictionaries.R")
source("code/data.R")
source("code/utils.R")
source("code/estimate_selection_weights.R")

targets::tar_option_set(
  format = "qs"
)

params_dag <- list(
  type = "minimal",
  effect = "total"
)
rq <- Sys.getenv("TAR_PROJECT")
exposure <- "chemical"
outcome <- "biomarker"
tbl_outcomes <- tibble::tibble(
  name = vars_of_interest()$new_metabolites |>
    stringr::str_to_lower(),
  outcome = vars_of_interest()$new_metabolites
)
################################################################################

################################################################################
dag <- load_dag(
  dags = dags(),
  exposure = exposure,
  outcome = outcome,
  params_dag = params_dag
)
load_dat <- rq_load_data(res_dag = dag)

ret <- lapply(5:8, function(idx) {
  preproc_dat <- rq_prepare_data(
    dat = load_dat,
    filter_panel = TRUE,
    type_sample_hcp = idx
  )
  
  ww <- rq_estimate_weights(
    dat = preproc_dat,
    include_selection_weights = TRUE,
    save_results = FALSE,
    parallel = FALSE,
    workers = 10
  )
  
  res <- lapply(tbl_outcomes$outcome, function(out) {
    fits <- rq_fit_model_weighted(
      dat = preproc_dat,
      outcome = out,
      weights = ww$estimated_weights,
      parallel = FALSE,
      workers = 10
    )
    
    rq_estimate_marginal_effects(
      fits = fits$fits,
      parallel = TRUE,
      workers = 3
    )
  }) # End loop over outcomes
  
  return(res)
}) # End loop over types of sample
################################################################################

names(ret) <- c("nu4mu5", "nu5mu6", "nu6mu7", "week")
ret_proc <- lapply(ret, function(x) {
  names(x) <- tbl_outcomes$name
  x
})

comparisons_by_chemout <- lapply(ret_proc, function(x) {
  tmp <- lapply(x, function(y) {
    lapply(y$marginal_effects, "[[", "comparisons") |>
      dplyr::bind_rows()
  }) |>
    dplyr::bind_rows(.id = "outcome") |>
    tidylog::select(
      outcome, variable, estimate, s.value, conf.low, conf.high
    )
}) |>
  dplyr::bind_rows(.id = "time_period")
to_plot_df <- comparisons_by_chemout |>
  dplyr::group_split(outcome)

plot_over_time <- function(df) {
  info_edcs <- myphd::edcs_information() |>
    tibble::as_tibble() |>
    tidylog::select(
      chem_id, class
    )
  df <- tidylog::full_join(
    df |>
      tidylog::mutate(
        variable = stringr::str_replace(
          variable, "hs_", ""
        ),
        variable = stringr::str_replace(
          variable, "_c", ""
        )
      ),
    info_edcs,
    by = c("variable" = "chem_id")
  ) |>
    tidylog::mutate(
      outcome = as.factor(outcome),
      variable = as.factor(variable),
      class = as.factor(class)
    )
  
  df |>
    ggplot2::ggplot(ggplot2::aes(
      x = time_period, y = estimate,
      group = variable, color = class
    )) +
    ggplot2::scale_x_discrete() +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        size = .data[["s.value"]]
      ),
      position = "identity",
      show.legend = FALSE
    ) +
    ggplot2::geom_line(
      color = "grey"
    ) +
    ggplot2::geom_hline(
      yintercept = 0.0,
      linewidth = 0.3
    ) +
    ggplot2::geom_errorbar(
      mapping = ggplot2::aes(
        ymin = conf.low,
        ymax = conf.high
      ),
      position = "identity",
      width = 0.0,
      linewidth = 0.3
    ) +
    ggplot2::guides(
      size = "none"
    ) +
    ggplot2::labs(
      title = glue::glue(
        "Outcome: {out}.",
        out = stringr::str_replace(
          df$outcome[[1]],
          pattern = "_",
          replacement = " "
        )
      )
    ) +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(hjust = 0),
      text = ggplot2::element_text(
        size = 12
      )
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(variable),
      ncol = 3
    )
}
