is_hpc = TRUE
path_store <- ifelse(
  is_hpc, 
  "/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/_targets/_targetsRQ", 
  "~/mounts/rstudioserver/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/_targets/_targetsRQ"
)

for (rq in c("2", "3")) {
  # Exploratory
  targets::tar_load_everything(
    store = paste0(path_store, paste0("0", rq))
  )
  
  # Analyses
  targets::tar_load_everything(
    store = paste0(path_store, rq)
  )
}

table_res_marginaleffects <- function(rq) {
  names_ <- gsub("marginal_", "",
                 ls(pattern = "marginal_*"))
  marginal_effects <- mget(
    ls(pattern = "marginal_*")
  )
  
  ret <- lapply(seq_along(marginal_effects), function(idx) {
    outcome <- gsub("marginal_", "",
                    names(marginal_effects[idx]))
    
    # Table for one outcome and all exposures
    x <- marginal_effects[[idx]]
    df <- lapply(x$marginal_effects, "[[", "comparisons") |>
      dplyr::bind_rows() |>
      dplyr::mutate(
        outcome = outcome,
        dplyr::across(
          c("estimate", "conf.low", "conf.high",
            "s.value"),
          \(x) round(x, digits = 3)
        )
      ) |>
      dplyr::select(-dplyr::all_of(
        c("contrast", "statistic")
      ))
    
    if (rq == "rq2") {
      df <- df |>
        dplyr::mutate(
          variable = gsub("hs_", "", variable),
          variable = gsub("_c", "", variable)
        )
    }
    
    return(df)
  }) # End loop over results of marginal effects
  names(ret) <- names_
  
  # Forest plots side-by-side
  all_res <- purrr::reduce(ret, dplyr::bind_rows)
  plot <- all_res |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(x = estimate,
                             y = variable)
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        size = .data[["s.value"]]
      )
    ) +
    ggplot2::geom_errorbar(
      mapping = ggplot2::aes(
        xmin = conf.low,
        xmax = conf.high
      ),
      width = 0.0,
      linewidth = 0.3
    ) +
    ggplot2::geom_vline(
      xintercept = 0.0
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(outcome),
      ncol = length(ret)
    )
  
  return(list(
    df = all_res,
    plot = plot
  ))
}
