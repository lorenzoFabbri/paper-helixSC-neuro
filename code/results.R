is_hpc = TRUE
path_store <- ifelse(
  is_hpc, 
  "/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/_targets/_targetsRQ", 
  "~/mounts/rstudioserver/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/_targets/_targetsRQ"
)
num_digits_est <- 3
num_digits_sig <- 3
################################################################################

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
################################################################################

sensitivity_analyses <- function(which_sens, fits, rq) {
  # Select type of sensitivity analysis
  if (which_sens == "sensemakr") {
    
  }
  
  return(list(
    
  ))
}
################################################################################

tidy_res_weighted_fits <- function(res_list, rq) {
  names_ <- gsub("weighted_fits_", "", names(res_list))
  
  weights_ <- lapply(1:1, function(idx) {
    outcome <- gsub("weighted_fits_", "", names_[idx])
    tmp <- lapply(res_list[[idx]]$fits, "[[", "weights")
    tmp <- lapply(seq_along(tmp), function(idx2) {
      ret <- tibble::tibble(tmp[[idx2]])
      colnames(ret) <- names(tmp)[idx2]
      return(ret)
    }) |>
      dplyr::bind_cols()
    tmp <- tmp |>
      dplyr::mutate(
        outcome = names_[[idx]]
      ) |>
      dplyr::relocate(outcome)
  }) |> # End loop extract balancing weights
    dplyr::bind_rows()
  weights_wide <- weights_
  
  # Tidy data
  weights_ <- weights_ |>
    dplyr::mutate(
      outcome = gsub("_", " ", outcome)
    ) |>
    tidyr::pivot_longer(
      cols = -c(outcome)
    ) |>
    dplyr::mutate(
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
    dplyr::select(-name)
  
  if (rq == "2") {
    info_edcs <- myphd::edcs_information() |>
      tibble::as_tibble()
    weights_ <- weights_ |>
      dplyr::left_join(
        info_edcs |>
          dplyr::select(chem_id, class),
        by = c("variable" = "chem_id")
      )
  } else {
    weights_ <- weights_ |>
      dplyr::mutate(
        class = c("TMP")
      )
  }
  
  # Plot distribution of weights
  plot <- weights_ |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = value,
        y = forcats::fct_reorder2(
          variable, value, class
        ),
        fill = class
      )
    ) +
    ggplot2::labs(
      x = "weight",
      y = "exposure"
    ) +
    ggdist::stat_halfeye(alpha = 0.6) +
    ggplot2::facet_wrap(
      ggplot2::vars(outcome),
      ncol = length(names_),
      scales = "free"
    ) +
    ggplot2::theme(
      strip.text.x = ggplot2::element_blank()
    )
  
  # Tidy table with numerical values
  colnames(weights_wide) <- gsub(
    "hs_|_c", "", colnames(weights_wide)
  )
  weights_wide <- weights_wide |>
    dplyr::select(-outcome)
  tbl <- c("{median} ({p25}, {p75})", "{min}, {max}") |>
    purrr::map(
      ~ weights_wide |>
        gtsummary::tbl_summary(
          statistic = gtsummary::all_continuous() ~ .x,
          missing = "ifany"
        )
    ) |>
    gtsummary::tbl_merge() |>
    gtsummary::modify_spanning_header(
      gtsummary::everything() ~ NA
    ) |>
    gtsummary::modify_footnote(
      gtsummary::everything() ~ NA
    ) |>
    gtsummary::modify_header(
      list(
        stat_0_1 ~ "Median (IQR)",
        stat_0_2 ~ "Range"
      )
    ) |>
    gtsummary::as_gt() |>
    gt::tab_footnote(
      footnote = "Truncated weights.",
      locations = gt::cells_column_labels()
    )
  
  return(list(
    table = tbl,
    plot = plot
  ))
}
################################################################################

tidy_res_marginaleffects <- function(rq) {
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
        variable = gsub("hs_", "", variable),
        variable = gsub("_c", "", variable)
      ) |>
      dplyr::select(-dplyr::all_of(
        c("contrast", "statistic")
      ))
    
    return(df)
  }) # End loop over results of marginal effects
  names(ret) <- names_
  
  # Tidy
  all_res <- purrr::reduce(ret, dplyr::bind_rows) |>
    dplyr::mutate(
      variable = gsub("hs_", "", variable),
      variable = gsub("_c", "", variable),
      variable = gsub("_", " ", variable),
      outcome = gsub("_", " ", outcome)
    )
  
  if (rq %in% c("1", "2")) {
    info_edcs <- myphd::edcs_information() |>
      tibble::as_tibble()
    df <- all_res |>
      dplyr::left_join(
        info_edcs |>
          dplyr::select(chem_id, class),
        by = c("variable" = "chem_id")
      ) |>
      dplyr::mutate(
        outcome = replace(
          outcome, outcome == "x11bhsd", "11bhsd"
        )
      )
  } else {
    df <- all_res |>
      dplyr::mutate(
        class = c("TMP"),
        variable = replace(
          variable, variable == "x11bhsd", "11bhsd"
        )
      )
  }
  names_ <- unique(df$outcome)
  df <- df |>
    dplyr::mutate(
      dplyr::across(
        c("class", "variable"),
        \(x) {
          x = factor(
            x, levels = sort(unique(x))
          )
        }
      )
    )
  
  # Forest plots side-by-side
  plot <- df |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(x = estimate,
                             y = forcats::fct_reorder2(
                               variable, estimate, class
                             ),
                             color = class)
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        size = .data[["s.value"]]
      ),
      show.legend = FALSE
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
      ncol = length(ret),
      scales = "free"
    ) +
    ggplot2::labs(
      y = "exposure",
      caption = "Size of the dots based on the S value."
    ) +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(hjust = 0)
    )
  
  # Pretty tables w/ numerical results
  df_gt <- df |>
    dplyr::arrange(
      outcome, class, dplyr::desc(variable)
    ) |>
    dplyr::mutate(
      dplyr::across(
        c("estimate", "conf.low", "conf.high"),
        \(x) round(x, digits = num_digits_est)
      ),
      dplyr::across(
        c("p.value", "s.value"),
        \(x) round(x, digits = num_digits_sig)
      ),
      sig = ifelse(
        sign(.data[["conf.low"]] * .data[["conf.high"]]) == 1,
        TRUE,
        FALSE
      ),
      val = glue::glue(
        "{estimate} ({conf.low}, {conf.high}){ifelse(
        sig == TRUE, '*', ''
        )}"
      )
    ) |>
    dplyr::select(
      class, variable, outcome,
      val
    ) |>
    tidyr::pivot_wider(
      names_from = c("outcome"),
      values_from = c("val")
    ) |>
    gt::gt(
      rowname_col = "variable",
      groupname_col = "class"
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = purrr::map(
        names_,
        \(x) {
          gt::cells_body(
            columns = x,
            rows = stringr::str_detect(!! rlang::sym(x), "\\*")
          )
        }
      )
    ) |>
    gt::tab_footnote(
      footnote = "*Significant results."
    )
  
  return(list(
    table = df_gt,
    plot = plot
  ))
}
################################################################################
