#' Title
#'
#' @param res_list 
#' @param rq 
#'
#' @return
#' @export
tidy_res_weighted_fits <- function(res_list, rq) {
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
    tmp <- tmp |>
      tidylog::mutate(
        outcome = names_[[idx]]
      ) |>
      dplyr::relocate(outcome)
  }) |> # End loop extract balancing weights
    dplyr::bind_rows()
  weights_wide <- weights_
  
  # Tidy data
  weights_ <- weights_ |>
    tidylog::mutate(
      outcome = gsub("_", " ", outcome)
    ) |>
    tidyr::pivot_longer(
      cols = -c(outcome)
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
    ggplot2::geom_vline(
      xintercept = 1.0
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(outcome),
      ncol = length(names_),
      scales = "free"
    ) +
    ggplot2::theme(
      strip.text.x = ggplot2::element_blank(),
      text = ggplot2::element_text(
        size = 12
      )
    )
  if (unique(weights_$class) == "TMP") {
    plot <- plot +
      ggplot2::theme(
        legend.position = "null"
      )
  }
  
  # Tidy table with numerical values
  colnames(weights_wide) <- gsub(
    "hs_|_c", "", colnames(weights_wide)
  )
  weights_wide <- weights_wide |>
    tidylog::select(-outcome)
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
} # End function tidy_res_weighted_fits

#' Title
#'
#' @param marginal_effects 
#' @param rq 
#'
#' @return
#' @export
tidy_res_meffects <- function(marginal_effects, rq) {
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
    df <- lapply(x$marginal_effects, "[[", "comparisons") |>
      dplyr::bind_rows() |>
      tidylog::mutate(
        outcome = outcome,
        variable = gsub("hs_", "", variable),
        variable = gsub("_c", "", variable)
      ) |>
      tidylog::select(-dplyr::all_of(
        c("contrast", "statistic")
      ))
    
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
  names_ <- unique(df$outcome)
  df <- df |>
    tidylog::mutate(
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
      ncol = 2,
      scales = "free_x"
    ) +
    ggplot2::labs(
      y = "exposure",
      caption = "Size of the dots based on the S value."
    ) +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(hjust = 0),
      text = ggplot2::element_text(
        size = 12
      )
    )
  
  # Pretty tables w/ numerical results
  df_gt <- df |>
    dplyr::arrange(
      outcome, class, dplyr::desc(variable)
    ) |>
    tidylog::mutate(
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
    tidylog::select(
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
      style = gt::cell_text(
        weight = "bold"
      ),
      locations = gt::cells_row_groups()
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
} # End function tidy_res_meffects

#' Visualize overlap of a variable based on a grouping factor
#'
#' @param dat
#' @param id_var
#' @param group_var
#' @param lower_percentile
#' @param upper_percentile
#'
#' @returns
#' @export
viz_overlap_quantiles <- function(dat,
                                  id_var,
                                  group_var,
                                  lower_percentile,
                                  upper_percentile) {
  # Process data
  dat_proc <- dat |>
    tidylog::select(-dplyr::any_of(id_var))
  vars <- setdiff(colnames(dat_proc), group_var)

  # Plot
  ret <- lapply(vars, function(x) {
    dat_proc |>
      tidylog::select(dplyr::all_of(c(x, group_var))) |>
      ggplot2::ggplot(ggplot2::aes(
        x = .data[[x]],
        y = .data[[group_var]],
        fill = 0.5 - abs(0.5 - stat(ecdf))
      )) +
      ggridges::stat_density_ridges(
        scale = 0.95,
        jittered_points = TRUE,
        position = ggridges::position_points_jitter(
          width = 0.05,
          height = 0
        ),
        point_size = 3,
        point_alpha = 1,
        point_shape = "|",
        geom = "density_ridges_gradient",
        calc_ecdf = TRUE,
        quantile_lines = TRUE,
        vline_color = "red",
        quantiles = c(lower_percentile, upper_percentile)
      ) +
      ggplot2::scale_fill_viridis_c(
        name = "Tail probability",
        direction = -1
      ) +
      ggplot2::labs(
        caption = glue::glue(
          "Lower percentile: {lower}th. Upper percentile: {upper}th.",
          lower = lower_percentile * 100,
          upper = upper_percentile * 100
        )
      )
  }) # End loop over variables to plot

  return(ret)
} # End function viz_overlap_quantiles

#' Visualize the clinical outcome across age and by sex
#'
#' @returns
#' @export
viz_clinical_outcome <- function() {
  # Load data
  dat <- load_dat_request()$dat
  dat <- myphd::extract_cohort(dat, id_var = "HelixID")
  outcome <- vars_of_interest()$outcomes
  dat_sel <- dat |>
    tidylog::select(
      cohort,
      hs_age_years, e3_sex,
      dplyr::all_of(outcome)
    ) |>
    tidylog::mutate(hs_age_years = as.factor(round(hs_age_years, 0))) |>
    tidylog::rename(
      age = hs_age_years,
      sex = e3_sex,
      hrt = {{ outcome }}
    )
  dat_sel$sex <- factor(dat_sel$sex,
    levels = c(0, 1),
    labels = c("male", "female")
  )

  # Create summary
  ret <- ggplot2::ggplot(
    dat_sel,
    ggplot2::aes(
      x = age,
      y = hrt,
      fill = sex
    )) +
    introdataviz::geom_split_violin(
      alpha = 0.4,
      trim = FALSE
    ) +
    ggplot2::geom_boxplot(
      width = 0.2,
      alpha = 0.6,
      fatten = NULL,
      show.legend = FALSE
    ) +
    ggplot2::stat_summary(
      fun.data = "mean_se",
      geom = "pointrange",
      show.legend = FALSE,
      position = ggplot2::position_dodge(0.175)
    ) +
    ggplot2::scale_x_discrete(name = "Age (years)") +
    ggplot2::scale_y_continuous(name = "Hit Reaction Time Standard Error (ms)") +
    ggplot2::scale_fill_brewer(
      palette = "Dark2",
      name = "Sex"
    ) +
    ggplot2::theme_minimal()

  return(ret)
} # End function viz_clinical_outcome

#' Visualize the types of the variables of interest by cohort
#'
#' @param dat
#' @param vars
#' @param fct_levels
#' @param is_chem
#'
#' @returns
#' @export
viz_desc_vars <- function(dat, vars, fct_levels, is_chem) {
  # Select only cohort and variables of interest (e.g., chemicals w/ `cdesc`)
  df <- dat |>
    tidylog::select(
      cohort,
      dplyr::all_of(vars)
    )
  # Pivot to long format and count values
  df_long <- df |>
    tidylog::pivot_longer(cols = -cohort) |>
    tidylog::count(cohort, name, value) |>
    tidylog::mutate(
      name = gsub("^X", "", name)
    )
  df_long$value <- factor(df_long$value,
    levels = fct_levels
  )
  # Pivot to long and compute frequencies
  freqs <- df |>
    tidylog::pivot_longer(cols = -cohort) |>
    tidylog::group_by(cohort, name, value) |>
    tidylog::summarise(n = dplyr::n()) |>
    tidylog::group_by(cohort, name) |>
    tidylog::mutate(f = n / sum(n) * 100) |>
    tidylog::select(-n) |>
    tidylog::ungroup() |>
    tidylog::mutate(
      name = gsub("^X", "", name)
    )

  # Join counts and frequencies
  df_plot <- tidylog::full_join(df_long, freqs,
    by = c("cohort", "name", "value")
  )

  # Heatmap
  plt <- df_plot |>
    dplyr::rowwise() |>
    tidylog::mutate(name = ifelse(
      is_chem == TRUE,
      stringr::str_split(name, "_")[[1]][2],
      stringr::str_split(name, "_")[[1]][1]
    )) |>
    tidylog::rename(
      variable = name,
      description = value,
      frequency = f
    ) |>
    ggplot2::ggplot(ggplot2::aes(variable, description)) +
    ggplot2::geom_tile(ggplot2::aes(fill = frequency)) +
    ggplot2::geom_text(ggplot2::aes(label = round(frequency, 0)),
      color = "black",
      size = 2.5
    ) +
    ggplot2::scale_fill_distiller(palette = "Blues", direction = 1) +
    ggplot2::coord_fixed() +
    ggplot2::facet_grid(cohort ~ .) +
    ggplot2::labs(caption = "1: quantifiable, 2: <LOD, 3: interference or out of range, 4: no sample") +
    ggplot2::theme(
      axis.line = ggplot2::element_line(color = "black"),
      plot.background = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1
      )
    )

  df_plot <- df_plot |>
    dplyr::arrange(
      dplyr::desc(value),
      name,
      dplyr::desc(f)
    )

  return(list(
    dat = df_plot,
    plot = plt
  ))
} # End function viz_desc_vars
################################################################################
