#' Title
#'
#' @param dat 
#' @param id_var 
#' @param group_var 
#' @param lower_percentile 
#' @param upper_percentile 
#'
#' @return
#' @export
viz_overlap_quantiles <- function(dat, id_var, group_var, 
                                  lower_percentile, upper_percentile) {
  # Process data
  dat_proc <- dat |>
    dplyr::select(-dplyr::any_of(id_var))
  vars <- setdiff(colnames(dat_proc), group_var)
  
  # Plot
  ret <- lapply(vars, function(x) {
    dat_proc |>
      dplyr::select(dplyr::all_of(c(x, group_var))) |>
      ggplot2::ggplot(ggplot2::aes(x = .data[[x]], 
                                   y = .data[[group_var]], 
                                   fill = 0.5 - abs(0.5 - stat(ecdf)))) +
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
      ggplot2::scale_fill_viridis_c(name = "Tail probability", 
                                    direction = -1) +
      ggplot2::labs(caption = glue::glue(
                      "Lower percentile: {lower}th. Upper percentile: {upper}th.", 
                      lower = lower_percentile * 100, 
                      upper = upper_percentile * 100
                    ))
  }) # End loop over variables to plot
  
  return(ret)
} # End function viz_overlap_quantiles

#' Title
#'
#' @return
#' @export
viz_clinical_outcome <- function() {
  # Load data
  dat <- load_dat_request()$dat
  dat <- myphd::extract_cohort(dat, id_var = "HelixID")
  outcome <- vars_of_interest()$outcomes
  dat_sel <- dat |>
    dplyr::select(
      cohort, 
      hs_age_years, e3_sex, 
      dplyr::all_of(outcome)
    ) |>
    dplyr::mutate(
      hs_age_years = as.factor(round(hs_age_years, 0))
    ) |>
    dplyr::rename(
      age = hs_age_years, 
      sex = e3_sex, 
      hrt = {{outcome}}
    )
  dat_sel$sex <- factor(dat_sel$sex, 
                        levels = c(0, 1), 
                        labels = c("male", "female"))
  
  # Create summary
  ret <- dat_sel |>
    ggplot2::ggplot(ggplot2::aes(x = age, 
                                 y = hrt, 
                                 fill = sex)) +
    introdataviz::geom_split_violin(alpha = 0.4, 
                                    trim = FALSE) +
    ggplot2::geom_boxplot(width = 0.2, 
                          alpha = 0.6, 
                          fatten = NULL, 
                          show.legend = FALSE) +
    ggplot2::stat_summary(fun.data = "mean_se", 
                          geom = "pointrange", 
                          show.legend = FALSE, 
                          position = ggplot2::position_dodge(0.175)) +
    ggplot2::scale_x_discrete(name = "Age (years)") +
    ggplot2::scale_y_continuous(name = "Hit Reaction Time Standard Error (ms)") +
    ggplot2::scale_fill_brewer(palette = "Dark2", 
                               name = "Sex") +
    ggplot2::theme_minimal()
  
  return(ret)
} # End function viz_clinical_outcome

#' Title
#'
#' @param dat 
#' @param vars 
#' @param fct_levels 
#'
#' @return
#' @export
viz_desc_vars <- function(dat, vars, fct_levels) {
  # Select only cohort and variables of interest (e.g., chemicals w/ `cdesc`)
  df <- dat |>
    dplyr::select(cohort, 
                  dplyr::all_of(vars))
  # Pivot to long format and count values
  df_long <- df |>
    tidyr::pivot_longer(cols = -cohort) |>
    dplyr::count(cohort, name, value)
  df_long$value <- factor(df_long$value, 
                          levels = fct_levels)
  # Pivot to long and compute frequencies
  freqs <- df |>
    tidyr::pivot_longer(cols = -cohort) |>
    dplyr::group_by(cohort, name, value) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::group_by(cohort, name) |>
    dplyr::mutate(f = n / sum(n) * 100) |>
    dplyr::select(-n)
  
  # Join counts and frequencies
  df_plot <- dplyr::full_join(
    df_long, freqs, 
    by = c("cohort", "name", "value")
  )
  
  # Heatmap
  plt <- df_plot |>
    dplyr::rowwise() |>
    dplyr::mutate(name = stringr::str_split(name, "_")[[1]][2]) |>
    dplyr::rename(
      variable = name, 
      description = value, 
      frequency = f
    ) |>
    ggplot2::ggplot(ggplot2::aes(variable, description)) +
    ggplot2::geom_tile(ggplot2::aes(fill = frequency)) +
    ggplot2::geom_text(
      ggplot2::aes(label = round(frequency, 1)), 
      color = "black", 
      size = 3
    ) +
    ggplot2::scale_fill_distiller(palette = "Blues", direction = 1) +
    ggplot2::coord_fixed() +
    ggplot2::facet_grid(cohort ~ .) +
    ggplot2::labs(
      caption = "1: quantifiable, 2: <LOD, 3: interference or out of range, 4: no sample"
    ) +
    ggplot2::theme(
      axis.line = ggplot2::element_line(color = "black"), 
      plot.background = ggplot2::element_blank(), 
      panel.grid.major = ggplot2::element_blank(), 
      panel.grid.minor = ggplot2::element_blank(), 
      panel.border = ggplot2::element_blank()
    )
  
  df_plot <- df_plot |>
    dplyr::arrange(dplyr::desc(value), 
                   name, 
                   dplyr::desc(f))
  
  return(list(
    dat = df_plot, 
    plot = plt
  ))
} # End function viz_desc_vars
################################################################################
