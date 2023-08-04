#' Title
#'
#' @param dat 
#' @param meta 
#' @param vars 
#' @param fct_levels 
#'
#' @return
#' @export
viz_desc_vars <- function(dat, meta, vars, fct_levels) {
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
