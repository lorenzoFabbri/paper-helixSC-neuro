#' Title
#'
#' @param rq 
#'
#' @return
#' @export
tidy_codebooks <- function(rq) {
  path_store <- Sys.getenv("path_store")
  
  ## Load objects
  targets::tar_load(
    paste0("rq", rq, "_preproc_dat"),
    store = paste0(path_store, rq)
  )
  
  ## Extract adjustments set used and mapped covariates
  adj_set <- get(paste0("rq", rq, "_preproc_dat"))$adjustment_set
  mapped_covars <- get(paste0("rq", rq, "_preproc_dat"))$mapping_covariates
  actual_covars <- get(paste0("rq", rq, "_preproc_dat"))$covariates |>
    colnames()
  meta <- get(paste0("rq", rq, "_preproc_dat"))$meta |>
    tidylog::filter(
      dag %in% adj_set &
        variable %in% mapped_covars
    ) |>
    tidylog::select(dag, variable, type, description, code, label,
                    remark, comments)
  meta <- meta |>
    dplyr::rowwise() |>
    tidylog::mutate(
      included = ifelse(
        variable %in% actual_covars,
        TRUE, FALSE
      )
    )
  perc_included <- round(sum(meta$included == TRUE) / nrow(meta) * 100, 2)
  
  ## Create tidy table
  ret <- meta |>
    tidylog::mutate(
      dplyr::across(
        dplyr::everything(),
        as.character
      )
    ) |>
    tidylog::group_by(dag) |>
    dplyr::arrange(dag, variable) |>
    tidylog::ungroup() |>
    tidylog::rename(
      coding = "code",
      labels = "label",
      remarks = "remark"
    ) |>
    tidylog::mutate(
      dplyr::across(
        c("coding", "labels", "remarks", "comments"),
        \(x) ifelse(
          x == "NA",
          "", x
        )
      )
    ) |>
    gt::gt(
      rowname_col = "variable",
      groupname_col = "dag"
    ) |>
    gt::tab_style(
      style = gt::cell_text(
        weight = "bold"
      ),
      locations = gt::cells_row_groups()
    ) |>
    gt::sub_missing(
      missing_text = ""
    ) |>
    gt::tab_footnote(
      footnote = glue::glue(
        "Percentage of confounders included in the models: {perc_included}%."
      ),
      locations = gt::cells_column_labels(columns = included)
    )
  
  return(ret)
} # End function tidy_codebooks
################################################################################

#' Title
#'
#' @param num_digits_est 
#' @param num_digits_sig 
#'
#' @return
#' @export
tbl_desc_pop <- function(num_digits_est, num_digits_sig) {
  path_store <- Sys.getenv("path_store")
  
  # Load data request (all variables)
  params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
  dat_request <- load_dat_request()
  dat_request$dat <- dat_request$dat |>
    tidylog::mutate(
      cohort = dplyr::case_when(
        cohort == "EDE" ~ "EDEN",
        cohort == "KAN" ~ "KANC",
        cohort == "MOB" ~ "MOBA",
        cohort == "RHE" ~ "RHEA",
        .default = cohort
      )
    )
  ## Handle time variables
  cols_to_season <- c("hs_date_neu", "e3_cbirth")
  dat_request$dat <- myphd::convert_time_season(
    dat = dat_request$dat,
    cols = cols_to_season
  ) |>
    tidylog::mutate(dplyr::across(
      dplyr::any_of(cols_to_season),
      \(x) factor(x)
    ))
  
  # Extract confounders by RQ
  mapped_adj_sets <- list()
  for (rq in c("1", "2", "3")) {
    targets::tar_load(
      paste0("rq", rq, "_preproc_dat"),
      store = paste0(path_store, rq)
    )
    
    # Only actually used covariates
    mapped_adj_sets[[paste0("rq",
                            rq)]] <- colnames(get(
                              paste0("rq", rq, "_preproc_dat")
                            )$covariates)
  }
  
  # Select confounders and clinical outcome in data request
  all_confounders <- mapped_adj_sets |>
    unlist() |>
    unname() |>
    unique()
  df <- dat_request$dat |>
    tidylog::select(
      HelixID,
      params_dat$variables$rq1$outcome,
      dplyr::any_of(all_confounders)
    )
  ## Must add creatinine from IMIM and remove chemicals
  df <- df |>
    tidylog::left_join(
      rq2_preproc_dat$covariates |>
        tidylog::select(HelixID, creatinine_to_helix),
      by = "HelixID"
    ) |>
    tidylog::select(
      -HelixID,
      -dplyr::matches("^hs_.*_c|_cadj$")
    )
  
  # Add metadata for better labels
  df_meta <- myphd::add_metadata(
    dat = df,
    metadat = dat_request$meta,
    categorical_types = c("categorical", "character", "integer"),
    cols_to_exclude = c("cohort")
  )
  
  # Generate "Table 1"
  desc_covars <- df_meta |>
    gtsummary::tbl_summary(
      by = "cohort",
      type = list(
        hs_fastfood ~ "continuous",
        hs_org_food ~ "continuous"
      ),
      statistic = list(
        gtsummary::all_continuous() ~ c(
          "{median} ({p25}, {p75})"
        ),
        gtsummary::all_categorical() ~ "{n} ({p}%)"
      )
    ) |>
    gtsummary::add_overall()
  
  # Save table
  path <- "results/tables/"
  name <- "tbl_desc_pop.docx"
  # gt::gtsave(
  #   data = desc_covars |>
  #     gtsummary::as_gt(),
  #   filename = paste0(path, name)
  # )
  
  return(gtsummary::as_gt(desc_covars))
} # End function tbl_desc_pop
################################################################################

#' Title
#'
#' @return
#' @export
tbl_desc_vars <- function() {
  path_store <- Sys.getenv("path_store")
  
  ## Load objects
  rq <- "02"
  targets::tar_load(
    "rq02_desc_data_exp",
    store = paste0(path_store, rq)
  )
  
  ## Save table
  path <- "results/tables/"
  name <- "tbl_desc_chemicals.docx"
  # gt::gtsave(
  #   data = rq02_desc_data_exp$step3 |>
  #     gtsummary::as_gt(),
  #   filename = paste0(path, name)
  # )
  ##############################################################################
  
  ## Load objects
  targets::tar_load(
    "rq02_desc_data_out",
    store = paste0(path_store, rq)
  )
  
  ## Save table
  path <- "results/tables/"
  name <- "tbl_desc_metabolites.docx"
  # gt::gtsave(
  #   data = rq02_desc_data_out$step3 |>
  #     gtsummary::as_gt(),
  #   filename = paste0(path, name)
  # )
  
  return(list(
    desc_chems = gtsummary::as_gt(rq02_desc_data_exp$step3),
    desc_mets = gtsummary::as_gt(rq02_desc_data_out$step3)
  ))
} # End function tbl_desc_vars
################################################################################

#' Title
#'
#' @param dat_tbl
#' @param dat_plt
#' @param sa_var
#'
#' @return
#' @export
tidy_res_weighted_fits <- function(dat_tbl, dat_plt, sa_var = NULL) {
  # Plot distribution of weights
  plot <- dat_plt |>
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
    )
  if (length(unique(dat_plt$class)) == 1) {
    plot <- plot +
      ggplot2::theme(
        legend.position = "null"
      )
  }
  if (!is.null(sa_var)) {
    plot <- plot +
      ggplot2::facet_wrap(
        ggplot2::vars(modifier),
        ncol = length(unique(dat_plt$modifier)),
        scales = "free"
      )
  } else {
    plot <- plot +
      ggplot2::theme(
        strip.text.x = ggplot2::element_blank(),
        text = ggplot2::element_text(
          size = 12
        )
      )
  }
  
  # Tidy table with numerical values
  colnames(dat_tbl) <- gsub(
    "hs_|_c", "", colnames(dat_tbl)
  )
  dat_tbl <- dat_tbl |>
    tidylog::select(-outcome)
  by <- if (is.null(sa_var)) {NULL} else {"modifier"}
  tbl <- c("{median} ({p25}, {p75})", "{min}, {max}") |>
    purrr::map(
      ~ dat_tbl |>
        gtsummary::tbl_summary(
          by = by,
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
    )
  
  if (is.null(sa_var)) {
    tbl <- tbl |>
      gtsummary::modify_spanning_header(
        list(
          stat_0_1 ~ "Median (IQR)",
          stat_0_2 ~ "Range"
        )
      )
  } else {
    tbl <- tbl |>
      gtsummary::modify_spanning_header(
        list(
          c(stat_1_1, stat_2_1) ~ "Median (IQR)",
          c(stat_1_2, stat_2_2) ~ "Range"
        )
      )
  }
  
  tbl <- tbl |>
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
################################################################################

#' Title
#'
#' @param df
#' @param sa_var
#' @param which_res
#' @param num_digits_est
#' @param num_digits_sig
#'
#' @return
#' @export
tidy_res_meffects <- function(df, sa_var = NULL,
                              which_res,
                              num_digits_est, num_digits_sig) {
  # Forest plots side-by-side
  if (is.null(sa_var) | which_res == "hypothesis") {
    plot <- df |>
      ggplot2::ggplot(
        mapping = ggplot2::aes(x = estimate,
                               y = forcats::fct_reorder2(
                                 variable, estimate, class
                               ),
                               color = class)
      )
  } else {
    plot <- df |>
      ggplot2::ggplot(
        mapping = ggplot2::aes(x = estimate,
                               y = forcats::fct_reorder2(
                                 variable, estimate, class
                               ),
                               color = class,
                               shape = modifier)
      )
  }
  position <- if (is.null(sa_var)) {"identity"} else {
    ggstance::position_dodgev(height = 0.5)
  }
  plot <- plot +
    ggplot2::scale_y_discrete() +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        size = .data[["s.value"]]
      ),
      position = position,
      show.legend = TRUE
    ) +
    ggplot2::guides(
      size = "none"
    ) +
    ggplot2::geom_errorbar(
      mapping = ggplot2::aes(
        xmin = conf.low,
        xmax = conf.high
      ),
      position = position,
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
  names_ <- unique(df$outcome)
  names_ <- if (is.null(sa_var) | which_res == "hypothesis") {names_} else {
    paste0(names_, "_", c(unique(df$modifier)))
  }
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
      dplyr::any_of(c(
        "class", "variable", "outcome",
        "modifier",
        "val"
      ))
    ) |>
    tidyr::pivot_wider(
      names_from = dplyr::any_of(c("outcome", "modifier")),
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
################################################################################

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
################################################################################

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
################################################################################

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
