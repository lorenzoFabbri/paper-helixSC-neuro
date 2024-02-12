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
    tidylog::select(
      dag, variable, type, description, code, label,
      remark, comments
    )
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
    ) |>
    gt::opt_footnote_marks(
      marks = "letters"
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
    mapped_adj_sets[[paste0(
      "rq",
      rq
    )]] <- colnames(get(
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
      -dplyr::any_of(colnames(rq2_preproc_dat$exposures))
    )
  
  # Add metadata for better labels
  df_meta <- myphd::add_metadata(
    dat = df,
    metadat = dat_request$meta |>
      tidylog::mutate(
        comments = stringr::str_replace_all(
          comments,
          pattern = " ",
          replacement = ""
        )
      ),
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
      ),
      digits = dplyr::everything() ~ 1,
      missing = "ifany"
    ) |>
    gtsummary::add_overall() |>
    gtsummary::as_gt() |>
    gt::opt_footnote_marks(
      marks = "letters"
    )
  
  return(desc_covars)
} # End function tbl_desc_pop
################################################################################

#' Title
#'
#' @return
#' @export
tbl_desc_vars <- function() {
  params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
  path_store <- Sys.getenv("path_store")
  
  dat_request <- load_dat_request()
  edcs <- myphd::edcs_information() |>
    dplyr::filter(!chem_id %in% c("dedtp", "dmdtp")) |>
    dplyr::select(chem_id, short_name, class)
  metabolites <- load_steroids()$metabolome
  steroids <- readODS::read_ods("docs/steroids.ods")
  dat <- list()
  
  # EDCs
  dat$exposures <- dat_request$dat |>
    tidylog::select(
      params_dat$variables$identifier,
      dplyr::any_of(params_dat$variables[["rq1"]]$exposures)
    )
  tbl_edcs <- dat$exposures |>
    tidylog::rename_with(
      .fn = \(x) gsub("hs_|_c", "", x)
    ) |>
    tidylog::select(
      -dplyr::any_of(c("cohort", "HelixID"))
    ) |>
    dplyr::rename_with(
      ~tibble::deframe(edcs)[.x],
      .cols = edcs$chem_id
    ) |>
    gtsummary::tbl_summary(
      statistic = list(
        gtsummary::all_continuous() ~ c(
          "{median} ({p25}, {p75}); {N_miss} ({p_miss})"
        )
      ),
      missing = "no",
      digits = dplyr::everything() ~ 1
    )
  
  desc_chems <- tbl_edcs |>
    gtsummary::as_gt() |>
    gt::tab_row_group(
      label = "OP pesticide metabolites",
      rows = variable %in% as.character(edcs[edcs$class == "OP pesticide metabolites", ]$short_name)
    ) |>
    gt::tab_row_group(
      label = "Phenols",
      rows = variable %in% as.character(edcs[edcs$class == "Phenols", ]$short_name)
    ) |>
    gt::tab_row_group(
      label = "Phthalate metabolites",
      rows = variable %in% as.character(edcs[edcs$class == "Phthalate metabolites", ]$short_name)
    ) |>
    gt::row_group_order(
      groups = c("OP pesticide metabolites",
                 "Phenols",
                 "Phthalate metabolites")
    ) |>
    gt::tab_style(
      locations = gt::cells_row_groups(group = dplyr::everything()),
      style = list(gt::cell_text(weight = "bold"))
    ) |>
    gt::opt_footnote_marks(
      marks = "letters"
    )
  ##############################################################################
  
  ## Glucocorticosteroids
  metabolites <- myphd::extract_cohort(
    dat = metabolites,
    id_var = "HelixID",
    st = 1, en = 3
  )
  dat$outcome <- metabolites |>
    tidylog::select(
      cohort,
      dplyr::any_of(params_dat$variables[["rq2"]]$outcome)
    ) |>
    tidylog::mutate(
      cohort = dplyr::case_when(
        cohort == "EDE" ~ "EDEN",
        cohort == "KAN" ~ "KANC",
        cohort == "MOB" ~ "MOBA",
        cohort == "RHE" ~ "RHEA",
        .default = cohort
      )
    ) |>
    tidylog::rename_with(
      .fn = \(x) gsub("^X", "", x)
    ) |>
    tidylog::rename(
      `E` = "CortisoneE",
      `17-DO-cortolone` = "17DOcortolone"
    )
  
  desc_mets <- dat$outcome |>
    gtsummary::tbl_summary(
      statistic = list(
        gtsummary::all_continuous() ~ c(
          "{median} ({p25}, {p75})"
        )
      ),
      missing = "ifany",
      by = "cohort",
      digits = dplyr::everything() ~ 1
    ) |>
    gtsummary::add_overall() |>
    gtsummary::as_gt()
  for (x in unique(steroids$type)) {
    desc_mets <- desc_mets |>
      gt::tab_row_group(
        label = x,
        rows = variable %in% as.character(
          steroids[steroids$type == x, ]$acronym
        )
      )
  }
  desc_mets <- desc_mets |>
    gt::tab_style(
      locations = gt::cells_row_groups(group = dplyr::everything()),
      style = list(gt::cell_text(weight = "bold"))
    ) |>
    gt::row_group_order(
      groups = c(
        "Glucocorticosteroid",
        "Glucocorticosteroid metabolite",
        "Glucocorticosteroid precursor",
        "Glucocorticosteroid precursor metabolite",
        "Androgen",
        "Androgen metabolite"
      )
    ) |>
    gt::opt_footnote_marks(
      marks = "letters"
    )
  ##############################################################################
  
  return(list(
    desc_chems = desc_chems,
    desc_mets = desc_mets
  ))
} # End function tbl_desc_vars
################################################################################

#' Title
#'
#' @param path_store
#' @param rq
#'
#' @return
#' @export
load_tidy_res_weighting <- function(path_store, rq) {
  # Load results weighting
  targets::tar_load(
    paste0("rq", substr(rq, 1, 1), "_weights"),
    store = paste0(path_store, rq)
  )
  obj <- get(paste0("rq", substr(rq, 1, 1), "_weights"))$estimated_weights
  
  # Extract summary information for all exposures
  info <- lapply(seq_along(obj), function(idx) {
    x <- cobalt::bal.tab(obj[[idx]])
    tmp <- x$Observations |>
      as.data.frame() |>
      tibble::rownames_to_column() |>
      tibble::as_tibble()
    colnames(tmp) <- c("type", "N")
    tmp |>
      tidyr::pivot_wider(
        names_from = "type",
        values_from = "N"
      ) |>
      tidylog::mutate(
        exposure = names(obj)[[idx]],
        exposure = gsub("hs_|_c", "", exposure),
        exposure = gsub("_", " ", exposure),
        exposure = replace(
          exposure, tolower(exposure) == "x11bhsd", "11bhsd"
        )
      ) |>
      tidylog::relocate(exposure)
  }) |>
    dplyr::bind_rows()
  
  ## Eventually add information on EDCs (chemical classes)
  if (rq %in% c("1", "2")) {
    info_edcs <- myphd::edcs_information() |>
      tibble::as_tibble()
    info <- info |>
      tidylog::left_join(
        info_edcs |>
          tidylog::select(chem_id, class),
        by = c("exposure" = "chem_id")
      )
  }
  
  return(info)
} # End function load_tidy_res_weighting
################################################################################

#' Title
#'
#' @param path_store
#' @param rq
#' @param sa_var
#'
#' @return
#' @export
load_res_weighted_fits <- function(path_store, rq, sa_var) {
  # Load results weighted fits
  targets::tar_load(
    dplyr::contains(paste0("rq", substr(rq, 1, 1), "_weighted_fits_")),
    store = paste0(path_store, rq)
  )
  res_list <- mget(ls(
    pattern = paste0("rq", substr(rq, 1, 1), "_weighted_fits_*")
  ))
  names_ <- gsub(
    paste0("rq", substr(rq, 1, 1), "_weighted_fits_"),
    "",
    names(res_list)
  )
  
  # Extract weights
  weights_ <- lapply(1:1, function(idx) {
    outcome <- gsub(
      paste0("rq", substr(rq, 1, 1), "_weighted_fits_"),
      "",
      names_[idx]
    )
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
  
  # Tidy values of effect modifier
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
  } # End check if SA
  
  # Tidy data
  weights_wide <- weights_
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
  ## Eventually add information on EDCs (chemical classes)
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
        class = c("Glucocorticosteroids")
      )
  }
  
  return(list(
    dat_tbl = weights_wide,
    dat_plt = weights_
  ))
} # End function load_res_weighted_fits
################################################################################

#' Title
#'
#' @param dat_tbl
#' @param dat_plt
#' @param sa_var
#'
#' @return
#' @export
tidy_res_weighted_fits <- function(dat_tbl, dat_plt, sa_var) {
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
  by <- if (is.null(sa_var)) {
    NULL
  } else {
    "modifier"
  }
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
    ) |>
    gt::opt_footnote_marks(
      marks = "letters"
    )
  
  return(list(
    table = tbl,
    plot = plot
  ))
} # End function tidy_res_weighted_fits
################################################################################

#' Title
#'
#' @param path_store
#' @param rq
#' @param sa_var
#' @param which_res
#'
#' @return
#' @export
load_res_meffects <- function(path_store, rq, sa_var, which_res) {
  # Load results
  targets::tar_load(
    dplyr::contains(paste0("rq", substr(rq, 1, 1), "_marginal_")),
    store = paste0(path_store, rq)
  )
  marginal_effects <- mget(ls(
    pattern = paste0("rq", substr(rq, 1, 1), "_marginal_*")
  ))
  names_ <- gsub(
    paste0("rq", substr(rq, 1, 1), "_marginal_"),
    "",
    names(marginal_effects)
  )
  
  # Loop over outcomes
  ret <- lapply(seq_along(marginal_effects), function(idx) {
    outcome <- gsub(
      paste0("rq", substr(rq, 1, 1), "_marginal_"),
      "",
      names(marginal_effects[idx])
    )
    if (outcome != "x11bhsd") {
      outcome <- gsub("hs", "", outcome)
    } else {
      outcome <- "11bhsd"
    }
    # Table for one outcome and all exposures
    x <- marginal_effects[[idx]]
    if (length(x$marginal_effects) == 0) {
      return(NULL)
    }
    
    if (which_res == "gcomp") {
      gcomp_res <- lapply(x$marginal_effects, "[[", which_res)
      df <- lapply(gcomp_res, function(x) {
        x <- setNames(
          x,
          c(
            "exposure", "estimate",
            "p.value", "s.value",
            "conf.low", "conf.high"
          )
        )
        x
      }) |>
        dplyr::bind_rows(.id = "variable")
      # End check if gcomp
    } else {
      df <- lapply(x$marginal_effects, "[[", which_res) |>
        dplyr::bind_rows() |>
        tidylog::select(-dplyr::any_of(
          c("contrast", "statistic")
        ))
      
      if (which_res == "hypothesis") {
        df$variable <- names(x$marginal_effects)
      }
    } # End check type of results
    
    df <- df |>
      tidylog::mutate(
        outcome = outcome
      )
    
    return(df)
  }) # End loop over results of marginal effects
  
  # Tidy
  names(ret) <- names_
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
  
  ## Eventually add information on EDCs (chemical classes)
  if (substr(rq, 1, 1) %in% c("1", "2")) {
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
        class = c("Glucocorticosteroids"),
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
          x <- factor(
            x,
            levels = sort(unique(x))
          )
        }
      ),
      dplyr::across(
        dplyr::where(is.character),
        \(x) stringr::str_trim(x, side = "both")
      )
    )
  ## Tidy values of effect modifier
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
} # End function load_res_meffects
################################################################################

#' Title
#'
#' @param df
#' @param sa_var
#' @param outcome
#' @param which_res
#' @param num_digits_est
#' @param num_digits_sig
#'
#' @return
#' @export
tidy_res_meffects <- function(df, sa_var, outcome,
                              which_res,
                              num_digits_est, num_digits_sig) {
  if (length(outcome) > 0) {
    df <- df |>
      tidylog::filter(
        outcome %in% .env$outcome
      )
  }
  
  df <- df |>
    tidylog::mutate(
      variable = as.character(variable),
      variable = dplyr::case_match(
        variable,
        "corticosterone production" ~ "corticost. prod.",
        "cortisol production" ~ "cortisol prod.",
        "cortisone production" ~ "cortisone prod.",
        "cortisol metabolism" ~ "cortisol met.",
        "cortisone metabolism" ~ "cortisone met.",
        .default = variable
      )
    )
  
  # Forest plots side-by-side
  if (is.null(sa_var) | which_res == "hypothesis") {
    if (length(outcome) > 0) {
      plot <- df |>
        ggplot2::ggplot(
          mapping = ggplot2::aes(
            x = estimate,
            y = forcats::fct_reorder2(variable, estimate, class),
            color = class,
            shape = outcome
          )
        )
    } else {
      plot <- df |>
        ggplot2::ggplot(
          mapping = ggplot2::aes(
            x = estimate,
            y = forcats::fct_reorder2(variable, estimate, class),
            color = class
          )
        )
    }
  } else {
    plot <- df |>
      ggplot2::ggplot(
        mapping = ggplot2::aes(
          x = estimate,
          y = forcats::fct_reorder2(
            variable, estimate, class
          ),
          color = class,
          shape = modifier
        )
      )
  }
  position <- if (is.null(sa_var) & length(outcome) <= 1) {
    "identity"
  } else {
    ggstance::position_dodgev(height = 0.9)
  }
  plot <- plot +
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
      width = 0.3,
      linewidth = 0.3
    ) +
    ggplot2::geom_vline(
      xintercept = 0.0,
      linetype = "longdash"
    ) +
    ggplot2::labs(
      x = "Marginal contrast"
    ) +
    ggplot2::theme_classic()
  if (is.null(sa_var) & which_res == "comparisons") {
    if (length(outcome) > 1) {
      plot <- plot +
        ggplot2::coord_cartesian(
          ylim = c(1, (nrow(df) / length(outcome)) + 1)
        ) +
        ggplot2::annotate(
          "text",
          x = 0, y = (nrow(df) / length(outcome)) + 1,
          label = ""
        )
    } else {
      plot <- plot +
        ggplot2::coord_cartesian(
          ylim = c(1, nrow(df) + 1)
        ) +
        ggplot2::annotate(
          "text",
          x = 0, y = nrow(df) + 1,
          label = ""
        )
    }
    plot <- plot +
      ggplot2::theme(
        legend.position = "bottom",
        legend.box = "vertical",
        legend.margin = ggplot2::margin(),
        axis.title.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_blank(),
        axis.line.x = ggplot2::element_line(linewidth = 0.6),
        axis.ticks.length = ggplot2::unit(0.3, "cm")
      )
  } else {
    plot <- plot +
      ggplot2::labs(
        y = "exposure"
      ) +
      ggplot2::facet_wrap(
        ggplot2::vars(outcome),
        ncol = 2
      ) +
      ggplot2::theme(
        plot.caption = ggplot2::element_text(hjust = 0),
        strip.background = ggplot2::element_blank(),
        legend.position = "right",
        axis.ticks.y = ggplot2::element_blank(),
        axis.line.x = ggplot2::element_line(linewidth = 0.6),
        axis.ticks.length = ggplot2::unit(0.3, "cm")
      )
  }
  ##############################################################################
  
  # Pretty tables w/ numerical results
  names_ <- unique(df$outcome)
  names_ <- if (is.null(sa_var) | which_res == "hypothesis") {
    names_
  } else {
    paste0(names_, "_", c(unique(df$modifier)))
  }
  processed_df <- df |>
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
        "val", "estimate"
      ))
    )
  df_gt <- processed_df |>
    tidylog::select(-estimate) |>
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
    # gt::tab_style(
    #   style = gt::cell_text(weight = "bold"),
    #   locations = purrr::map(
    #     names_,
    #     \(x) {
    #       gt::cells_body(
    #         columns = x,
    #         rows = stringr::str_detect(!!rlang::sym(x), "\\*")
    #       )
    #     }
    #   )
    # ) |>
    # gt::tab_footnote(
    #   footnote = "*Significant results."
    # ) |>
    gt::tab_footnote(
      footnote = "Estimate and 95% CI.",
      locations = gt::cells_column_labels(
        columns = names_
      )
    ) |>
    gt::opt_footnote_marks(
      marks = "letters"
    )
  
  if (is.null(sa_var) & which_res == "comparisons") {
    processed_df <- dplyr::bind_rows(
      tibble::tibble(
        variable = "Exposure",
        val = "Marginal contrast (95% CI)",
        estimate = 100,
        outcome = "",
        class = ""
      ),
      processed_df
    )
  }
  
  return(list(
    numerical_results = processed_df,
    table = df_gt,
    plot = plot
  ))
} # End function tidy_res_meffects
################################################################################

#' Title
#'
#' @param df
#'
#' @return
#' @export
plot_estimate_over_time <- function(df) {
  # Structure of df:
  # - time_period (character, x-axis).
  # - outcome (character).
  # - variable (character, for faceting).
  # - estimate (numeric, y-axis).
  # - rvar (for distribution of estimate).
  
  # Add information on EDCs
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
      outcome = factor(outcome),
      class = factor(
        class,
        levels = c(
          "OP pesticide metabolites",
          "Phenols",
          "Phthalate metabolites"
        )
      ),
      variable = as.factor(variable)
    )
  
  # Plot
  plts <- lapply(unique(df$variable), function(x) {
    df |>
      dplyr::filter(variable == x) |>
      dplyr::group_by(class) |>
      dplyr::arrange(dplyr::desc(variable)) |>
      dplyr::ungroup() |>
      ggplot2::ggplot(ggplot2::aes(
        x = time_period,
        ydist = rvar
      )) +
      ggplot2::scale_x_discrete() +
      ggdist::stat_slabinterval() +
      ggplot2::geom_hline(
        yintercept = 0.0,
        linewidth = 0.2
      ) +
      ggplot2::guides(size = "none") +
      ggplot2::labs(
        title = glue::glue(
          "Outcome: {out}. Exposure: {expo}",
          out = stringr::str_replace(
            df$outcome[[1]],
            pattern = "_",
            replacement = " "
          ),
          expo = x
        ),
        subtitle = "Simulation-based inference",
        x = "time period",
        y = "estimate",
        caption = "nu: night urine. mu: morning urine. week: pooled weekly sample."
      ) +
      ggplot2::theme(
        plot.caption = ggplot2::element_text(hjust = 0),
        text = ggplot2::element_text(
          size = 12
        )
      )
  }) # End loop plot exposures
  ## Arrange all plots in multiple pages
  ret <- gridExtra::marrangeGrob(
    grobs = plts,
    nrow = 1, ncol = 1
  )
  
  return(ret)
} # End function plot_estimate_over_time
################################################################################

#' Title
#'
#' @param df_preds
#'
#' @return
#' @export
plot_adrf <- function(df_preds) {
  adrf <- df_preds |>
    ggplot2::ggplot(ggplot2::aes(
      x = exposure
    )) +
    ggplot2::geom_point(
      ggplot2::aes(y = estimate)
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = estimate),
      linewidth = 0.2
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = conf.low,
        ymax = conf.high
      ),
      alpha = 0.2
    ) +
    ggplot2::scale_x_continuous(
      limits = c(
        min(df_preds[["exposure"]]),
        max(df_preds[["exposure"]])
      )
    ) +
    ggplot2::labs(
      x = unique(df_preds[["variable"]]),
      y = "E[Y|A]"
    ) +
    ggplot2::theme_minimal()
  
  return(adrf)
} # End function plot_adrf
################################################################################

#' Visualize overlap of a variable based on a grouping factor
#'
#' @param dat
#' @param id_var
#' @param group_var
#' @param lower_percentile
#' @param upper_percentile
#' @param rq
#'
#' @returns
#' @export
viz_overlap_quantiles <- function(dat,
                                  id_var,
                                  group_var,
                                  lower_percentile,
                                  upper_percentile,
                                  rq) {
  # Process data
  dat_proc <- dat |>
    tidylog::select(-dplyr::any_of(id_var))
  colnames(dat_proc) <- colnames(dat_proc) |>
    stringr::str_replace_all(
      pattern = "hs_|_c",
      replacement = ""
    ) |>
    stringr::str_replace_all(
      pattern = "_",
      replacement = " "
    )
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
      ggplot2::xlim(
        min(dat_proc[[x]]),
        quantile(dat_proc[[x]], upper_percentile)
      ) +
      ggplot2::labs(
        caption = glue::glue(
          "Red bars: {lower}th and {upper}th percentiles.
          Figure truncated at the {upper}th percentile of the exposure.",
          lower = lower_percentile * 100,
          upper = upper_percentile * 100
        )
      )
  }) # End loop over variables to plot
  
  # Save results (single file)
  ggplot2::ggsave(
    paste0("results/figures/overlap_exp_", rq, ".pdf"),
    gridExtra::marrangeGrob(
      grobs = ret,
      nrow = 1,
      ncol = 1
    ),
    dpi = 360,
    height = 6
  )
} # End function viz_overlap_quantiles
################################################################################

#' Visualize the clinical outcome across age and by sex
#'
#' @returns
#' @export
viz_clinical_outcome <- function() {
  # Load data
  dat <- load_dat_request()$dat
  dat <- myphd::extract_cohort(dat, id_var = "HelixID",
                               st = 1, en = 3)
  outcome <- vars_of_interest(append_to_chem = NULL)$outcomes
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
    )
  ) +
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
  
  # if (is_chem) {
  #   plt <- plt +
  #     ggplot2::labs(caption = "1: quantifiable, 2: <LOD, 3: interference or out of range, 4: not analysed")
  # } else {
  #   plt <- plt +
  #     ggplot2::labs(caption = "1: quantifiable, 2: <LOQ, 3: interference or out of range, 4: not detected")
  # }
  
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
