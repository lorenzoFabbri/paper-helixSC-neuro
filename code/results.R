source("DAGs/dag_v2.R")
source("code/dictionaries.R")
source("code/data.R")
source("code/plot.R")
source("code/qba.R")

Sys.setenv(is_hpc = TRUE)
path_store <- ifelse(
  Sys.getenv("is_hpc"), 
  "/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/_targets/_targetsRQ", 
  "~/mounts/rstudioserver/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/_targets/_targetsRQ"
)
num_digits_est <- 3
num_digits_sig <- 2
sa_var <- "e3_sex"
################################################################################

# Table 1 (description population and outcome)
tbl_desc_pop <- function(path_store) {
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
  gt::gtsave(
    data = desc_covars |>
      gtsummary::as_gt(),
    filename = paste0(path, name)
  )
  
} # End function tbl_desc_pop
tbl_desc_pop(path_store = path_store)
################################################################################

# Table (description chemicals and metabolites)
tbl_desc_vars <- function(path_store) {
  ## Load objects
  rq <- "02"
  targets::tar_load(
    "rq02_desc_data_exp",
    store = paste0(path_store, rq)
  )
  
  ## Save table
  path <- "results/tables/"
  name <- "tbl_desc_chemicals.docx"
  gt::gtsave(
    data = rq02_desc_data_exp$step3 |>
      gtsummary::as_gt(),
    filename = paste0(path, name)
  )
  ##############################################################################
  
  ## Load objects
  targets::tar_load(
    "rq02_desc_data_out",
    store = paste0(path_store, rq)
  )
  
  ## Save table
  path <- "results/tables/"
  name <- "tbl_desc_metabolites.docx"
  gt::gtsave(
    data = rq02_desc_data_out$step3 |>
      gtsummary::as_gt(),
    filename = paste0(path, name)
  )
} # End function tbl_desc_vars
tbl_desc_vars(path_store = path_store)
################################################################################

# Visualize weighs and marginal effects
for (rq in c("1", "2", "3", "1SA", "2SA", "3SA")) {
  # Weighted fits
  ## Load objects
  targets::tar_load(
    dplyr::contains(paste0(
      "rq", substr(rq, 1, 1), "_weighted_fits_"
    )),
    store = paste0(path_store, rq)
  )
  
  ## Tidy results
  ret <- tidy_res_weighted_fits(
    res_list = mget(ls(
      pattern = paste0("rq", substr(rq, 1, 1), "_weighted_fits_*")
    )),
    rq = substr(rq, 1, 1),
    sa_var = if (grepl("SA$", rq)) sa_var else NULL
  )
  
  ## Save results (plot and table)
  path <- "results/figures/weighted_fits/"
  name <- paste0("rq", rq, ".pdf")
  ggplot2::ggsave(
    filename = paste0(path, name),
    plot = ret$plot,
    width = 7,
    dpi = 300
  )
  path <- "results/tables/weighted_fits/"
  name <- paste0("rq", rq, ".docx")
  gt::gtsave(
    data = ret$table,
    filename = paste0(path, name)
  )
  
  ## Remove objects to save space
  rm(ret)
  rm(list = ls(pattern = paste0("rq", substr(rq, 1, 1), "_weighted_fits_*")))
  ##############################################################################
  
  # Marginal effects
  ## Load objects
  targets::tar_load(
    dplyr::contains(paste0(
      "rq", substr(rq, 1, 1), "_marginal_"
    )),
    store = paste0(path_store, rq)
  )
  
  ## Tidy results (comparisons)
  ret <- tidy_res_meffects(
    marginal_effects = mget(ls(
      pattern = paste0("rq", substr(rq, 1, 1), "_marginal_*")
    )),
    rq = substr(rq, 1, 1),
    sa_var = if (grepl("SA$", rq)) sa_var else NULL,
    which_res = "comparisons"
  )
  
  ## Save results (plot and table)
  path <- "results/figures/marginal_effects/"
  name <- paste0("rq", rq, ".pdf")
  ggplot2::ggsave(
    filename = paste0(path, name),
    plot = ret$plot,
    dpi = 300,
    width = 9,
    height = 8
  )
  path <- "results/tables/marginal_effects/"
  name <- paste0("rq", rq, ".docx")
  gt::gtsave(
    data = ret$table,
    filename = paste0(path, name)
  )
  
  ## Tidy results (hypothesis)
  if (grepl("SA$", rq)) {
    ret <- tidy_res_meffects(
      marginal_effects = mget(ls(
        pattern = paste0("rq", substr(rq, 1, 1), "_marginal_*")
      )),
      rq = substr(rq, 1, 1),
      sa_var = sa_var,
      which_res = "hypothesis"
    )
    
    ## Save results (plot and table)
    path <- "results/figures/marginal_effects/"
    name <- paste0("rq", rq, "_hypothesis.pdf")
    ggplot2::ggsave(
      filename = paste0(path, name),
      plot = ret$plot,
      dpi = 300,
      width = 9,
      height = 8
    )
    path <- "results/tables/marginal_effects/"
    name <- paste0("rq", rq, "_hypothesis.docx")
    gt::gtsave(
      data = ret$table,
      filename = paste0(path, name)
    )
  }
  
  ## Remove objects to save space
  rm(ret)
  rm(list = ls(pattern = paste0("rq", substr(rq, 1, 1), "_marginal_*")))
} # End loop tidy results
################################################################################

# Files for SM
path_sm <- "output/paper/SM/"

## Visualization of description chemicals and metabolites
for (rq in c("01")) {
  ## Load objects
  targets::tar_load(
    dplyr::contains(paste0(
      "rq", rq, "_viz_desc_"
    )),
    store = paste0(path_store, rq)
  )
  
  ## Save results (plots)
  invisible(lapply(
    paste0("rq", rq, c("_viz_desc_chems", "_viz_desc_metabs")),
    function(x) {
      obj <- get(x)$plot +
        ggplot2::theme(
          legend.position = "bottom",
          text = ggplot2::element_text(
            size = 8
          )
        )
      
      ggplot2::ggsave(
        filename = paste0(path_sm,
                          gsub(paste0("rq", rq, "_"), "", x),
                          ".png"),
        plot = obj,
        dpi = 300,
        width = 7,
        height = 8,
        bg = "white"
      )
    }
  )) # End loop over type plots
  
  ## Remove objects to save space
  rm(list = ls(pattern = paste0("rq", rq, "_viz_desc_")))
}
################################################################################

## DAGs
all_dags <- dags()
invisible(
  lapply(seq_along(all_dags), function(idx) {
    file_conn <- file(paste0(path_sm,
                             "dag_",
                             names(all_dags)[[idx]]))
    writeLines(all_dags[[idx]],
               con = file_conn)
    close(file_conn)
  })
) # End invisible loop DAGs
################################################################################

## Adjustment sets by RQ w/ mapped covariates
for (rq in c("1", "2", "3")) {
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
  
  ## Save results (table)
  name <- paste0("rq", rq, "_codebook", ".docx")
  gt::gtsave(
    data = ret,
    filename = paste0(path_sm, name)
  )
  
  ## Remove objects to save space
  rm(ret)
  rm(list = ls(pattern = paste0("rq", rq, "_load_dat")))
} # End loop over RQs for tables adjustment sets
################################################################################
