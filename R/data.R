#' Compute "steroid scores"
#'
#' @param dat Dataset containing original variables. A dataframe.
#'
#' @return A dataset with additional variables. A dataframe.
#'
#' @export
create_steroid_scores <- function(dat) {
  warning(
    "Computation of `androgens_production` is wrong since AED is missing.",
    call. = TRUE
  )
  
  dat_ret <- dat |>
    tidylog::mutate(
      # Cortisol
      cortisol_production = rowSums(dplyr::across(c(`F`, X20aDHF, X20bDHF, X5bDHF, X5aTHF, X5bTHF, X6OHF, X5a20acortol, X5a20bcortol, X5b20acortol, X5b20bcortol))),
      cortisol_metabolism = rowSums(dplyr::across(c(X20aDHF, X20bDHF, X5bDHF, X5aTHF, X5bTHF, X6OHF, X5a20acortol, X5a20bcortol, X5b20acortol, X5b20bcortol))) / `F`,
      # Cortisone
      cortisone_production = rowSums(dplyr::across(c(CortisoneE, X20aDHE, X20bDHE, X5aTHE, X5bTHE, X6OHE, X5b20acortolone, X5b20bcortolone))),
      cortisone_metabolism = rowSums(dplyr::across(c(X20aDHE, X20bDHE, X5aTHE, X5bTHE, X6OHE, X5b20acortolone, X5b20bcortolone))) / CortisoneE,
      # 11beta
      X11bHSD = cortisone_production / cortisol_production,
      # Global reductases
      # global_reductase_f = rowSums(dplyr::across(c(X5aTHF, X5bTHF))) / `F`,
      # global_reductase_e = X5bTHE / CortisoneE,
      # Others
      cyp3a4 = X6OHF / `F`,
      corticosterone_production = rowSums(dplyr::across(c(X5aTHB, X5bTHB, A, X17DOcortolone))),
      # X11deoxycortisol_production = rowSums(dplyr::across(c(S, X5bDHS, X5bTHS))),
      # X11hydroxylase = rowSums(dplyr::across(c(X5aTHF, X5bTHF, X5bTHE))) / X5bTHS,
      # X17hydroxylase = rowSums(dplyr::across(c(X5aTHF, X5bTHF))) / rowSums(dplyr::across(c(X5aTHB, X5bTHB))),
      androgens_production = rowSums(dplyr::across(c(`T`, Andros, Etio))),
      # X5a_reductase = Andreos / Etio,
      lyase = rowSums(dplyr::across(c(Andros, Etio))) / rowSums(dplyr::across(c(X17HP, PT))),
      global_adrenal_function = rowSums(dplyr::across(c(X5aTHF, X5bTHF, X5bTHE))) / rowSums(dplyr::across(c(X17HP, PT)))
    )
  
  return(dat_ret)
}
################################################################################

#' Load the steroid metabolomics data
#'
#' @returns A named list with the metabolomics data,
#' the dataframe containing the description of the values for
#' each variable, the LOD values for each variable.
#'
#' @export
load_steroids <- function() {
  # Load data
  params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
  paths <- params_dat$paths
  steroids <- paths$path_all_steroids
  # This is the "clean" dataset provided by Oliver
  datasets <- c("corticosteroids_unprocessed.csv")
  
  # Loop over datasets
  tbls <- lapply(datasets, function(x) {
    # Metabolites
    dd <- readr::read_csv(
      file = paste0(steroids, x),
      col_names = TRUE,
      trim_ws = TRUE,
      col_types = readr::cols()
    ) |>
      tibble::as_tibble()
    dd <- janitor::clean_names(dd, case = "none")
    colnames(dd)[[1]] <- params_dat$variables$identifier
    
    # Tidy names
    dd <- dd |>
      tidylog::mutate(HelixID = stringr::str_trim(HelixID, side = "both")) |>
      tidylog::mutate(HelixID = stringr::str_replace(HelixID, "EDP", "EDE"))
    colnames(dd) <- gsub(
      pattern = "_",
      replacement = "",
      x = colnames(dd)
    )
    
    # Remove samples w/o data
    dd <- dd |>
      tidylog::filter(
        !HelixID %in% c("RHE232094", "MOB154")
      )
    
    # LOD information
    loq <- readr::read_csv(
      file = paste0(steroids, "lod.csv"),
      col_names = FALSE,
      trim_ws = TRUE,
      col_types = readr::cols()
    ) |>
      tibble::as_tibble()
    colnames(loq) <- c("metabolite", "loq")
    loq$metabolite <- janitor::make_clean_names(loq$metabolite,
                                                case = "none"
    )
    loq <- loq |>
      tidylog::mutate(metabolite = stringr::str_replace_all(
        metabolite, "_", ""
      ))
    assertthat::assert_that(
      nrow(loq) == ncol(dd) - 2,
      msg = "Mismatch in the number of metabolites."
    )
    ############################################################################
    
    # Cleaning
    cols <- setdiff(
      colnames(dd),
      params_dat$variables$identifier
    )
    ## Create new dataset for `_cdesc`
    dd_cdesc <- dd
    dd_cdesc <- dd_cdesc |>
      tidylog::mutate(dplyr::across(
        dplyr::all_of(cols),
        \(x) dplyr::case_when(
          # Value below the limit of quantification
          stringr::str_detect(x, "LOQ") ~ 2,
          # Interference or out of range
          stringr::str_detect(x, "\\*") ~ 3,
          # Value not detected
          stringr::str_detect(x, "n.d.") ~ 4,
          # Quantifiable
          TRUE ~ 1
        )
      ), .keep = "unused")
    dd_cdesc <- tidylog::rename_with(
      dd_cdesc,
      ~ paste0(.x, "_cdesc", recycle0 = TRUE),
      !dplyr::starts_with(params_dat$variables$identifier)
    )
    
    ## Remove unwanted columns and change to numeric
    # unwanted_cols <- c("AED")
    unwanted_cols <- c()
    dd <- dd |>
      tidylog::select(-dplyr::all_of(unwanted_cols)) |>
      tidylog::mutate(
        dplyr::across(
          dplyr::all_of(setdiff(cols, unwanted_cols)),
          \(x) dplyr::case_when(
            stringr::str_detect(x, "LOQ") ~ NA,
            stringr::str_detect(x, "\\*") ~ NA,
            stringr::str_detect(x, "n.d.") ~ NA,
            TRUE ~ x
          )
        ),
        dplyr::across(
          dplyr::all_of(setdiff(cols, unwanted_cols)),
          as.numeric
        )
      )
    dd_cdesc <- dd_cdesc |>
      tidylog::select(-dplyr::any_of(paste0(unwanted_cols, "_cdesc"))) |>
      tidylog::mutate(dplyr::across(
        dplyr::where(is.numeric),
        as.factor
      ))
    loq <- loq |>
      tidylog::filter(!metabolite %in% unwanted_cols)
    
    ## Convert units of creatinine. Note: ng/mL = mcrg/L (metabolites).
    dd <- dd |>
      tidylog::mutate(
        # Creatinine to g/L as in HELIX, instead of mcrmol/L.
        creatinine_to_helix = Creatinine * 1.1312 * 1e-4,
      )
    ############################################################################
    
    tbl_vals <- dd_cdesc
    tbl_vals$HelixID <- NULL
    tbl_vals <- tbl_vals |>
      c() |>
      unlist() |>
      unname() |>
      table()
    assertthat::assert_that(
      sum(is.na(dd)) == sum(tbl_vals[2:4]),
      msg = "Number of missing values in metabolomics does not match description."
    )
    
    return(list(
      met = dd,
      loq = loq,
      cdesc = dd_cdesc
    ))
  }) # End loop read data
  
  # Merged data
  metabs <- purrr::reduce(lapply(tbls, "[[", "met"),
                          .f = dplyr::bind_rows
  )
  cdescs <- purrr::reduce(lapply(tbls, "[[", "cdesc"),
                          .f = dplyr::bind_rows
  )
  loqs <- tbls[[1]]$loq
  
  # Some sanity checks
  assertthat::assert_that(
    nrow(metabs) == length(unique(metabs[[params_dat$variables$identifier]])),
    msg = "Number of unique subjects is different in metabolome data."
  )
  assertthat::assert_that(
    nrow(cdescs) == length(unique(cdescs[[params_dat$variables$identifier]])),
    msg = "Number of unique subjects is different in description data."
  )
  l_desc_id <- length(unique(cdescs[[params_dat$variables$identifier]]))
  l_dat_id <- length(unique(metabs[[params_dat$variables$identifier]]))
  assertthat::assert_that(
    l_desc_id == l_dat_id,
    msg = "Number of unique subjects is different between metabolomics and
          description data."
  )
  assertthat::assert_that(
    nrow(metabs) == nrow(cdescs),
    msg = "Mismatched number of rows in metabolomics and description data."
  )
  assertthat::assert_that(
    ncol(metabs) == ncol(cdescs) + 1,
    msg = "Mismatched number of columns in metabolomics and description data."
  )
  
  return(list(
    metabolome = metabs,
    desc = cdescs,
    loq = loqs
  ))
} # End function load_steroids
################################################################################

#' Title
#'
#' @param which_sample
#'
#' @return
#' @export
load_cp_data <- function(which_sample) {
  params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
  
  chems <- vars_of_interest(append_to_chem = "c")$chemicals
  chems <- stringr::str_replace(
    string = chems,
    pattern = "hs_",
    replacement = "hcp_"
  )
  
  # Load dataset
  dat <- haven::read_dta(
    file = params_dat$paths$path_cp_data
  ) |>
    tidylog::filter(
      # Keep only first visit
      hcp_period_c == 1,
      # Type of sample for measuring chemicals
      hcp_sample == which_sample
    ) |>
    tidylog::select(
      helix_id,
      dplyr::all_of(chems)
    ) |>
    tidylog::rename(
      HelixID = "helix_id"
    )
  colnames(dat) <- stringr::str_replace(
    string = colnames(dat),
    pattern = "hcp_",
    replacement = "hs_"
  )
  
  return(dat)
} # End function load_cp_data
################################################################################

#' Load and clean the dataset corresponding to the HELIX data request
#'
#' @returns A named list of data and metadata.
#'
#' @export
load_dat_request <- function(load_all = FALSE) {
  params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
  paths <- params_dat$paths
  
  dat <- read.csv(paths$path_dat_request,
                  header = TRUE, stringsAsFactors = TRUE,
                  na.strings = c("NA", "null")
  ) |>
    tibble::as_tibble() |>
    tidylog::mutate(
      dplyr::across(
        !dplyr::where(is.numeric),
        \(x) stringr::str_trim(x, side = "both")
      )
    )
  dat <- dat |>
    tidylog::mutate(
      e3_cbirth = lubridate::as_date(dat$e3_cbirth),
      hs_date_neu = lubridate::as_date(dat$hs_date_neu),
      dplyr::across(
        dplyr::all_of(c(
          "e3_alcpreg_yn", "e3_alcpreg_1g", "e3_alcpreg_1gd",
          "hs_sample_c",
          "hs_tob", "hs_smk_parents", "hs_globalexp",
          "hs_fam_car", "hs_c_room",
          "hs_wrk_m", "hs_finance", "FAS_cat", "FAS_score",
          "hs_temp", "hs_noise", "hs_qual_test", "hs_rest_nth",
          "hs_mood", "hs_healthc_tday"
        )),
        \(x) factor(x)
      ),
      hs_neuro_diag = factor(hs_neuro_diag),
      hs_bf = factor(hs_bf),
      cohort = factor(cohort,
                      levels = c(
                        "BIB", "EDEN",
                        "KANC", "MOBA",
                        "RHEA", "SAB"
                      )
      ),
      dplyr::across(
        dplyr::contains(c(
          "_matrix",
          "_cdich", "_cdesc", "_mdesc",
          "_timing", "_laboratory"
        )),
        \(x) factor(x)
      ),
      h_folic_t1 = factor(h_folic_t1,
                          levels = c("Yes", "No"),
                          labels = c(1, 2)
      ),
      dplyr::across(
        dplyr::contains("_pass_smok"),
        \(x) factor(x)
      ),
      dplyr::across(
        dplyr::contains(c(
          "_psmok", "_asmokyn"
        )),
        function(x) {
          factor(x,
                 levels = c("no", "yes"),
                 labels = c(0, 1)
          )
        }
      ),
      hs_wtr_hm = factor(hs_wtr_hm,
                         levels = c(
                           "Bottled",
                           "Municipal (tap) filtered",
                           "Municipal (tap ) non-filtered",
                           "Other, specify in the next question",
                           "Don't know"
                         ),
                         labels = c(1, 2, 3, 4, 5)
      ),
      dplyr::across(
        dplyr::contains("_ethnicity"),
        \(x) factor(x)
      ),
      # Correct variable for ethnicity of the children
      # h_ethnicity_c = factor(
      #   h_ethnicity_c,
      #   levels = c(
      #     "African",
      #     "Asian",
      #     "Caucasian",
      #     "Native American",
      #     "Other",
      #     "Pakistani",
      #     "White non-European"
      #   ),
      #   labels = c(1, 2, 3, 4, 5, 6, 7)
      # ),
      e3_edum = factor(e3_edum,
                       levels = c(
                         "primary school",
                         "secondary school",
                         "university degree or higher"
                       ),
                       labels = c(0, 1, 2)
      ),
      e3_eduf = factor(e3_eduf,
                       levels = c(
                         "primary school",
                         "secondary school",
                         "university degree or higher"
                       ),
                       labels = c(0, 1, 2)
      ),
      e3_edumc = factor(e3_edumc,
                        levels = c("low", "middle", "high"),
                        labels = c(0, 1, 2)
      ),
      e3_edufc = factor(e3_edufc,
                        levels = c("low", "middle", "high"),
                        labels = c(0, 1, 2)
      ),
      e3_edupc = factor(e3_edupc,
                        levels = c("low", "middle", "high"),
                        labels = c(0, 1, 2)
      ),
      e3_ses = factor(e3_ses,
                      levels = c("low income", "medium income", "high income"),
                      labels = c(1, 2, 3)
      ),
      e3_marital = factor(e3_marital,
                          levels = c(
                            "living with the father",
                            "living alone",
                            "other situation"
                          ),
                          labels = c(0, 1, 2)
      ),
      e3_sex = factor(e3_sex,
                      levels = c("male", "female"),
                      labels = c(0, 1)
      )
    ) |>
    tidylog::rename(
      hs_dmdtp_cadj = hs_dmdtp_crawadj,
      hs_dedtp_cadj = hs_dedtp_crawadj,
      hs_dedtp_madj = hs_dedtp_mrawadj,
      hs_cotinine_cadj = hs_cotinine_crawadj
    )
  
  # Manually modify some factors that have levels w/ few subjects
  dat <- dat |>
    tidylog::mutate(
      hs_wrk_m = forcats::fct_collapse(
        hs_wrk_m,
        "Employed" = c(1),
        "Unemployed" = c(2),
        "Other" = c(3, 5, 6, 7),
        "Stay-at-home parent" = c(4),
        "Does not wish to answer" = c(8)
      ) |>
        factor(
          levels = c(
            "Employed", "Unemployed", "Other",
            "Stay-at-home parent",
            "Does not wish to answer"
          ),
          labels = c(1, 2, 3, 4, 5)
        )
    ) |>
    # Exclude subjects with not usable test
    tidylog::filter(hs_qual_test %in% c(1, 2)) |>
    tidylog::select(-hs_qual_test)
  
  which_meta <- switch(Sys.getenv("TAR_PROJECT"),
                       "rq01" = "rq1",
                       "rq1" = "rq1",
                       "rq02" = "rq2",
                       "rq2" = "rq2",
                       "rq03" = "rq3",
                       "rq3" = "rq3",
                       "rq04" = "rq4",
                       "rq4" = "rq4"
  )
  meta <- readODS::read_ods(here::here("docs", "data_request_all.ods"),
                            col_names = TRUE,
                            strings_as_factors = TRUE
  ) |>
    tibble::as_tibble() |>
    tidylog::mutate(variable = dplyr::case_when(
      variable == "hs_dedtp_crawadj" ~ "hs_dedtp_cadj",
      TRUE ~ variable
    )) |>
    tidylog::mutate(
      group = stringr::str_to_lower(group),
      tab = stringr::str_to_lower(tab),
      period = stringr::str_to_lower(period),
    )
  if (!is.null(which_meta) & load_all == FALSE) {
    meta <- meta |>
      tidylog::filter(.data[[which_meta]] == TRUE)
  }
  cols_to_change_type <- c(
    "dag", "variable",
    "description", "code", "label",
    "comments"
  )
  meta[cols_to_change_type] <- sapply(
    meta[cols_to_change_type], as.character
  )
  
  return(list(
    dat = dat,
    meta = meta
  ))
} # End function load_dat_request
################################################################################
