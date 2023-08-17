#' Load the steroid metabolomics data
#'
#' @return
#'
#' @export
load_steroids <- function() {
  # Load data
  params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
  paths <- params_dat$paths
  steroids <- paths$path_all_steroids
  datasets <- c("urine_bib", "urine_inma_rhea_eden_kanc", "urine_moba")
  datasets <- paste0(datasets, ".xlsx")
  
  # Loop over datasets
  tbls <- lapply(datasets, function(x) {
    cat(paste0("Loading dataset: ", x, "...\n"))
    
    # Metabolites
    dd <- readxl::read_xlsx(paste0(steroids, x), 
                            sheet = 1, 
                            col_names = TRUE, trim_ws = TRUE) |>
      tibble::as_tibble()
    dd <- janitor::clean_names(dd, case = "none")
    colnames(dd)[[1]] <- params_dat$variables$identifier
    # Tidy names
    dd <- dd |>
      dplyr::mutate(HelixID = stringr::str_trim(HelixID, side = "both")) |>
      dplyr::rowwise() |>
      dplyr::mutate(HelixID = dplyr::case_when(
        grepl("KAN|EDP", HelixID) ~ stringr::str_split(HelixID, "_|-")[[1]][1], 
        grepl("SAB", HelixID) ~ paste0(
          stringr::str_split(HelixID, " ")[[1]][1], 
          stringr::str_split(HelixID, " ")[[1]][2] |>
            stringr::str_remove("^0+")
        ), 
        grepl("^[[:digit:]]+", HelixID) ~ paste0("RHE", HelixID), 
        # Remove leading zeros to match HELIX dataset
        grepl("MOB", HelixID) ~ paste0(
          "MOB", 
          stringr::str_split(HelixID, "MOB")[[1]][2] |>
            stringr::str_remove("^0+")
        ), 
        TRUE ~ HelixID
      )) |>
      dplyr::mutate(HelixID = stringr::str_replace(HelixID, "EDP", "EDE"))
    
    # LOQ information
    loq <- readxl::read_xlsx(paste0(steroids, x), 
                             sheet = 2, 
                             col_names = TRUE, trim_ws = TRUE) |>
      tibble::as_tibble()
    colnames(loq) <- c("metabolite", "loq")
    loq$metabolite <- janitor::make_clean_names(loq$metabolite, 
                                                case = "none")
    ############################################################################
    
    # Cleaning
    cols <- colnames(dd) |>
      setdiff(params_dat$variables$identifier)
    ## Create new dataset for `_cdesc`
    dd_cdesc <- dd
    dd_cdesc <- dd_cdesc |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(cols), 
        \(x) dplyr::case_when(
          # Value below the limit of quantification
          x %in% c("<LOQ", "<LLOQ") ~ 2, 
          # Interference or out of range
          stringr::str_detect(x, "\\*") ~ 3, 
          # Value not detected
          x == "n.d." ~ 4, 
          # Quantifiable
          TRUE ~ 1
        )
      ), .keep = "unused")
    dd_cdesc <- dplyr::rename_with(dd_cdesc, 
                                   ~ paste0(.x, "_cdesc", recycle0 = TRUE), 
                                   !dplyr::starts_with(params_dat$variables$identifier))
    
    ## Remove unwanted columns and change to numeric
    unwanted_cols <- c("AED")
    dd <- dd |>
      dplyr::select(-dplyr::all_of(unwanted_cols)) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(setdiff(cols, unwanted_cols)), 
          \(x) dplyr::case_when(
            x %in% c("<LOQ", "<LLOQ") ~ NA, 
            stringr::str_detect(x, "\\*") ~ NA, 
            x == "n.d." ~ NA, 
            TRUE ~ x
          )
        ), 
        dplyr::across(dplyr::all_of(setdiff(cols, unwanted_cols)), 
                      as.numeric)
      )
    dd_cdesc <- dd_cdesc |>
      dplyr::select(-dplyr::all_of(paste0(unwanted_cols, "_cdesc")))
    ############################################################################
    
    return(list(
      met = dd, 
      loq = loq, 
      cdesc = dd_cdesc
    ))
  }) # End loop read data
  
  # Merged data
  metabs <- purrr::reduce(lapply(tbls, "[[", "met"), 
                          .f = dplyr::bind_rows)
  cdescs <- purrr::reduce(lapply(tbls, "[[", "cdesc"), 
                          .f = dplyr::bind_rows)
  loqs <- tbls[[1]]$loq
  
  # Some sanity checks
  assertthat::are_equal(nrow(metabs), 
                        length(unique(metabs[[params_dat$variables$identifier]])))
  assertthat::are_equal(nrow(metabs), nrow(cdescs))
  assertthat::are_equal(ncol(metabs), ncol(cdescs))
  
  return(list(
    metabolome = metabs, 
    desc = cdescs, 
    loq = loqs
  ))
} # End function load_steroids
################################################################################

#' Load and clean the dataset corresponding to the HELIX data request
#'
#' @return A named list of data and metadata. A list.
#'
#' @export
load_dat_request <- function() {
  params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
  paths <- params_dat$paths
  
  dat <- read.csv(paths$path_dat_request, 
                  header = TRUE, stringsAsFactors = TRUE, 
                  na.strings = c("NA", "null")) |>
    tibble::as_tibble()
  dat <- dat |>
    dplyr::mutate(
      e3_cbirth = lubridate::as_date(dat$e3_cbirth), 
      hs_date_neu = lubridate::as_date(dat$hs_date_neu), 
      dplyr::across(
        dplyr::all_of(c("hs_bf", 
                        "e3_alcpreg_yn", "e3_alcpreg_1g", "e3_alcpreg_1gd", 
                        "hs_sample_c", 
                        "hs_tob", "hs_smk_parents", "hs_globalexp", 
                        "hs_fam_car", "hs_c_room", 
                        "hs_wrk_m", "hs_finance", "FAS_cat", "FAS_score", 
                        "hs_neuro_diag", 
                        "hs_temp", "hs_noise", "hs_qual_test", "hs_rest_nth", 
                        "hs_mood", "hs_healthc_tday")), 
        \(x) factor(x)
      ), 
      h_bf = factor(h_bf, 
                    levels = c("Never", "Ever"), 
                    labels = c(0, 1)), 
      cohort = factor(cohort, 
                      levels = c("BIB", "EDEN", 
                                 "KANC", "MOBA", 
                                 "RHEA", "SAB")), 
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
                          labels = c(1, 2)), 
      dplyr::across(
        dplyr::contains("_pass_smok"), 
        \(x) factor(x)
      ), 
      dplyr::across(
        dplyr::contains(c(
          "_psmok", "_asmokyn"
        )), 
        function(x) {factor(x, 
                            levels = c("yes", "no"), 
                            labels = c(1, 2))}
      ), 
      hs_wtr_hm = factor(hs_wtr_hm, 
                          levels = c("Bottled", 
                                     "Municipal (tap) filtered", 
                                     "Municipal (tap ) non-filtered", 
                                     "Other, specify in the next question", 
                                     "Don't know"), 
                          labels = c(1, 2, 3, 4, 5)), 
      dplyr::across(
        dplyr::contains("_ethnicity"), 
        \(x) factor(x)
      ), 
      e3_edum = factor(e3_edum, 
                       levels = c("primary school", 
                                  "secondary school", 
                                  "university degree or higher"), 
                       labels = c(0, 1, 2)), 
      e3_eduf = factor(e3_eduf, 
                       levels = c("primary school", 
                                  "secondary school", 
                                  "university degree or higher"), 
                       labels = c(0, 1, 2)), 
      e3_edumc = factor(e3_edumc, 
                        levels = c("low", "middle", "high"), 
                        labels = c(0, 1, 2)), 
      e3_edufc = factor(e3_edufc, 
                        levels = c("low", "middle", "high"), 
                        labels = c(0, 1, 2)), 
      e3_edupc = factor(e3_edupc, 
                        levels = c("low", "middle", "high"), 
                        labels = c(0, 1, 2)), 
      e3_ses = factor(e3_ses, 
                      levels = c("low income", "medium income", "high income"), 
                      labels = c(1, 2, 3)), 
      e3_marital = factor(e3_marital, 
                          levels = c("living alone", 
                                     "living with the father", 
                                     "other situation"), 
                          labels = c(1, 0, 2)), 
      e3_sex = factor(e3_sex, 
                      levels = c("female", "male"), 
                      labels = c(1, 0))
    ) |>
    dplyr::rename(
      hs_dmdtp_cadj = hs_dmdtp_crawadj, 
      hs_dedtp_cadj = hs_dedtp_crawadj, 
      hs_dedtp_madj = hs_dedtp_mrawadj, 
      hs_cotinine_cadj = hs_cotinine_crawadj
    )
  
  # Manually modify some factors that have levels w/ few subjects
  dat <- dat |>
    dplyr::mutate(
      hs_wrk_m = forcats::fct_collapse(
        hs_wrk_m, 
        "Employed" = c(1), 
        "Unemployed" = c(2), 
        "Other" = c(3, 5, 6, 7), 
        "Stay-at-home parent" = c(4), 
        "Does not wish to answer" = c(8)
      ) |>
        factor(
          levels = c("Employed", "Unemployed", "Other", 
                     "Stay-at-home parent", 
                     "Does not wish to answer"), 
          labels = c(1, 2, 3, 4, 5)
        )
    ) |>
    # Exclude subjects with not usable test
    dplyr::filter(hs_qual_test != 3)
  
  which_meta <- switch(Sys.getenv("TAR_PROJECT"), 
                       "rq01" = "rq1", 
                       "rq1" = "rq1", 
                       "rq02" = "rq2", 
                       "rq2" = "rq2", 
                       "rq03" = "rq3", 
                       "rq3" = "rq3")
  meta <- readODS::read_ods("docs/data_request_all.ods", 
                            col_names = TRUE, 
                            strings_as_factors = TRUE) |>
    tibble::as_tibble() |>
    dplyr::mutate(variable = dplyr::case_when(
      variable == "hs_dedtp_crawadj" ~ "hs_dedtp_cadj", 
      TRUE ~ variable
    )) |>
    dplyr::mutate(
      group = stringr::str_to_lower(group), 
      tab = stringr::str_to_lower(tab), 
      period = stringr::str_to_lower(period), 
    ) |>
    dplyr::filter(.data[[which_meta]] == TRUE)
  cols_to_change_type <- c("dag", "variable", 
                           "description", "code", "label", 
                           "comments")
  meta[cols_to_change_type] <- sapply(
    meta[cols_to_change_type], as.character
  )
  
  return(list(
    dat = dat, 
    meta = meta
  ))
} # End function load_dat_request
################################################################################
