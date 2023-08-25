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
  datasets <- c("urine_bib", "urine_inma_rhea_eden_kanc", "urine_moba")
  datasets <- paste0(datasets, ".xlsx")
  
  problematic_ids <- c("KAN1292", "KAN147", "KAN315", "KAN918") |>
    paste(collapse = "|")
  
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
    
    # Problematic IDs
    dd <- dd |>
      tidylog::filter(!grepl(problematic_ids, HelixID))
    if (x == "urine_inma_rhea_eden_kanc.xlsx") {
      kan <- readxl::read_xlsx(paste0(steroids, 
                                      "data_anomalies_kan.xlsx"), 
                               sheet = 1, 
                               col_names = TRUE, trim_ws = TRUE) |>
        tibble::as_tibble()
      kan <- janitor::clean_names(kan, case = "none")
      colnames(kan)[[1]] <- params_dat$variables$identifier
      
      dd <- data.table::rbindlist(list(dd, kan), 
                                  use.names = TRUE) |>
        tibble::as_tibble()
    }
    
    # Tidy names
    dd <- dd |>
      tidylog::mutate(HelixID = stringr::str_trim(HelixID, side = "both")) |>
      dplyr::rowwise() |>
      tidylog::mutate(HelixID = dplyr::case_when(
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
      tidylog::mutate(HelixID = stringr::str_replace(HelixID, "EDP", "EDE")) |>
      tidylog::mutate(HelixID = ifelse(HelixID == "SAB5501", 
                                       "SAB550", HelixID))
    colnames(dd) <- gsub(
      pattern = "_", 
      replacement = "", 
      x = colnames(dd)
    )
    
    # LOQ information
    loq <- readxl::read_xlsx(paste0(steroids, x), 
                             sheet = 2, 
                             col_names = TRUE, trim_ws = TRUE) |>
      tibble::as_tibble()
    colnames(loq) <- c("metabolite", "loq")
    loq$metabolite <- janitor::make_clean_names(loq$metabolite, 
                                                case = "none")
    loq <- loq |>
      tidylog::mutate(metabolite = stringr::str_replace_all(
        metabolite, "_", ""
      ))
    ############################################################################
    
    # Cleaning
    cols <- setdiff(colnames(dd), 
                    params_dat$variables$identifier)
    ## Create new dataset for `_cdesc`
    dd_cdesc <- dd
    dd_cdesc <- dd_cdesc |>
      tidylog::mutate(dplyr::across(
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
    dd_cdesc <- tidylog::rename_with(dd_cdesc, 
                                     ~ paste0(.x, "_cdesc", recycle0 = TRUE), 
                                     !dplyr::starts_with(params_dat$variables$identifier))
    
    ## Remove unwanted columns and change to numeric
    unwanted_cols <- c("AED")
    dd <- dd |>
      tidylog::select(-dplyr::all_of(unwanted_cols)) |>
      tidylog::mutate(
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
      tidylog::select(-dplyr::all_of(paste0(unwanted_cols, "_cdesc"))) |>
      tidylog::mutate(dplyr::across(
        dplyr::where(is.numeric), 
        as.factor
      ))
    loq <- loq |>
      tidylog::filter(!metabolite %in% unwanted_cols)
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
    ncol(metabs) == ncol(cdescs), 
    msg = "Mismatched number of columns in metabolomics and description data."
  )
  
  return(list(
    metabolome = metabs, 
    desc = cdescs, 
    loq = loqs
  ))
} # End function load_steroids
################################################################################

#' Load and clean the dataset corresponding to the HELIX data request
#'
#' @returns A named list of data and metadata.
#'
#' @export
load_dat_request <- function() {
  params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
  paths <- params_dat$paths
  
  dat <- read.csv(paths$path_dat_request, 
                  header = TRUE, stringsAsFactors = TRUE, 
                  na.strings = c("NA", "null")) |>
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
          levels = c("Employed", "Unemployed", "Other", 
                     "Stay-at-home parent", 
                     "Does not wish to answer"), 
          labels = c(1, 2, 3, 4, 5)
        )
    ) |>
    # Exclude subjects with not usable test
    tidylog::filter(hs_qual_test %in% c(1, 2))
  
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
    tidylog::mutate(variable = dplyr::case_when(
      variable == "hs_dedtp_crawadj" ~ "hs_dedtp_cadj", 
      TRUE ~ variable
    )) |>
    tidylog::mutate(
      group = stringr::str_to_lower(group), 
      tab = stringr::str_to_lower(tab), 
      period = stringr::str_to_lower(period), 
    ) |>
    tidylog::filter(.data[[which_meta]] == TRUE)
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
