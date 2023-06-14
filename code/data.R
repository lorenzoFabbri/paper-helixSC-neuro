#' Title
#'
#' @param paths 
#'
#' @return
#' @export
process_steroids <- function(paths) {
  steroids <- paths$path_all_steroids
  datasets <- c("urine_bib", "urine_inma_rhea_eden_kanc", "urine_moba")
  datasets <- paste0(datasets, ".xlsx")
  params_dat <- params(is_hpc = Sys.getenv("is_hpc"))
  
  tbls <- lapply(datasets, function(x) {
    dd <- readxl::read_xlsx(paste0(steroids, x), 
                            sheet = 1, 
                            col_names = TRUE, trim_ws = TRUE) |>
      tibble::as_tibble()
    dd <- janitor::clean_names(dd, case = "none")
    
    if (x == "urine_bib.xlsx") {
      dd <- dd |>
        dplyr::rename(HelixID = HELIX2021)
      loq <- readxl::read_xlsx(paste0(steroids, x), 
                               sheet = 2, skip = 2, 
                               col_names = TRUE, trim_ws = TRUE) |>
        tibble::as_tibble()
      colnames(loq) <- c("metabolite", "loq")
      loq$metabolite <- janitor::make_clean_names(loq$metabolite, 
                                                  case = "none")
      creat <- readxl::read_xlsx(paste0(steroids, x), 
                                 sheet = 3, skip = 1, 
                                 col_names = TRUE, trim_ws = TRUE) |>
        tibble::as_tibble()
      colnames(creat) <- c(params_dat$variables$identifier, "creatinine")
    } else if (x == "urine_inma_rhea_eden_kanc.xlsx") {
      
    } else if (x == "urine_moba.xlsx") {
      
    }
    
    # Take care of values <LOQ
    cols <- colnames(dd) |>
      setdiff(params_dat$variables$identifier)
    dd <- dd |>
      dplyr::mutate(dplyr::across(dplyr::all_of(cols), 
                                  ~ ifelse(. == "<LOQ", NA, .)), 
                    dplyr::across(dplyr::all_of(cols), 
                                  as.numeric))
    dd <- dplyr::inner_join(dd, creat, 
                            by = params_dat$variables$identifier)
    dd <- dd |>
      dplyr::mutate(dplyr::across(dplyr::all_of(cols), 
                                  ~ ifelse(creatinine > params_dat$variables$creatinine_threshold & is.na(.), 
                                           handle_loq_urine(loq[loq$metabolite == dplyr::cur_column(), 
                                                                "loq"], 
                                                            params_dat$variables$strategy_loq_urine), 
                                           .)))
  }) # End read data
  
  return(tbls)
}

handle_loq_urine <- function(loq, strategy) {
  if (strategy == "div2") {
    new_val <- as.numeric(loq) / 2
  }
  
  return(new_val)
}

#' Load the dataset corresponding to the HELIX data request
#'
#' @return A named list of data and metadata. A list.
#' @export
load_dat_request <- function(paths) {
  dat <- read.csv(paths$path_dat_request, 
                  header = TRUE, stringsAsFactors = TRUE, 
                  na.strings = c("NA", "null")) |>
    tibble::as_tibble()
  dat <- dat |>
    dplyr::mutate(
      e3_cbirth = lubridate::as_date(dat$e3_cbirth), 
      hs_date_neu = lubridate::as_date(dat$hs_date_neu)
    ) |>
    dplyr::rename(
      hs_dmdtp_cadj = hs_dmdtp_crawadj, 
      hs_dedtp_cadj = hs_dedtp_crawadj, 
      hs_dedtp_madj = hs_dedtp_mrawadj, 
      hs_cotinine_cadj = hs_cotinine_crawadj
    )
  
  if (Sys.getenv("TAR_PROJECT") %in% c("rq2" ,"rq3")) {
    metab <- read.csv(paths$path_steroids, 
                      header = TRUE, stringsAsFactors = TRUE, 
                      na.strings = c("NA", "null")) |>
      tibble::as_tibble()
  }
  
  which_meta <- switch(Sys.getenv("TAR_PROJECT"), 
                       "rq01" = "_rq1", 
                       "rq1" = "_rq1")
  meta <- readODS::read_ods(paste0("docs/data_request_relevant", 
                                   which_meta, ".ods"), 
                            col_names = TRUE, strings_as_factors = TRUE) |>
    tibble::as_tibble()
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
} # End function load data request
################################################################################

#' Load exposome object
#'
#' @param path_in Path to exposome object. A string.
#'
#' @return A named list of phenotype data, meta-data, and exposures. A list.
#' @export
load_exposome <- function(path_in) {
  # Path to the object of class ExposomeSet
  obj <- get(load(path_in))
  # Covariates
  pheno <- data.table::as.data.table(obj@phenoData@data)
  # Metadata on covariates and exposures
  meta <- data.table::as.data.table(obj@featureData@data)
  # Exposures
  helix_ids <- colnames(tibble::as_tibble(obj@assayData$exp))
  expo <- data.table::as.data.table(rexposome::expos(obj))
  assertthat::are_equal(length(helix_ids), 
                        nrow(expo))
  expo[, HelixID := helix_ids]
  
  return(list(
    pheno_data = pheno, 
    meta_data = meta, 
    exposure_data = expo
  ))
} # End function load exposome object
