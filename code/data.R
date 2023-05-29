#' Title
#'
#' @param dat 
#'
#' @return
#' @export
extract_cohort_id <- function(dat) {
  params_dat <- params()
  dat <- dat |>
    dplyr::mutate(cohort = substr(
      params_dat$variables$identifier, 1, 3
    )) |>
    dplyr::relocate(cohort)
  
  return(dat)
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
  dat$e3_cbirth <- lubridate::as_date(dat$e3_cbirth)
  
  meta <- readODS::read_ods("docs/data_request_relevant.ods", 
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
