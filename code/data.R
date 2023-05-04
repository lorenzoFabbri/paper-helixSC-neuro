# Script containing functions to handle datasets.

################################################################################
# Function to load exposome object and extract relevant datasets.
################################################################################
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
}
