Sys.setenv(is_hpc = TRUE)

# Custom path to _targets for different research questions
path_store <- ifelse(
  Sys.getenv("is_hpc"), 
  "/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/_targets/_targetsRQ", 
  "~/mounts/rstudioserver/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/_targets/_targetsRQ"
)
Sys.setenv(path_store = path_store)

# Path to store results
path_store_res <- ifelse(
  Sys.getenv("is_hpc"), 
  "/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/results/", 
  "~/mounts/rstudioserver/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/results/"
)
Sys.setenv(path_store_res = path_store_res)
################################################################################

rq <- "2"
Sys.setenv(TAR_PROJECT = paste0("rq0", rq))
store <- paste0(path_store, paste("0", rq))
targets::tar_make(script = "_targets_rq0X.R", 
                  store = store)
Sys.setenv(TAR_PROJECT = paste0("rq", rq))
store <- paste0(path_store, rq)
targets::tar_make(script = "_targets_rqX.R", 
                  store = store)
