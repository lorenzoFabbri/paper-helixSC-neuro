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

rqs <- c("2", "3")
for (rq in rqs) {
  # Exploratory
  Sys.setenv(TAR_PROJECT = paste0("rq0", rq))
  store <- paste0(path_store, paste0("0", rq))
  targets::tar_make(script = "_targets_rq0X.R", 
                    store = store)
  # Analyses
  Sys.setenv(TAR_PROJECT = paste0("rq", rq))
  store <- paste0(path_store, rq)
  targets::tar_make(script = "_targets_rqX.R", 
                    store = store)
} # End loop over research questions

#targets::tar_destroy(destroy = "all", store = store)
#targets::tar_load_everything(store = store)
