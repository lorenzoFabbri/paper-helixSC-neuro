Sys.setenv(is_hpc = TRUE)

# Custom path to _targets for different research questions
path_store <- ifelse(
  Sys.getenv("is_hpc"), 
  "~/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/_targets/_targetsRQ", 
  "~/mounts/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/_targets/_targetsRQ"
)
Sys.setenv(path_store = path_store)

# Path to store results
path_store_res <- ifelse(
  Sys.getenv("is_hpc"), 
  "~/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/results/", 
  "~/mounts/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/results/"
)
Sys.setenv(path_store_res = path_store_res)

################################################################################
Sys.setenv(TAR_PROJECT = "rq01")
store <- paste0(path_store, "01")
targets::tar_make(script = "_targets_rq01.R", 
                  store = store)
targets::tar_load_everything(store = store)

################################################################################
Sys.setenv(TAR_PROJECT = "rq1")
store <- paste0(path_store, "1")
targets::tar_make(script = "_targets_rq1.R", 
                  store = store)
targets::tar_load_everything(store = store)
#targets::tar_destroy(destroy = "all", store = store)
