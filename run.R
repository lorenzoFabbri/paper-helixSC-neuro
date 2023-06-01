Sys.setenv(is_hpc = FALSE)

path_store <- ifelse(
  Sys.getenv("is_hpc"), 
  "~/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF_phd/data/data_paper3/_targets/_targetsRQ", 
  "~/mounts/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF_phd/data/data_paper3/_targets/_targetsRQ"
)

################################################################################
Sys.setenv(TAR_PROJECT = "rq01")
targets::tar_make(script = "_targets_rq01.R", 
                  store = paste0(path_store, "01"))

################################################################################
Sys.setenv(TAR_PROJECT = "rq1")
targets::tar_make(script = "_targets_rq1.R", 
                  store = paste0(path_store, "1"))
#targets::tar_destroy(destroy = "all")
