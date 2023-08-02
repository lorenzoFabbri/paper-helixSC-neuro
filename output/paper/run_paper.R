Sys.setenv(is_hpc = TRUE)
store <- ifelse(
  Sys.getenv("is_hpc"), 
  "/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/_targets/_targets_paper", 
  "~/mounts/rstudioserver/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/_targets/_targets_paper"
)
Sys.setenv(store = store)

targets::tar_make(script = "output/paper/_targets_paper.R", 
                  store = store)
