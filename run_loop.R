Sys.setenv(is_hpc = FALSE)
# Custom path to _targets for different research questions
path_store <- ifelse(
  Sys.getenv("is_hpc"), 
  paste0(
    "/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/",
    "data/data_paper3/_targets/_targetsRQ"
  ), 
  "~/mounts/rstudioserver/data/data_paper3/_targets/_targetsRQ"
)
Sys.setenv(path_store = path_store)

for (rq in c("1", "2", "3")) {
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
  
  # Sensitivity analyses
  Sys.setenv(TAR_PROJECT = paste0("rq", rq))
  store <- paste0(path_store, rq, "SA")
  targets::tar_make(script = "_targets_sa_rqX.R", 
                    store = store)
}

# Process results for paper
targets::tar_make(
  script = "_targets_res.R",
  store = ifelse(
    Sys.getenv("is_hpc"), 
    paste0(
      "/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/",
      "data/data_paper3/_targets/_targets_res"
    ), 
    "~/mounts/rstudioserver/data/data_paper3/_targets/_targets_res"
  )
)
