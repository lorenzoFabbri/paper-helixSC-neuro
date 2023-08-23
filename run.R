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

future::plan(future::multisession, 
             workers = 2)
progressr::with_progress({
  p <- progressr::progressor(steps = length(c("2", "3")))
  
  furrr::future_map(c("2", "3"), function(rq, p) {
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
  }, 
  .options = furrr::furrr_options(
    seed = TRUE
  ), 
  p = p) # End loop over RQs
}) # End progress bar
future::plan(future::sequential)

#targets::tar_destroy(destroy = "all", store = store)
#targets::tar_load_everything(store = store)
