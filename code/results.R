source("DAGs/dag_v2.R")
source("code/dictionaries.R")
source("code/plot.R")

is_hpc = TRUE
path_store <- ifelse(
  is_hpc, 
  "/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/_targets/_targetsRQ", 
  "~/mounts/rstudioserver/PROJECTES/HELIX_OMICS/DATA_PREVIOUS_MIGRATION/lorenzoF/data/data_paper3/_targets/_targetsRQ"
)
num_digits_est <- 3
num_digits_sig <- 3
################################################################################

# Visualize weighs and marginal effects
for (rq in c("1", "2", "3", "4")) {
  # Weighted fits
  ## Load objects
  targets::tar_load(
    dplyr::contains(paste0(
      "rq", rq, "_weighted_fits_"
    )),
    store = paste0(path_store, rq)
  )
  
  ## Tidy results
  ret <- tidy_res_weighted_fits(
    res_list = mget(ls(
      pattern = paste0("rq", rq, "_weighted_fits_*")
    )),
    rq = rq
  )
  
  ## Save results (plot and table)
  path <- "results/figures/weighted_fits/"
  name <- paste0("rq", rq, ".pdf")
  ggplot2::ggsave(
    filename = paste0(path, name),
    plot = ret$plot,
    width = 7,
    dpi = 300
  )
  path <- "results/tables/marginal_effects/"
  name <- paste0("rq", rq, ".docx")
  gt::gtsave(
    data = ret$table,
    filename = paste0(path, name)
  )
  
  ## Remove objects to save space
  rm(ret)
  rm(list = ls(pattern = paste0("rq", rq, "_weighted_fits_*")))
  ##############################################################################
  
  # Marginal effects
  ## Load objects
  targets::tar_load(
    dplyr::contains(paste0(
      "rq", rq, "_marginal_"
    )),
    store = paste0(path_store, rq)
  )
  
  ## Tidy results
  ret <- tidy_res_meffects(
    marginal_effects = mget(ls(
      pattern = paste0("rq", rq, "_marginal_*")
    )),
    rq = rq
  )
  
  ## Save results (plot and table)
  path <- "results/figures/marginal_effects/"
  name <- paste0("rq", rq, ".pdf")
  ggplot2::ggsave(
    filename = paste0(path, name),
    plot = ret$plot,
    dpi = 300,
    width = 9,
    height = 8
  )
  path <- "results/tables/marginal_effects/"
  name <- paste0("rq", rq, ".docx")
  gt::gtsave(
    data = ret$table,
    filename = paste0(path, name)
  )
  
  ## Remove objects to save space
  rm(ret)
  rm(list = ls(pattern = paste0("rq", rq, "_marginal_*")))
} # End loop tidy results
################################################################################

# Files for SM
path_sm <- "output/paper/SM/"

## DAGs
all_dags <- dags()
invisible(
  lapply(seq_along(all_dags), function(idx) {
    file_conn <- file(paste0(path_sm,
                             "dag_",
                             names(all_dags)[[idx]]))
    writeLines(all_dags[[idx]],
               con = file_conn)
    close(file_conn)
  })
) # End invisible loop DAGs
################################################################################
