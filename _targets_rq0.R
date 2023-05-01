source("code/dictionaries.R")
source("code/data.R")

# Set target options
targets::tar_option_set(
  format = "qs"
)
options(clustermq.scheduler = "multicore")

# Pipeline
list(
  targets::tar_target(
    name = data, 
    command = ""
  )
)
