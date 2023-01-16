# Load packages required to define the pipeline
library(targets)
library(tarchetypes)
source("code/")

# Set target options
targets::tar_option_set(
  packages = c(""), 
  format = "qs"
)
options(clustermq.scheduler = "multicore")

# Run the R scripts in the code/ folder with custom functions
targets::tar_source()

list(
  targets::tar_target(
    name = data, 
    command = ""
  ), 
  targets::tar_target(
    name = model, 
    command = ""
  )
)
