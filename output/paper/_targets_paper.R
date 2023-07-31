targets::tar_option_set(
  format = "qs"
)
options(clustermq.scheduler = "future")

list(
  tarchetypes::tar_quarto(
    name = figures, 
    path = "figures.qmd", 
    quiet = FALSE
  ), # End target manuscript
  ##############################################################################
  tarchetypes::tar_quarto(
    name = tables, 
    path = "tables.qmd", 
    quiet = FALSE
  ), # End target manuscript
  ##############################################################################
  tarchetypes::tar_quarto(
    name = manuscript, 
    path = "manuscript.qmd", 
    quiet = FALSE
  ), # End target manuscript
  ##############################################################################
  tarchetypes::tar_quarto(
    name = si, 
    path = "si.qmd", 
    quiet = FALSE
  ) # End target manuscript
  ##############################################################################
)
