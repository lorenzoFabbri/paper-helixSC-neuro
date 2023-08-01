targets::tar_option_set(
  format = "qs"
)

list(
  tarchetypes::tar_quarto(
    name = figures, 
    path = "output/paper/figures.qmd", 
    quiet = FALSE
  ), # End target manuscript
  ##############################################################################
  tarchetypes::tar_quarto(
    name = tables, 
    path = "output/paper/tables.qmd", 
    quiet = FALSE
  ), # End target manuscript
  ##############################################################################
  tarchetypes::tar_quarto(
    name = manuscript, 
    path = "output/paper/manuscript.qmd", 
    quiet = FALSE
  ), # End target manuscript
  ##############################################################################
  tarchetypes::tar_quarto(
    name = si, 
    path = "output/paper/si.qmd", 
    quiet = FALSE
  ) # End target manuscript
  ##############################################################################
)
