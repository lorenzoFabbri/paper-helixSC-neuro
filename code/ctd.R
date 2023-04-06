# Functions to load and analyse data 
# from the Comparative Toxicogenomics Database (CTD).

loadCTD <- function(path, filter_evidence) {
  #path <- "results/files/CTD_chem_diseases_1680702221550_05042023.csv"
  ctd <- read.csv(path) |>
    tibble::as_tibble()
  
  if (filter_evidence == TRUE) {
    ctd <- ctd |>
      dplyr::filter(DirectEvidence %in% c("marker/mechanism", 
                                          "therapeutic", 
                                          "marker/mechanism|therapeutic"))
  }
  
  ctd <- ctd |>
    tidyr::separate_longer_delim(DiseaseCategories, 
                                 delim = "|")
  ctd$DiseaseCategories <- factor(ctd$DiseaseCategories, 
                                  levels = sort(unique(ctd$DiseaseCategories), 
                                                decreasing = TRUE))
  
  return(ctd)
}

describeCTD <- function(dat) {
  
}

plotCTD <- function(dat) {
  plt <- dat |>
    dplyr::filter(DiseaseCategories != "") |>
    dplyr::group_by(DiseaseCategories) |>
    ggplot2::ggplot(mapping = ggplot2::aes(
      x = DiseaseCategories, 
      fill = factor(ChemicalName)
    )) +
    ggplot2::geom_bar() +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Disease categories") +
    colorspace::scale_fill_discrete_qualitative(name = "Chemicals", 
                                                palette = "Dynamic")
  
  return(plt)
}
