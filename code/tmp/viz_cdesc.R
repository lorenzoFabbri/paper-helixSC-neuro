chemicals <- c(
  "hs_mep_", 
  "hs_mibp_", 
  "hs_mnbp_", 
  "hs_mbzp_", 
  "hs_mehp_", 
  "hs_mehhp_", 
  "hs_meohp_", 
  "hs_mecpp_", 
  "hs_ohminp_", 
  "hs_oxominp_", 
  "hs_mepa_", 
  "hs_etpa_", 
  "hs_prpa_", 
  "hs_bpa_", 
  "hs_bupa_", 
  "hs_oxbe_", 
  "hs_trcs_", 
  "hs_dmp_", 
  "hs_dmtp_", 
  "hs_dmdtp_", 
  "hs_dep_", 
  "hs_detp_", 
  "hs_dedtp_"
) # End list of chemicals
chemicals <- paste0(chemicals, "cdesc")

df <- load_dat_request()
er <- df$dat |> dplyr::select(cohort,
                              dplyr::all_of(chemicals))
erl <- er |>
  tidyr::pivot_longer(cols = -cohort) |>
  dplyr::count(cohort, name, value)
erl$value <- factor(erl$value, levels = c(1,2,3))

erl |> ggplot(aes(name, n)) +
  geom_bar(stat = "identity",
           aes(fill = value),
           position = "dodge") +
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 0.5,
    hjust = 0.5
  )) +
  facet_grid(cohort~.)

frqs <- er |>
  tidyr::pivot_longer(cols = -cohort) |>
  dplyr::group_by(cohort, name, value) |>
  dplyr::summarise(n = dplyr::n()) |>
  dplyr::group_by(cohort, name) |>
  dplyr::mutate(f = n / sum(n) * 100) |>
  dplyr::select(-n)

joined <- dplyr::full_join(erl, frqs, 
                 by = c("cohort", "name", "value"))
joined |> ggplot(aes(name, f)) +
  geom_bar(stat = "identity",
           aes(fill = value),
           position = "dodge") +
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 0.5,
    hjust = 0.5
  )) +
  facet_grid(cohort~., scales = "free")
