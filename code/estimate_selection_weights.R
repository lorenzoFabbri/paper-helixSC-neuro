estimate_selection_weights <- function(dat, idxs_selected, id_var,
                                       filter_out,
                                       list_covars,
                                       method, method_args,
                                       trim_weights, threshold_trim) {
  # Add variable indicating whether subject was in fact selected
  dat <- dat |>
    tidylog::mutate(
      selected = ifelse(
        .data[[id_var]] %in% idxs_selected,
        1, 0
      ),
      selected = as.integer(selected)
    )
  ## Eventually filter out observations
  if (length(filter_out) > 0) {
    dat <- dat |>
      tidylog::filter(
        ! .data[[names(filter_out)]] %in% filter_out[[1]]
      )
  }
  
  assertthat::assert_that(
    sum(as.integer(dat$selected)) == length(
      base::intersect(dat[[id_var]], idxs_selected)
    ),
    msg = "Number of subjects selected does not match."
  )
  
  # Estimate probability of selection given covariates
  sel_weights <- myphd::estimate_weights(
    dat = dat |>
      tidylog::select(
        selected,
        dplyr::all_of(list_covars)
      ),
    exposure = "selected",
    covariates = list_covars,
    id_var = id_var,
    method = method,
    method_args = method_args
  )
  if (trim_weights == TRUE) {
    sel_weights <- WeightIt::trim(
      sel_weights$weights,
      at = threshold_trim,
      lower = TRUE
    )
  } else {
    sel_weights <- sel_weights$weights
  }
  
  # Check balance
  
  # Tidy return
  ret <- tibble::tibble(
    {{ id_var }} := dat[[id_var]],
    selected = dat$selected,
    selection_weights = sel_weights$weights
  )
  
  return(ret)
}
