fit_mobility_models <- function(
    mobility_data,
    dates = NULL
  ){

  if(is.null(dates)){
    first_date <- min(mobility_data$date)
    last_date <- max(mobility_data$date)
  } else {
    first_date <- min(dates)
    last_date <- max(dates)
  }


  mobility_data |>
    mutate(
      state = abbreviate_states(state)
    ) |>
    filter(
      !is.na(trend)
    ) |>
    group_by(state, datastream) |>
    nest() |>
    mutate(
      model_df = map2(
        .x = data,
        .y = state,
        .f = prepare_mobility_model_data,
        min_date = first_date,
        max_date = last_date
      ),
      model_with_int = map(
        .x = model_df,
        .f = fit_mobility_with_int
      )
    )

}
