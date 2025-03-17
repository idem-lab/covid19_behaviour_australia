fit_predict_mobility <- function(
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

  mobility_data %>%
    rename(
      state_long = state,
    ) %>%
    filter(
      !is.na(state_long) &
        !is.na(trend)
    ) %>%
    mutate(
      state = abbreviate_states(state_long)
    ) %>%
    group_by(state, datastream) %>%
    do(
      predict_mobility_trend(
        .,
        min_date = first_date,
        max_date = last_date
      )
    ) %>%
    ungroup()


}
