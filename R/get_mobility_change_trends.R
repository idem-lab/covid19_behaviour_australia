get_mobility_change_trends <- function(mobility_fit_pred){

  mobility_fit_pred |>
    select(datastream, pred_df) |>
    unnest(pred_df) |>
    mutate(
      state_long = unabbreviate_states(state)
    ) |>
    mutate(
      change = 1 + (predicted_trend / 100),
      state_datastream = paste(state_long, datastream)
    ) |>
    select(
      state_datastream,
      state = state_long,
      datastream,
      change,
      date
    )
}
