generate_mobility_plots <- function (
  mobility_fit_pred,
  ticks_and_labels
  #dates = NULL
) {

  # if(!is.null(dates)){
  #
  #   mobility_fit_pred <- mobility_fit_pred |>
  #     filter(
  #       date >= dates$min,
  #       date <= dates$max
  #     )
  #
  # }


  mdat <- mobility_fit_pred |>
    select(datastream, pred_df) |>
    unnest(pred_df) |>
    filter(!is.na(fitted_trend)) |>
    mutate(
      state_long = unabbreviate_states(state)
    )

  # individual plots for each state and datastream
  mdat |>
    group_by(state, datastream) |>
    nest() %$%
    walk2(
      .x = data,
      .y = datastream,
      .f = function(x, y, ticks_and_labels){
        plot_mobility_single(
          plot_data = x,
          datastream = y,
          ticks_and_labels = ticks_and_labels
        )
      },
      ticks_and_labels,
      .progress = "Mobility stream and state plots"
    )

  # plots grouped by state
  # individual plots for each state and datastream
  mdat |>
    group_by(state) |>
    nest() %$%
    walk(
      .x = data,
      .f = function(x, ticks_and_labels){
        plot_mobility_state(
          plot_data = x,
          ticks_and_labels = ticks_and_labels
        )
      },
      ticks_and_labels,
      .progress = "Mobility state plots"
    )

  TRUE

}
