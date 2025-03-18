generate_mobility_plots <- function (
  mobility_fit_pred#,
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
    filter(!is.na(fitted_trend))

  # individual plots for each state and datastream
  mdat |>
    group_by(state, datastream) |>
    nest() |>
    walk(
      .x = data,
      .f = plot_mobility
    )





}
