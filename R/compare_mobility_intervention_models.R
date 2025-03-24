#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility_fit_pred
#' @return
#' @author geryan
#' @export
compare_mobility_intervention_models <- function(mobility_fit_pred) {

  deltas <- mobility_fit_pred |>
    select(
      state,
      datastream,
      aicc_int,
      aicc_no_int
    ) |>
    mutate(
      delta = aicc_no_int - aicc_int
    ) |>
    select(
      - aicc_int,
      - aicc_no_int
    )

  # plot

  # deltas |>
  #   mutate(negative = ifelse(delta < 0, TRUE, FALSE)) |>
  #   ggplot() +
  #     geom_bar(
  #       aes(
  #         x = state,
  #         y = delta,
  #         fill = negative
  #       ),
  #       stat = "identity"
  #     ) +
  #     facet_grid(
  #       rows = "datastream",
  #       scales = "free_y"
  #     )

  # table

  deltas_wide <- deltas |>
    pivot_wider(
      names_from = state,
      values_from = delta
    )

  deltas_wide |>
    write_csv(
      file = "outputs/tables/mobility_model_comparison.csv"
    )

  deltas_wide

}
