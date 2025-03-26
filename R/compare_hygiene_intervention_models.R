#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param hygiene_predictions
#' @return
#' @author geryan
#' @export
compare_hygiene_intervention_models <- function(hygiene_predictions) {

  deltas <- hygiene_predictions |>
    select(-predictions)

  deltas |>
    pivot_wider(
      names_from = state,
      values_from = delta
    ) |>
    write_csv(
      file = "outputs/tables/hygiene_model_comparison.csv"
    )

  deltas

}
