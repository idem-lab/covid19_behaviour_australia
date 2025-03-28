#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param macrodistancing_predictions
#' @return
#' @author geryan
#' @export
compare_macrodistancing_intervention_models <- function(macrodistancing_predictions) {

  mp <- macrodistancing_predictions |>
    select(
      state,
      delta
    )

  write_csv(
    x = mp,
    file = "outputs/tables/macrodistancing_model_comparison.csv"
  )

  mp

}
