#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param microdistancing_predictions
#' @return
#' @author geryan
#' @export
compare_microdistancing_intervention_models <- function(microdistancing_predictions) {

  mp <- microdistancing_predictions |>
    select(
      state,
      delta
    )

  write_csv(
    x = mp,
    file = "outputs/tables/microdistancing_model_comparison.csv"
  )

  mp

}
