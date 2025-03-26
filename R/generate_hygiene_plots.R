#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param hygiene_data
#' @param hygiene_predictions
#' @param hygiene_ticks_labels
#' @return
#' @author geryan
#' @export
generate_hygiene_plots <- function(
    hygiene_data,
    hygiene_predictions,
    hygiene_ticks_labels
) {

  plot_dat <- hygiene_predictions |>
    select(-delta) |>
    unnest(predictions)

  plot_dat |>
    ggplot() +
    geom_line(
      aes(
        x = date,
        y = mean,
        col = question
      )
    ) +
    facet_wrap(
      ~state,
      ncol = 2
    )

}
