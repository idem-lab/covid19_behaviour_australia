#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param hygiene_data
#' @param question
#' @return
#' @author geryan
#' @export
get_question_data <- function(
    hygiene_data,
    question
  ) {

  hygiene_data |>
    filter(question == question)

}
