get_microdistancing_data <- function(
  hygiene_data,
  dates
){

  latent_distancing <- readRDS("data/social_distancing_latent.RDS")

  # clip distancing to non-degenerate values
  latent_distancing_range <- range(latent_distancing$mean[!latent_distancing$mean %in% c(0,  1)])


  micro_survey <- hygiene_data |>



}
