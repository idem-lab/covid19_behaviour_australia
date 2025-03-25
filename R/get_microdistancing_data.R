get_microdistancing_data <- function(
  hygiene_data,
  pred_dates
){

  latent_distancing <- readRDS("data/social_distancing_latent.RDS")

  # clip distancing to non-degenerate values
  latent_distancing_range <- range(latent_distancing$mean[!latent_distancing$mean %in% c(0,  1)])

  # mimic old function of right join
  old_right_join <- function(x, y, ...) {
    left_join(y, x, ...)
  }

  # get data to predict to
  pred_data <- latent_distancing %>%
    rename(distancing = mean) %>%
    mutate(
      distancing = pmax(distancing, latent_distancing_range[1]),
      distancing = pmin(distancing, latent_distancing_range[2])
    ) %>%
    select(date, distancing) %>%
    old_right_join(
      expand_grid(
        date = pred_dates,
        state = unique(hygiene_data$state)
      )
    ) %>%
    mutate(
      distancing = case_when(
        is.na(distancing) & date < min(pred_dates) ~ 0,
        is.na(distancing) & date >= min(pred_dates) ~ 1,
        TRUE ~ distancing
      )
    ) %>%
    mutate(
      state_id = match(state, unique(state)),
      time = as.numeric(date - interventions("national")$date[3]),
      time = time / max(time)
    ) %>%
    arrange(state, date)

  # subset to 1.5m question and add data for modelling
  hygiene_data %>%
    filter(question == "1.5m compliance") |>
    mutate(data = TRUE) |>
    full_join(
      y = pred_data
    ) |>
    mutate(data = ifelse(is.na(data), FALSE, data)) |>
    arrange(state, date)

}


