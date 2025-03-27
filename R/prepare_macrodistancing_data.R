#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mobility_change_trends
#' @param contact_data
#' @return
#' @author geryan
#' @export
prepare_macrodistancing_data <- function(
    mobility_change_trends,
    contact_data
  ) {

  location_change_trends <- mobility_change_trends |>
    mutate(
      location = case_when(
        datastream == "residential" ~ "home",
        datastream == "transit stations" ~ "transit",
        datastream == "parks" ~ "public",
        datastream == "workplaces" ~ "work",
        datastream == "retail and recreation" ~ "retail",
        TRUE ~ "other"
      )
    ) |>
    filter(location != "other") |>
    select(-state_datastream, -datastream) |>
    pivot_wider(
      names_from = location,
      values_from = change
    ) |>
    mutate_at(
      vars(public, home, retail, transit, work),
      ~replace_na(., 1)
    ) |>
    mutate(state = abbreviate_states(state)) |>
    filter(
      date >= min(contact_data$date),
      date <= max(contact_data$date)
    )

  list(
    breaks = c(0:10, 20, 50, Inf),
    contacts = contact_data,
    location_change_trends = location_change_trends
  )


}
