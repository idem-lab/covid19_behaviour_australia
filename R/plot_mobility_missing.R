plot_mobility_missing <- function(mobility_data){

  # this code steals ideas from naniar::gg_miss_span
  # but because you can only facet by one variable
  # I've gutted it and hand coded in this instead

  p <- mobility_data %>%
    mutate(
      state = abbreviate_states(state),
      date = floor_date(
        x = date,
        unit = "month"
      )
    ) |>
    group_by(datastream, state, date) |>
    summarise(
      n_missing = sum(is.na(trend)),
      .groups = "drop"
    ) |>
    mutate(
      datastream = gsub(
        pattern = "and ",
        replacement = "and\\\n",
        x = datastream
      )
    ) |>
    ggplot() +
    geom_bar(
      aes(
        x = date,
        y = n_missing
      ),
      stat = "identity"
    ) +
    #facet_grid(state ~ datastream) +
    facet_grid(datastream ~ state) +
    theme_minimal() +
    #theme(strip.text.y = element_text(size = 8)) +
    labs(
      title = "Missing mobility data",
      x = "Month",
      y = "Days of data missing per month"
    )

  dpi <- 200

  ggsave(
    filename = "outputs/figures/mobility/mobility_missing_data.png",
    plot = p,
    width = 1600 / dpi,
    height = 1000 / dpi,
    dpi = dpi,
    scale = 1.2,
    bg = "white"
  )

  TRUE

}
