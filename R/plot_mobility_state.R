plot_mobility_state <- function(
  plot_data,
  ticks_and_labels = NULL
){

  first_date <- min(plot_data$date)
  last_date <- max(plot_data$date)

  state_long <- unique(plot_data$state_long)
  state_short <- abbreviate_states(state_long)


  p <- plot_data |>
    ggplot(
      aes(
        x = date,
        y =fitted_trend
      )
    ) +
    # reference condition
    geom_hline(
      yintercept = 0,
      colour = "grey80",
      linetype = 3
    ) +
    # interventions
    geom_vline(
      aes(xintercept = date),
      data = interventions() %>%
        filter(state == state_short),
      colour = "grey80"
    ) +
    # end date
    # geom_vline(
    #   aes(xintercept = last_date),
    #   colour = "grey80",
    #   linetype = 2
    # ) +
    # uncertainty estimate
    geom_ribbon(
      aes(
        ymin = fitted_trend_lower,
        ymax = fitted_trend_upper
      ),
      fill = grey(0.9),
      colour = grey(0.8),
      linewidth = 0.1
    ) +
    # fitted trend
    geom_line(
      aes(date, fitted_trend),
      colour = "grey40"
    ) +
    # data
    geom_point(
      aes(date, trend),
      size = 0.2,
      col = "purple"
    ) +
    # # predicted trend
    # geom_line(
    #   aes(date, predicted_trend),
    #   size = 1
    # ) +
    coord_cartesian(
      xlim = c(as.Date("2020-03-01"), last_date)# + 7 * n_weeks_ahead)
    ) +
    #scale_y_continuous(position = "right") +
    scale_x_date(
      breaks = ticks_and_labels$ticks,
      labels = ticks_and_labels$labels,
      limits = range(plot_data$date)
    ) +
    labs(
      x = "",
      y = "",
      title = state_long,
      subtitle = sprintf(
        "Percentage change in mobility metric, from %s, %s to %s, %s",
        format(first_date, format = "%B %d"),
        format(first_date, format = "%Y"),
        format(last_date, format = "%B %d"),
        format(last_date, format = "%Y")
      )
    ) +
    cowplot::theme_cowplot() +
    cowplot::panel_border(remove = TRUE) +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      strip.text = element_text(
        hjust = 0,
        face = "bold"
      ),
      axis.text.x = element_text(size = 10),
      axis.title.y.right = element_text(
        vjust = 0.5,
        angle = 90
      ),
      panel.spacing = unit(
        1.2,
        "lines"
      )
    )+
    facet_wrap(
      ~datastream,
      ncol = 3,
      scales = "free"
    )


  dpi <- 200

  ggsave(
    filename = sprintf(
      "outputs/figures/mobility/state/%s_mobility_all.png",
      state_short
    ),
    plot = p,
    width = 1600 / dpi,
    height = 1000 / dpi,
    dpi = dpi,
    scale = 1.2,
    bg = "white"
  )

}
