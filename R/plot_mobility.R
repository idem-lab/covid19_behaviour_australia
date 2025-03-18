plot_mobility <- function(
  plot_data
){

  state_long <- unique(plot_data$state_long)
  state_short <- abbreviate_states(state_long)

  plot_data |>
    ggplot(
      aes(
        x = date,
        y =fitted_trend
      )
    ) +
    geom_hline(
      yintercept = 0,
      colour = "grey80",
      linetype = 3
    ) +
    geom_vline(
      aes(xintercept = date),
      data = interventions() %>%
        filter(state == state_short),
      colour = "grey80"
    ) +
    geom_vline(
      aes(xintercept = last_date),
      colour = "grey80",
      linetype = 2
    ) +
    facet_wrap(
      ~datastream,
      ncol = 3,
      scales = "free"
    ) +
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
      colour = "gray40"
    ) +
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
    scale_y_continuous(position = "right") +
    scale_x_date(
      breaks = mobility_ticks_labels$ticks,
      labels = mobility_ticks_labels$labels,
      limits = range(mobility_fitted$date)
    ) +
    xlab("") +
    ylab("") +
    ggtitle(
      sprintf(
        "Percentage change in selected mobility datastreams up to %s, %s",
        format(last_date, format = "%B %d"),
        format(last_date, format = "%Y")
      )
    ) +
    cowplot::theme_cowplot() +
    cowplot::panel_border(remove = TRUE) +
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0, face = "bold"),
          axis.text.x = element_text(size = 10),
          axis.title.y.right = element_text(vjust = 0.5, angle = 90),
          panel.spacing = unit(1.2, "lines"))

  dpi <- 150
  ggsave(
    filename = sprintf(
      "outputs/figures/%s_datastream_model_fit_%s.png",
      this_state,
      last_date
    ),
    width = 1500 / dpi,
    height = 1250 / dpi,
    dpi = dpi,
    scale = 1.2
  )

}
