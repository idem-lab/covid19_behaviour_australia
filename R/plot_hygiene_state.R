#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param plot_data
#' @param hygiene_ticks_labels
#' @return
#' @author geryan
#' @export
plot_hygiene_state <- function(
    plot_data,
    hygiene_ticks_labels
  ) {

  hygiene_cols <- c(
    "magenta", # cough
    "darkorchid", # face covering
    "slateblue", # hand washing
    "salmon" # physical contact
  )

  intervention_lines <- inner_join(
    x = plot_data |>
      select(state, date) |>
      distinct(),
    y = interventions()
  )


  state <- plot_data$state[[1]]


  #### all questions per state

  p <- plot_data |>
    ggplot(
      aes(
        x = date,
        y = mean,
        col = question,
        fill = question
      )
    ) +
    xlab(element_blank()) +
    coord_cartesian(ylim = c(0, 1)) +
    scale_y_continuous(position = "right") +
    scale_x_date(
      breaks = hygiene_ticks_labels$ticks,
      labels = hygiene_ticks_labels$labels,
      limits = c(
        min(plot_data$date),
        max(plot_data$date)
      )
    ) +
    geom_vline(
      aes(xintercept = date),
      data = intervention_lines,
      colour = "grey80"
    ) +
    geom_ribbon(
      aes(
        ymin = ci_90_lo,
        ymax = ci_90_hi
      ),
      alpha = 0.3
    ) +
    geom_ribbon(
      aes(
        ymin = ci_50_lo,
        ymax = ci_50_hi
      ),
      alpha = 0.6
    ) +
    cowplot::theme_cowplot() +
    cowplot::panel_border(remove = TRUE) +
    theme(
      legend.position = "bottom",
      strip.background = element_blank(),
      strip.text = element_text(hjust = 0, face = "bold"),
      axis.title.y.right = element_text(vjust = 0.5, angle = 90),
      panel.spacing = unit(1.2, "lines"),
      axis.text.x = element_text(size = 7)
    ) +
    # and titles
    labs(
      title = sprintf(
        "Hygiene behaviour trends in %s",
        state
      ),
      y = "Porportion of responses",
      colour = "Question",
      fill = "Question"
    ) +
    scale_fill_manual(
      values = hygiene_cols
    ) +
    scale_colour_manual(
      values = hygiene_cols
    )

  #p

  dpi <- 200

  ggsave(
    filename = sprintf(
      "outputs/figures/hygiene/state/%s_hygiene_all.png",
      state
    ),
    plot = p,
    width = 1600 / dpi,
    height = 1000 / dpi,
    dpi = dpi,
    scale = 1.2,
    bg = "white"
  )



}
