#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param plot_data
#' @param question
#' @param hygiene_ticks_labels
#' @return
#' @author geryan
#' @export
plot_hygiene_single <- function(
    plot_data,
    question,
    hygiene_ticks_labels
  ) {

  intervention_lines <- inner_join(
    x = plot_data |>
      select(state, date) |>
      distinct(),
    y = interventions()
  )

  state <- plot_data$state[[1]]

  # hygiene_cols <- c(
  #   "magenta", # cough
  #   "darkorchid", # face covering
  #   "slateblue", # hand washing
  #   "salmon" # physical contact
  # )

  ribbon_col <- switch(
    question,
    "Cough etiquette" = "magenta",
    "Face covering" = "darkorchid",
    "Hand washing" = "slateblue",
    "Physical contact"  = "salmon"
  )

  p <- plot_data |>
    ggplot(
      aes(
        x = date,
        y = mean
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
      alpha = 0.3,
      colour = ribbon_col,
      fill = ribbon_col
    ) +
    geom_ribbon(
      aes(
        ymin = ci_50_lo,
        ymax = ci_50_hi
      ),
      alpha = 0.6,
      colour = ribbon_col,
      fill = ribbon_col
    ) +
    cowplot::theme_cowplot() +
    cowplot::panel_border(remove = TRUE) +
    theme(
      legend.position = "none",
      strip.background = element_blank(),
      strip.text = element_text(hjust = 0, face = "bold"),
      axis.title.y.right = element_text(vjust = 0.5, angle = 90),
      panel.spacing = unit(1.2, "lines"),
      axis.text.x = element_text(size = 7)
    ) +
    # and titles
    labs(
      title = sprintf(
        "Trends in %s behaviour in %s",
        tolower(question),
        state
      ),
      y = "Porportion of responses"
    )

  # p

  dpi <- 200

  ds <-gsub(
    pattern = " ",
    replacement = "_",
    x = tolower(question)
  )

  ggsave(
    filename = sprintf(
      "outputs/figures/hygiene/single/%s_hygiene_%s.png",
      state,
      ds
    ),
    plot = p,
    width = 1600 / dpi,
    height = 1000 / dpi,
    dpi = dpi,
    scale = 1.2,
    bg = "white"
  )

}
