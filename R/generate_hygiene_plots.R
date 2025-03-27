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

  hygiene_cols <- c(
    "magenta", # cough
    "darkorchid", # face covering
    "slateblue", # hand washing
    "salmon" # physical contact
  )

  plot_dat <- hygiene_predictions |>
    select(-delta) |>
    unnest(predictions)

  intervention_lines <- inner_join(
    x = plot_dat |>
      select(state, date) |>
      distinct(),
    y = interventions()
  )


#### all plots in one by state

  p_all <- plot_dat |>
    ggplot(
      aes(
        x = date,
        y = mean,
        col = question,
        fill = question
      )
    ) +
    # geom_line(
    #   aes(
    #     x = date,
    #     y = mean,
    #     col = question
    #   )
    # ) +
    # facet_wrap(
    #   ~state,
    #   ncol = 2
    # ) +
    xlab(element_blank()) +

    coord_cartesian(ylim = c(0, 1)) +
    scale_y_continuous(position = "right") +
    scale_x_date(
      breaks = hygiene_ticks_labels$ticks,
      labels = hygiene_ticks_labels$labels
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
      title = "Hygiene behaviour trends",
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

  save_ggplot(filename = "hygiene_all.png")

  p_all <- plot_dat |>
    ggplot(
      aes(
        x = date,
        y = mean,
        col = question,
        fill = question
      )
    ) +
    # geom_line(
    #   aes(
    #     x = date,
    #     y = mean,
    #     col = question
    #   )
    # ) +
    # facet_wrap(
    #   ~state,
    #   ncol = 2
    # ) +
    xlab(element_blank()) +

    coord_cartesian(ylim = c(0, 1)) +
    scale_y_continuous(position = "right") +
    scale_x_date(
      breaks = hygiene_ticks_labels$ticks,
      labels = hygiene_ticks_labels$labels
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
      title = "Hygiene behaviour trends",
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

  save_ggplot(filename = "hygiene_all.png")


}
