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

  plot_dat <- hygiene_predictions |>
    select(-delta) |>
    unnest(predictions)

  plot_dat |>
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
    facet_wrap(
      ~state,
      ncol = 2
    ) +
    xlab(element_blank()) +

    coord_cartesian(ylim = c(0, 1)) +
    scale_y_continuous(position = "right") +
    scale_x_date(
      breaks = hygiene_ticks_labels$ticks,
      labels = hygiene_ticks_labels$labels
    ) +
    #scale_alpha(range = c(0, 0.5)) +

    geom_vline(
      aes(xintercept = date),
      data = interventions(),
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
      alpha = 0.3
    ) +
    geom_line(
      aes(y = ci_90_lo),
      alpha = 0.5
    ) +
    geom_line(
      aes(y = ci_90_hi),
      alpha = 0.5
    ) +
    cowplot::theme_cowplot() +
    cowplot::panel_border(remove = TRUE) +
    theme(
      #legend.position = "none",
      strip.background = element_blank(),
      strip.text = element_text(hjust = 0, face = "bold"),
      axis.title.y.right = element_text(vjust = 0.5, angle = 90),
      panel.spacing = unit(1.2, "lines"),
      axis.text.x = element_text(size = 7)
    ) +
    # and titles
    ggtitle(
      label = "Hygiene behaviour trends"
    ) +
    ylab("Proportion")

}
