#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param plot_data
#' @param ticks_and_labels
#' @return
#' @author geryan
#' @export
plot_hygiene_question <- function(
    plot_data,
    ticks_and_labels
  ) {


  question <- plot_data$question[[1]]

  state_cols <- c(
    "darkgray", # ACT
    "cornflowerblue", # NSW
    "chocolate1", #NT
    "violetred4", # QLD
    "red1", # SA
    "darkgreen", # TAS
    "darkblue", # VIC
    "gold1" # WA
  )


  #### all questions per state

  p <- plot_data |>
    ggplot(
      aes(
        x = date,
        y = mean,
        col = state,
        fill = state
      )
    ) +
    xlab(element_blank()) +
    coord_cartesian(ylim = c(0, 1)) +
    scale_y_continuous(position = "right") +
    scale_x_date(
      breaks = hygiene_ticks_labels$ticks,
      labels = hygiene_ticks_labels$labels,
      limits = c(
        min(plot_dat$date),
        max(plot_dat$date)
      )
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
        "Trends in %s behaviour",
        tolower(question)
      ),
      y = "Porportion of responses",
      colour = "State",
      fill = "State"
    ) +
    scale_fill_manual(
      values = state_cols
    ) +
    scale_colour_manual(
      values = state_cols
    )

  p

  dpi <- 200

  ggsave(
    filename = sprintf(
      "outputs/figures/hygiene/question/%s_hygiene.png",
      question
    ),
    plot = p,
    width = 1600 / dpi,
    height = 1000 / dpi,
    dpi = dpi,
    scale = 1.2,
    bg = "white"
  )



}
