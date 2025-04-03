#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param macrodistancing_predictions
#' @param macrodistancing_ticks_labels
#' @return
#' @author geryan
#' @export
generate_macrodistancing_plots <- function(
    macrodistancing_predictions,
    macrodistancing_ticks_labels
) {


  plot_dat <- macrodistancing_predictions |>
    select(-delta) |>
    unnest(predictions)

  intervention_lines <- inner_join(
    x = plot_dat |>
      select(state, date) |>
      distinct(),
    y = interventions()
  )

  holiday_lines <- holiday_dates() |>
    mutate(state = abbreviate_states(state)) |>
    filter(
      date >= min(plot_dat)
    )

  ## all macrodistancing

  p_all <- plot_dat |>
    ggplot(
      aes(
        x = date,
        y = mean
      )
    ) +
    # geom_line(
    #   aes(
    #     x = date,
    #     y = mean
    #   )
    # ) +
    facet_wrap(
      ~state,
      ncol = 2
    ) +
    xlab(element_blank()) +
    #coord_cartesian(ylim = c(0, 1)) +
    scale_y_continuous(position = "right") +
    scale_x_date(
      breaks = macrodistancing_ticks_labels$ticks,
      labels = macrodistancing_ticks_labels$labels,
      limits = c(
        min(plot_dat$date),
        max(plot_dat$date)
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
      colour = purple(),
      fill = purple()
    ) +
    geom_ribbon(
      aes(
        ymin = ci_50_lo,
        ymax = ci_50_hi
      ),
      alpha = 0.6,
      colour = purple(),
      fill = purple()
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
      title = "Macrodistancing trends",
      y = "Number of contacts"
    ) +
    geom_rug(
      aes(date),
      data = holiday_lines,
      col = "green",
      size = 1,
      length = unit(0.05, "npc"),
      sides = "b",
      inherit.aes = FALSE
    )  +
    theme(
      axis.text.x = element_text(size = 8),
      axis.ticks.x = element_line(colour = macro_ticks_labels$tick.cols)
    )

  p_all

  save_ggplot(
    filename = "hygiene_all_by_state.png",
    subdir = "figures/hygiene"
  )


#### questions by each state

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
    facet_wrap(
      ~state,
      ncol = 2
    ) +
    xlab(element_blank()) +

    coord_cartesian(ylim = c(0, 1)) +
    scale_y_continuous(position = "right") +
    scale_x_date(
      breaks = macrodistancing_ticks_labels$ticks,
      labels = macrodistancing_ticks_labels$labels,
      limits = c(
        min(plot_dat$date),
        max(plot_dat$date)
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
      title = "macrodistancing behaviour trends",
      y = "Porportion of responses",
      colour = "Question",
      fill = "Question"
    ) +
    scale_fill_manual(
      values = macrodistancing_cols
    ) +
    scale_colour_manual(
      values = macrodistancing_cols
    )

  p_all

  save_ggplot(
    filename = "macrodistancing_all_by_state.png",
    subdir = "figures/macrodistancing"
  )


  #### all states by question

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

  p_all_by_q <- plot_dat |>
    ggplot(
      aes(
        x = date,
        y = mean,
        col = state,
        fill = state
      )
    ) +
    facet_wrap(
      ~question,
      ncol = 2
    ) +
    xlab(element_blank()) +
    coord_cartesian(ylim = c(0, 1)) +
    scale_y_continuous(position = "right") +
    scale_x_date(
      breaks = macrodistancing_ticks_labels$ticks,
      labels = macrodistancing_ticks_labels$labels,
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
      title = "macrodistancing behaviour trends",
      y = "Porportion of responses",
      colour = "Question",
      fill = "Question"
    ) +
    scale_fill_manual(
      values = state_cols
    ) +
    scale_colour_manual(
      values = state_cols
    )

  p_all_by_q

  save_ggplot(
    filename = "macrodistancing_all_by_question.png",
    subdir = "figures/macrodistancing"
  )

  ###
  # individual plots for each state and datastream
  plot_dat |>
    mutate(
      st = state
    ) |>
    group_by(st, question) |>
    nest() %$%
    walk2(
      .x = data,
      .y = question,
      .f = function(x, y, macrodistancing_ticks_labels){
        plot_macrodistancing_single(
          plot_data = x,
          question = y,
          macrodistancing_ticks_labels = macrodistancing_ticks_labels
        )
      },
      macrodistancing_ticks_labels,
      .progress = "macrodistancing individual plots"
    )

  # plots grouped by state
  plot_dat |>
    mutate(
      st = state
    ) |>
    group_by(st) |>
    nest() %$%
    walk(
      .x = data,
      .f = function(x, macrodistancing_ticks_labels){
        plot_macrodistancing_state(
          plot_data = x,
          macrodistancing_ticks_labels = macrodistancing_ticks_labels
        )
      },
      macrodistancing_ticks_labels,
      .progress = "macrodistancing state plots"
    )

  # plots grouped by question
  plot_dat |>
    mutate(
      q = question
    ) |>
    group_by(q) |>
    nest() %$%
    walk(
      .x = data,
      .f = function(x, macrodistancing_ticks_labels){
        plot_macrodistancing_question(
          plot_data = x,
          macrodistancing_ticks_labels = macrodistancing_ticks_labels
        )
      },
      macrodistancing_ticks_labels,
      .progress = "macrodistancing question plots"
    )

  TRUE



}
