#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param microdistancing_predictions
#' @return
#' @author geryan
#' @export
generate_microdistancing_plots <- function(
  microdistancing_data,
  microdistancing_predictions,
  microdistancing_ticks_labels
) {

  survey_distance <- microdistancing_data |>
    filter(data) |>
    select(-data)

  pred_plot <- microdistancing_predictions |>
    select(-delta) |>
    unnest(predictions)



  line_df <- pred_plot %>%
    mutate_at(
      vars(mean, ci_90_lo, ci_90_hi, ci_50_lo, ci_50_hi),
      ~ . * 100
    ) %>%
    filter(date >= as.Date("2020-03-01")) %>%
    mutate(type = "Nowcast")



  point_df <- survey_distance %>%
    group_by(state, wave_date) %>%
    summarise(
      count =  sum(count),
      respondents = sum(respondents)
    ) %>%
    ungroup() %>%
    mutate(
      proportion = count / respondents,
      percentage = proportion * 100
    ) %>%
    rename(date = wave_date) %>%
    mutate(type = "Nowcast")

  # Compute confidence intervals for the proportions for plotting. Need to fudge
  # the sample size for one survey round with 100% adherence on a small sample
  pred <- point_df %>%
    mutate(
      id = factor(row_number()),
      respondents = ifelse(respondents == count,
                           respondents + 1,
                           respondents)
    ) %>%
    glm(cbind(count, respondents - count) ~ id,
        data = .,
        family = stats::binomial) %>%
    predict(se.fit = TRUE)

  # Monte Carlo integration based on normal approximation to logit-probability
  logit_sims <- replicate(
    10000,
    rnorm(length(pred$fit),
          pred$fit,
          pred$se.fit)
  )

  p_sims <- plogis(logit_sims)
  estimate <- rowMeans(p_sims)
  cis <- t(apply(
    X = p_sims,
    MARGIN = 1,
    FUN = quantile,
    c(0.025, 0.975)
  ))

  point_df <- point_df %>%
    mutate(
      percentage = estimate * 100,
      lower = cis[, 1] * 100,
      upper = cis[, 2] * 100
    )

  # # save these fits for plotting later
  # module(line_df, point_df) %>%
  #   saveRDS("outputs/micro_plotting_data.RDS")

  base_colour <- purple()

  p <- ggplot(line_df) +

    aes(date, mean, fill = type) +

    xlab(element_blank()) +

    coord_cartesian(ylim = c(0, 100)) +
    scale_y_continuous(position = "right") +
    scale_x_date(
      breaks = microdistancing_ticks_labels$ticks,
      labels = microdistancing_ticks_labels$labels
    ) +
    scale_alpha(range = c(0, 0.5)) +
    scale_fill_manual(values = c("Nowcast" = base_colour)) +

    geom_vline(
      aes(xintercept = date),
      data = interventions(),
      colour = "grey80"
    ) +

    geom_ribbon(aes(ymin = ci_90_lo,
                    ymax = ci_90_hi),
                alpha = 0.2) +
    geom_ribbon(aes(ymin = ci_50_lo,
                    ymax = ci_50_hi),
                alpha = 0.5) +
    geom_line(aes(y = ci_90_lo),
              colour = base_colour,
              alpha = 0.8) +
    geom_line(aes(y = ci_90_hi),
              colour = base_colour,
              alpha = 0.8) +

    facet_wrap(~state, ncol = 2, scales = "free") +

    cowplot::theme_cowplot() +
    cowplot::panel_border(remove = TRUE) +
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0, face = "bold"),
          axis.title.y.right = element_text(vjust = 0.5, angle = 90),
          panel.spacing = unit(1.2, "lines"),
          axis.text.x = element_text(size = 7)) +

    # add empirical percentages
    geom_point(
      aes(date, percentage),
      data = point_df,
      size = 2,
      pch = "_"
    ) +

    geom_errorbar(
      aes(date, percentage, ymin = lower, ymax = upper),
      data = point_df,
      linewidth = 1,
      alpha = 0.2,
      width = 0
    ) +

    # and titles
    ggtitle(
      label = "Micro-distancing trend",
      subtitle = "Calibrated against self-reported adherence to physical distancing"
    ) +
    ylab("Estimate of percentage 'always' keeping 1.5m distance")

  p


  save_ggplot("microdistancing_effect.png")



  p <- ggplot(line_df) +

    aes(date, mean, fill = type) +

    xlab(element_blank()) +

    coord_cartesian(ylim = c(0, 100)) +
    scale_y_continuous(position = "right") +
    scale_x_date(
      breaks = microdistancing_ticks_labels$ticks,
      labels = microdistancing_ticks_labels$labels
    ) +
    scale_alpha(range = c(0, 0.5)) +
    scale_fill_manual(values = c("Nowcast" = base_colour)) +

    geom_vline(
      aes(xintercept = date),
      data = interventions(),
      colour = "grey80"
    ) +

    geom_ribbon(aes(ymin = ci_90_lo,
                    ymax = ci_90_hi),
                alpha = 0.2) +
    geom_ribbon(aes(ymin = ci_50_lo,
                    ymax = ci_50_hi),
                alpha = 0.5) +
    geom_line(aes(y = ci_90_lo),
              colour = base_colour,
              alpha = 0.8) +
    geom_line(aes(y = ci_90_hi),
              colour = base_colour,
              alpha = 0.8) +

    facet_wrap(~state, ncol = 2, scales = "free") +

    cowplot::theme_cowplot() +
    cowplot::panel_border(remove = TRUE) +
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0, face = "bold"),
          axis.title.y.right = element_text(vjust = 0.5, angle = 90),
          panel.spacing = unit(1.2, "lines"),
          axis.text.x = element_text(size = 7)) +
    # and titles
    ggtitle(
      label = "Micro-distancing trend",
      subtitle = "Calibrated against self-reported adherence to physical distancing"
    ) +
    ylab("Estimate of percentage 'always' keeping 1.5m distance")

  #p


  save_ggplot("microdistancing_effect_no_data_vis.png")


  p <- ggplot(line_df) +

    aes(date, mean, fill = type) +

    xlab(element_blank()) +

    coord_cartesian(ylim = c(0, 100)) +
    scale_y_continuous(position = "right") +
    scale_x_date(
      breaks = microdistancing_ticks_labels$ticks,
      labels = microdistancing_ticks_labels$labels
    ) +
    scale_alpha(range = c(0, 0.5)) +
    scale_fill_manual(values = c("Nowcast" = base_colour)) +

    geom_vline(
      aes(xintercept = date),
      data = interventions(),
      colour = "grey80"
    ) +

    geom_ribbon(aes(ymin = ci_90_lo,
                    ymax = ci_90_hi),
                alpha = 0.2) +
    geom_ribbon(aes(ymin = ci_50_lo,
                    ymax = ci_50_hi),
                alpha = 0.5) +
    geom_line(aes(y = ci_90_lo),
              colour = base_colour,
              alpha = 0.8) +
    geom_line(aes(y = ci_90_hi),
              colour = base_colour,
              alpha = 0.8) +

    facet_wrap(~state, ncol = 3, scales = "free") +

    cowplot::theme_cowplot() +
    cowplot::panel_border(remove = TRUE) +
    theme(legend.position = "none",
          strip.background = element_blank(),
          strip.text = element_text(hjust = 0, face = "bold"),
          axis.title.y.right = element_text(vjust = 0.5, angle = 90),
          panel.spacing = unit(1.2, "lines"),
          axis.text.x = element_text(size = 7)) +
    # and titles
    ggtitle(
      label = "Micro-distancing trend",
      subtitle = "Calibrated against self-reported adherence to physical distancing"
    ) +
    ylab("Estimate of percentage 'always' keeping 1.5m distance")

  #p


  ggsave(
    filename = "outputs/figures/microdistancing_effect_no_data_vis_wide.png",
    height = 1600,
    width = 2400,,
    dpi = 200,
    units = "px"
  )


}
