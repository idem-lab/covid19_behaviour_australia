#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mod_comp_mobility
#' @param mod_comp_microdistancing
#' @param mod_comp_hygiene
#' @param mod_comp_macrodistancing
#' @return
#' @author geryan
#' @export
compare_interventions <- function(
    mod_comp_mobility,
    mod_comp_microdistancing,
    mod_comp_hygiene,
    mod_comp_macrodistancing
  ) {


  all_deltas <- bind_rows(
    mod_comp_mobility |>
      mutate(
        group = "mobility",
        data_type = datastream,
        group_type = sprintf(
          "Mobility: %s",
          data_type
        )
      ) |>
      select(state, group, data_type, group_type, delta),
    mod_comp_microdistancing |>
      mutate(
        group = "Survey",
        data_type = "microdistancing",
        group_type = "Survey: Microdistancing"
      ) |>
      select(state, group, data_type, group_type, delta),
    mod_comp_hygiene |>
      mutate(
        group = "Survey",
        data_type = question,
        group_type = sprintf(
          "Survey: %s",
          data_type
        )
      ) |>
      select(state, group, data_type, group_type, delta),
    mod_comp_macrodistancing |>
      mutate(
        group = "Survey",
        data_type = "macrodistancing",
        group_type = "Survey: Macrodistancing"
      ) |>
      select(state, group, data_type, group_type, delta)
  )

  all_deltas |>
    pivot_wider(
      names_from = state,
      values_from = delta
    ) |>
    write_csv(file = "outputs/tables/intervention_model_comparison_deltas.csv")

  delta_plot <- all_deltas |>
    ggplot() +
    geom_tile(
      aes(
        x = state,
        y = group_type,
        fill = delta
      )
    ) +
    scale_fill_gradient2() +
    labs(
      x = "",
      y = "",
      title = expression(
        Delta~
          AICc~
          between~
          behaviour~
          models~
          with~
          and~
          without~
          intervention~
          effects
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
      ),
      axis.line = element_blank()
    )

  delta_plot

  dpi <- 200
  ggsave(
    filename = "outputs/figures/intervention_model_comparison.png",
    plot = delta_plot,
    width = 1600 / dpi,
    height = 1000 / dpi,
    dpi = dpi,
    scale = 1.2,
    bg = "white"
  )


}
