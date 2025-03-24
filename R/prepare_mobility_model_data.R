prepare_mobility_model_data <- function(
  data,
  state,
  min_pred_date,
  max_pred_date,
  min_data_date,
  max_data_date
){


  all_dates <- seq(min_pred_date, max_pred_date, by = 1)

  public_holidays <- holiday_dates() %>%
    mutate(
      state = abbreviate_states(state)
    ) %>%
    rename(
      holiday = name
    )

  school_holidays <- school_holiday_dates() %>%
    mutate(
      state = abbreviate_states(state)
    )

  # create intervention step-change covariates
  intervention_steps <- interventions(end_dates = TRUE) %>%
    mutate(
      intervention_id = paste0(
        "intervention_",
        match(date, unique(date))
      )
    ) %>%
    group_by(intervention_id, state) %>%
    do(
      tibble(
        date = all_dates,
        intervention_effect = as.numeric(all_dates >= .$date)
      )
    ) %>%
    group_by(state, date) %>%
    summarise(
      intervention_stage = sum(intervention_effect),
      .groups = "drop"
    ) %>%
    mutate(
      intervention_stage = factor(intervention_stage)
    )

  df <- data %>%
    mutate(
      state = state
    ) |>
    left_join(
      public_holidays,
      by = c("state", "date")
    ) %>%
    left_join(
      school_holidays,
      by = c("state", "date")
    ) %>%
    left_join(
      intervention_steps,
      by = c("state", "date")
    ) %>%
    mutate(
      holiday = replace_na(holiday, "none"),
      is_a_holiday = holiday != "none",
      is_a_school_holiday = !is.na(school_holiday),
      holiday = factor(holiday),
      date_num = as.numeric(date - min_pred_date),
      dow = lubridate::wday(date, label = TRUE),
      dow = as.character(dow)
    ) %>%
    filter(!is.na(trend))
}
