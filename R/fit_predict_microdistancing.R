fit_predict_microdistancing <- function (
    microdistancing_data
) {

  survey_distance <- microdistancing_data |>
    filter(data) |>
    select(-data)

  min_data_date <- min(survey_distance$date)
  max_data_date <- max(survey_distance$date)

  all_dates <- seq(min_data_date, max_data_date, by = 1)


  intervention_steps <- interventions(
    end_dates = TRUE#,
    # exclude_after = "2021-10-21"
  ) %>%
    filter(date <= max_data_date) %>%
    # no survey data from during the TAS lockdown in these dates so not possible
    # to fit effect of this lockdown, and therefore excluding this intervention
    filter(!(state == "TAS" & date >= "2021-10-16" & date <= "2021-10-19")) %>%
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


  min_intervention_stage <- intervention_steps %>%
    filter(date == min_data_date) %>%
    dplyr::rename(min_intervention_stage = intervention_stage) %>%
    dplyr::select(-date)

  max_intervention_stage <- intervention_steps %>%
    filter(date == max_data_date) %>%
    dplyr::rename(max_intervention_stage = intervention_stage) %>%
    dplyr::select(-date)

  df_fit <- survey_distance %>%
    left_join(
      intervention_steps,
      by = c("state", "date")
    )%>%
    dplyr::select(
      state,
      date,
      count,
      respondents,
      intervention_stage,
      distancing
    ) %>%
    nest(
      fit_dat = c(
        date,
        count,
        respondents,
        intervention_stage,
        distancing
      )
    )


  df_pred <- microdistancing_data %>%
    filter(date %in% all_dates) |>
    select(date, state, distancing, state_id, time) |>
    left_join(
      intervention_steps,
      by = c("state", "date")
    ) %>%
    left_join(
      min_intervention_stage,
      by = "state"
    ) %>%
    left_join(
      max_intervention_stage,
      by = "state"
    ) %>%
    mutate(
      intervention_stage = case_when(
        is.na(intervention_stage) & date < min_data_date ~ min_intervention_stage,
        is.na(intervention_stage) & date > max_data_date ~ max_intervention_stage,
        state == "VIC" & intervention_stage == 4 ~  factor(5, levels = levels(intervention_stage)),
        TRUE ~ intervention_stage
      )
    ) %>%
    dplyr::select(
      state,
      date,
      intervention_stage,
      distancing
    ) %>%
    nest(
      pred_dat = c(
        date,
        intervention_stage,
        distancing
      )
    )

  df_mic <- full_join(
    df_fit,
    df_pred,
    by = "state"
  )


  df_mic |>
    rowwise() |>
    mutate(
      model_with_int = fit_microdistancing_gam_intervention(
        fit_dat,
        pred_dat
      ) |>
        list(),
      model_no_int = fit_microdistancing_gam_no_intervention(
        fit_dat,
        pred_dat
      ) |>
        list()
    ) |>
    ungroup() |>
    mutate(
      aicc_int = map(
        .x = model_with_int,
        .f = AICc
      ) |>
        unlist(),
      aicc_no_int = map(
        .x = model_no_int,
        .f = AICc
      ) |>
        unlist()
    ) |>
    rowwise() |>
    mutate(
      predictions = predict_microdistancing_gam(
        pred_dat = pred_dat,
        m = model_with_int
      ) |>
        list()
    ) |>
    ungroup() |>
    mutate(
      delta = aicc_no_int - aicc_int
    ) |>
    select(
      -fit_dat,
      -pred_dat,
      - model_with_int,
      - model_no_int,
      - aicc_int,
      - aicc_no_int
    )

}
