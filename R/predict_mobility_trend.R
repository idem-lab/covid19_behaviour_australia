predict_mobility_trend <- function(
    df,
    m,
    state,
    min_pred_date,
    max_pred_date,
    min_data_date,
    max_data_date
) {


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
    filter(date <= max_data_date) %>%
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


  # compute mean and standard deviation of Gaussian observation model for fitted data
  fit <- mgcv::predict.gam(m, se.fit = TRUE)
  fit$sd <- sqrt(var(residuals(m)) + fit$se.fit ^ 2)

  df_fitted <- df %>%
    # predict with fitted model (and get 90% CIs)
    mutate(
      fitted_trend = fit$fit,
      fitted_trend_upper = fit$fit + fit$sd * qnorm(0.025),
      fitted_trend_lower = fit$fit + fit$sd * qnorm(0.975),
    )

  # predict each date, *averaging over the weekday effect for each date*
  pred_df <- expand_grid(
    state,
    dow = unique(df$dow),
    date = all_dates,
  ) %>%
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
      # remove any named holidays not in the training data
      holiday = case_when(
        holiday %in% unique(df$holiday) ~ holiday,
        TRUE ~ "none"
      ),
      holiday = factor(holiday),
      is_a_school_holiday = !is.na(school_holiday),
      date_num = as.numeric(date - min_pred_date),
      # clamp the smooth part of the prediction at both ends
      date_num = pmax(date_num, min_data_date - min_pred_date),
      date_num = pmin(date_num, max_data_date - min_pred_date)
    )

  # predict trends under these conditions, and average over day of the week
  pred_df %>%
    mutate(
      predicted_trend = predict(m, newdata = pred_df)
    ) %>%
    group_by(
      state, date
    ) %>%
    summarise(
      predicted_trend = mean(predicted_trend),
      .groups = "drop"
    ) %>%
    group_by(
      state
    ) %>%
    # smooth fitted curve over days of the week and holidays
    mutate(
      predicted_trend = slider::slide_dbl(
        predicted_trend,
        gaussian_smooth,
        na.rm = TRUE,
        sd = 2.8,
        .before = 5,
        .after = 5
      )
    ) %>%
    ungroup() %>%
    left_join(
      df_fitted %>%
        select(
          state, date,
          trend,
          fitted_trend,
          fitted_trend_lower,
          fitted_trend_upper
        ),
      by = c("state", "date")
    )

}
