#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param macrodistancing_data
#' @return
#' @author geryan
#' @export
fit_predict_macrodistancing <- function(
    macrodistancing_data
  ) {

  contact_data <- macrodistancing_data$contacts |>
    select(date, state, contact_num, wave_date) |>
    mutate(
      contact_num = ifelse(contact_num > 99, 99, contact_num),
      dow = wday(date)
    )

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




  intervention_steps <- interventions(end_dates = TRUE) |>
    filter(
      date <= max(macrodistancing_data$location_change_trends$date),
      date >= min(macrodistancing_data$contacts$date)
    ) %>%
    # no survey data from during the TAS lockdown in these dates so not possible
    # to fit effect of this lockdown, and therefore excluding this intervention
    filter(!(state == "TAS" & date >= "2021-10-16" & date <= "2021-10-19")) %>%
    # no contact data for VIC in this period
    filter(!(state == "VIC" & date >= "2020-06-30" & date <= "2020-07-07")) |>
    mutate(
      intervention_id = paste0(
        "intervention_",
        match(date, unique(date))
      )
    ) %>%
    group_by(intervention_id, state) %>%
    do(
      tibble(
        date = seq(
          min(macrodistancing_data$location_change_trends$date),
          max(macrodistancing_data$location_change_trends$date),
          by = 1
        ),
        intervention_effect = as.numeric(
          seq(
            min(macrodistancing_data$location_change_trends$date),
            max(macrodistancing_data$location_change_trends$date),
            by = 1
          ) >= .$date
        )
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


  df_fit <- macrodistancing_data$location_change_trends %>%
    select(state,date) %>%
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
    left_join(
      contact_data,
      by = c("state", "date")
    ) %>%
    mutate(
      holiday = replace_na(holiday, "none"),
      is_a_holiday = holiday != "none",
      is_a_school_holiday = !is.na(school_holiday),
      holiday = factor(holiday),
      date_num = as.numeric(date - min(date)),
      dow = lubridate::wday(date, label = TRUE)
    ) %>%
    select(-school_holiday) %>%
    filter(!(is.na(contact_num))) %>%
    arrange(state) %>%
    nest(
      fit_dat = c(
        date,
        date_num,
        contact_num,
        intervention_stage,
        is_a_holiday,
        holiday,
        is_a_school_holiday,
        dow,
        wave_date
      )
    )

  df_pred <- macrodistancing_data$location_change_trends %>%
    select(state,date) %>%
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
    left_join(
      contact_data %>%
        select(state, date, wave_date),
      by = c("state", "date")
    ) %>%
    distinct(
      date,
      state,
      .keep_all = TRUE
    ) %>%
    mutate(
      holiday = replace_na(holiday, "none"),
      is_a_holiday = holiday != "none",
      is_a_school_holiday = !is.na(school_holiday),
      holiday = factor(holiday),
      date_num = as.numeric(date - min(date)),
      dow = lubridate::wday(date, label = TRUE)
    ) %>%
    arrange(state) %>%
    select(-school_holiday) %>%
    nest(
      pred_dat = c(
        date,
        date_num,
        intervention_stage,
        is_a_holiday,
        holiday,
        is_a_school_holiday,
        dow,
        wave_date
      )
    )


  df_macro <- full_join(
    df_fit,
    df_pred,
    by = "state"
  )

  df_macro |>
    rowwise() |>
    mutate(
      model_with_int = fit_macrodistancing_gam_intervention(
        fit_dat
      ) |>
        list(),
      model_no_int = fit_macrodistancing_gam_no_intervention(
        fit_dat
      ) |>
        list()
    ) |>
    ungroup() |>
    mutate(
      aicc_int = map(
        .x = model_with_int,
        .f = function(x){
          if(is.logical(x)){
            NA
          } else {
            AICc(x)
          }
        }
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
      predictions = predict_macrodistancing_gam(
        pred_dat = pred_dat,
        m1 = model_with_int,
        m2 = model_no_int
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


