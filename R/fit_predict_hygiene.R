#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param hygiene_data
#' @return
#' @author geryan
#' @export
fit_predict_hygiene <- function(
    hygiene_data
  ) {

  hygiene <- hygiene_data |>
    filter(question != "1.5m compliance")

  min_data_date <- min(hygiene$date)
  max_data_date <- max(hygiene$date)

  all_dates <- seq(min_data_date, max_data_date, by = 1)

  pred_dates <- hygiene |>
    group_by(question) |>
    summarise(
      mindate = min(date),
      maxdate = max(date)
    ) |>
    rowwise() |>
    mutate(
      date = seq.Date(
        from = mindate,
        to = maxdate,
        by = 1
      ) |>
        list()
    ) |>
    ungroup () |>
    select(-mindate, -maxdate) |>
    unnest(date) |>
    expand_grid(state = unique(hygiene$state))

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

  df_fit <- hygiene %>%
    left_join(
      intervention_steps,
      by = c("state", "date")
    )%>%
    dplyr::select(
      state,
      question,
      date,
      count,
      respondents,
      intervention_stage
    ) %>%
    nest(
      fit_dat = c(
        date,
        count,
        respondents,
        intervention_stage
      )
    )


  df_pred <- hygiene %>%
    full_join(y = pred_dates) |>
    arrange(state, question, date) |>
    select(state, question, date) |>
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
      question,
      date,
      intervention_stage
    ) %>%
    nest(
      pred_dat = c(
        date,
        intervention_stage
      )
    )

  df_hygiene <- full_join(
    df_fit,
    df_pred,
    by = c("state", "question")
  )


  df_hygiene |>
    rowwise() |>
    mutate(
      model_with_int = fit_hygiene_gam_intervention(
        fit_dat,
        pred_dat
      ) |>
        list(),
      model_no_int = fit_hygiene_gam_no_intervention(
        fit_dat,
        pred_dat
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
        #.f = AICc
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
      predictions = predict_hygiene_gam(
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
