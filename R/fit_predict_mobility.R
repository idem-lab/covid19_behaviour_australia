fit_predict_mobility <- function(
    mobility_data,
    pred_dates = NULL
  ){

  if(is.null(pred_dates)){
    min_pred_date <- min(mobility_data$date)
    max_pred_date <- max(mobility_data$date)
  } else {
    min_pred_date <- min(pred_dates)
    max_pred_date <- max(pred_dates)
  }


  mobility_data |>
    # code for testing on small portion
    # filter(state %in% c("Victoria", "Tasmania"), datastream %in% c("parks", "workplaces")) |>
    mutate(
      state = abbreviate_states(state)
    ) |>
    filter(
      !is.na(trend)
    ) |>
    group_by(state, datastream) |>
    nest() |>
    mutate(
      min_pred_date = min_pred_date,
      max_pred_date = max_pred_date,
      min_data_date = map(
        .x = data,
        .f = function(x){
          min(x$date)
        }
      ) |>
        unlist() |>
        as.Date(),
      max_data_date = map(
        .x = data,
        .f = function(x){
          max(x$date)
        }
      ) |>
        unlist() |>
        as.Date()
    ) |>
    # I have no idea why this needs to be split out and rowwised
    # for unknown reasons pmap spat an error when
    rowwise() |>
    mutate(
      model_df = prepare_mobility_model_data(
        data = data,
        state = state,
        min_pred_date = min_pred_date,
        max_pred_date = max_pred_date,
        min_data_date = min_data_date,
        max_data_date = max_data_date
      ) |>
        list()
    ) |>
    ungroup() |>
    mutate(
      model_with_int = map(
        .x = model_df,
        .f = fit_mobility_with_int,
        .progress = "Fit with interventions"
      ),
      model_no_int = map(
        .x = model_df,
        .f = fit_mobility_no_int,
        .progress = "Fit without interventions"
      ),
      aicc_int = map(
        .x = model_with_int,
        .f = AICc
      ) |>
        unlist(),
      aicc_no_int = map(
        .x = model_no_int,
        .f = AICc
      ) |>
        unlist(),
      pred_df = pmap(
        .l = list(
          df = model_df,
          m = model_with_int,
          state = state,
          min_pred_date = min_pred_date,
          max_pred_date = max_pred_date,
          min_data_date = min_data_date,
          max_data_date = max_data_date
        ),
        .f = predict_mobility_trend,
        .progress = "Predict mobility"
      )
    ) |>
    select(
      -model_df,
      -model_with_int,
      -model_no_int
    )

}
