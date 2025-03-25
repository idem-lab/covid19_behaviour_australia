library(targets)

# if need to install the in-house analysis package
# behaviour.change.analysis
# pak::pak("idem-lab/behaviour.change.analysis")

tar_option_set(
  packages = c(
    "readr",
    "dplyr",
    "tidyr",
    "ggplot2",
    "behaviour.change.analysis",
    "purrr",
    "stringr",
    "magrittr",
    "lubridate",
    "MuMIn"
  )
)

tar_source()

list(


  # Mobility
  tar_target(
    pred_dates,
    seq.Date(
      from = as.Date("2020-01-01"),
      to = as.Date("2023-12-31"),
      by = "day"
    )
  ),

  tar_target(
    raw_mobility_data,
    download_google_mobility_data()
  ),

  tar_target(
    mobility_data,
    process_mobility_data(raw_mobility_data)
  ),

  tar_target(
    mobility_missing_plot,
    plot_mobility_missing(mobility_data)
  ),

  tar_target(
    mobility_fit_pred,
    fit_predict_mobility(
      mobility_data,
      pred_dates
    )
  ),

  tar_target(
    mobility_intervention_model_comparison,
    compare_mobility_intervention_models(mobility_fit_pred)
  ),

  tar_target(
    mobility_ticks_labels,
    split_ticks_and_labels(
      tick_freq = "3 month",
      label_freq = "6 months",
      label_format = "%b %y",
      label_shift = FALSE,
      start_date = as.Date("2020-01-01"),
      #end_date = as.Date("2022-12-31")
      end_date = as.Date("2023-01-01")
    )
  ),

  tar_target(
    mobility_plots,
    generate_mobility_plots(
      mobility_fit_pred,
      mobility_ticks_labels
    )
  ),

  tar_target(
    mobility_change_trends,
    get_mobility_change_trends(mobility_fit_pred)
  ),

  # Questionnaire

  ## Hygiene behaviours

  tar_target(
    name = hygiene_data_file,
    command = "data/hygiene_data.csv",
    format = "file"
  ),

  tar_target(
    hygiene_data,
    read_csv(
      file = hygiene_data_file
    )
  ),

  ### microdistancing

  tar_target(
    microdistancing_data,
    get_microdistancing_data(
      hygiene_data,
      pred_dates
    )
  ),

  tar_target(
    microdistancing_predictions,
    fit_predict_microdistancing(microdistancing_data = microdistancing_data)
  ),

  tar_target(
    microdistancing_intervention_model_comparison,
    compare_microdistancing_intervention_models(microdistancing_predictions)
  ),

  tar_target(
    microdistancing_ticks_labels,
    split_ticks_and_labels(
      tick_freq = "1 month",
      label_freq = "6 months",
      label_format = "%b %y",
      label_shift = FALSE,
      start_date = as.Date("2020-01-01"),
      #end_date = as.Date("2022-12-31")
      end_date = as.Date("2024-01-01")
    )
  ),

  tar_target(
    microdistancing_plots,
    generate_microdistancing_plots(
      microdistancing_data,
      microdistancing_predictions,
      microdistancing_ticks_labels
    )
  ),

  ## face coverings
  face_covering_data,
  get_question_data(
    hygiene_data,
    question = "Face covering"
  ),

  ## Contacts / macro-distancing

  tar_target(
    name = contact_data_file,
    command = "data/contact_data.csv",
    format = "file"
  ),

  tar_target(
    contact_data,
    read_csv(
      file = contact_data_file
    )
  ),

  tar_target(
    name = pointless_end_target,
    command = "So I can put a comma on the end of everything and fuggedabadit"
  )

)
