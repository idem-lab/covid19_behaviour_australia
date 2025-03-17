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
    "stringr"
  )
)

tar_source()

list(

  tar_target(
    project_dates,
    seq.Date(
      from = as.Date("2020-01-01"),
      to = as.Date("2023-12-31"),
      by = "day"
    )
  ),

  # Mobility

  tar_target(
    raw_mobility_data,
    download_google_mobility_data()
  ),

  tar_target(
    mobility_data,
    process_mobility_data(raw_mobility_data)
  ),

  tar_target(
    mobility_fit_pred,
    fit_predict_mobility(
      mobility_data,
      dates = project_dates
    )
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
