process_mobility_data <- function(raw_mobility_data){

  raw_mobility_data |>
    tidyr::pivot_longer(
      ends_with("_percent_change_from_baseline"),
      names_to = "category",
      values_to = "trend"
    ) |>
    dplyr::select(
      state = sub_region_1,
      datastream = category,
      date = date,
      trend
    ) |>
    mutate(
      datastream = str_remove_all(datastream, "_percent_change_from_baseline"),
      datastream = str_replace_all(datastream, "_", " ")
    ) |>
    # here removing the aus-wide data
    filter(!is.na(state))

}
