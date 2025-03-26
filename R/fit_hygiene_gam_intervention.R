fit_hygiene_gam_intervention <- function(
    fit_dat,
    pred_dat
){

  respondents <- fit_dat$respondents
  count <- fit_dat$count
  date <- fit_dat$date
  intervention_stage <- fit_dat$intervention_stage

  n_unique_interventions <- length(unique(intervention_stage))

  if (n_unique_interventions < 2) {
    return(NA)
  }

  date_num <- as.numeric(date - min(date))

  mgcv::gam(
    cbind(count, I(respondents - count)) ~ s(date_num) + intervention_stage,
    select = TRUE,
    family = stats::binomial,
    optimizer = c("outer","optim")
  )

}
