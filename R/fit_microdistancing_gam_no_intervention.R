fit_microdistancing_gam_no_intervention <- function(
    fit_dat,
    pred_dat
){

  respondents <- fit_dat$respondents
  count <- fit_dat$count
  date <- fit_dat$date
  date_num <- as.numeric(date - min(date))

  m <- mgcv::gam(
    cbind(count, I(respondents - count)) ~ s(date_num),
    select = TRUE,
    family = stats::binomial,
    optimizer = c("outer","optim")
  )

}
