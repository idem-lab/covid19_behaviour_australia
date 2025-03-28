fit_macrodistancing_gam_intervention <- function(
    fit_dat,
    pred_dat
){

  intervention_stage <- fit_dat$intervention_stage

  if (all(is.na(intervention_stage))) {
    return(NA)
  }

  mgcv::gam(
    data = fit_dat,
    formula = contact_num ~ #
      # smooth variations in mobility
      s(date_num, k = -1, bs="tp") +

      # step changes around intervention impositions
      intervention_stage +

      # # random effect on holidays (different for each holiday, but shrunk
      # # to an average holiday effect which used to predict into future)
      is_a_holiday +
      #s(holiday, bs = "re") +
      #
      # # constant effect for school holidays
      is_a_school_holiday - 1, #+

    # day of the week effect
    #dow, #+
    #Day of year effect
    #s(doy, bs = "cc",k=50),
    select = TRUE,
    #,
    family = mgcv::nb(),#gaussian(discrete_lognormal_for_gam()),#stats::poisson()
    gamma = 0.2,
    #optimizer = "perf",
    control = list(irls.reg=1, maxit = 500)
  )

}
