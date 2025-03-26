predict_hygiene_gam <- function (
    pred_dat,
    m1,
    m2
) {

  pred_dat$date_num <- as.numeric(pred_dat$date - min(pred_dat$date))

  # m <- ifelse(
  #   is.logical(unlist(m1)),
  #   m2,
  #   m1
  # )

  m1_is_gam <- inherits(m1, "gam")

  if(m1_is_gam) {
    m <- m1
  } else {
    m <- m2
  }

  pred <- predict(
    object = m,
    newdata = pred_dat,
    se.fit = TRUE,
    type = "link"
  )

  quantile95 <- qnorm(0.95)
  quantile75 <- qnorm(0.75)
  ci_90_hi <- pred$fit + (quantile95 * pred$se.fit)
  ci_90_lo <- pred$fit - (quantile95 * pred$se.fit)
  ci_50_hi <- pred$fit + (quantile75 * pred$se.fit)
  ci_50_lo <- pred$fit - (quantile75 * pred$se.fit)

  fitted <- m$family$linkinv(pred$fit)
  ci_90_hi <- m$family$linkinv(ci_90_hi)
  ci_90_lo <- m$family$linkinv(ci_90_lo)
  ci_50_hi <- m$family$linkinv(ci_50_hi)
  ci_50_lo <- m$family$linkinv(ci_50_lo)


  tibble(
    date = pred_dat$date,
    mean = fitted ,
    ci_90_lo,
    ci_50_lo,
    ci_50_hi,
    ci_90_hi
  )

}
