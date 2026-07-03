f_glmer <- function(myData, myCriterion, myFixedEffects, myRandomEffects) {
  # Construct the formula as a string
  formula_string <- paste0(
    myCriterion,
    " ~ ",
    myFixedEffects,
    " + ",
    myRandomEffects
  )

  # Convert to formula object
  formula_obj <- as.formula(formula_string)

  # Fit the generalized linear mixed-effects model with binomial family
  model <- eval(bquote(
    lme4::glmer(
      .(formula_obj),
      data = myData,
      family = binomial(link = "logit"),
      control = lme4::glmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 2e5)
      )
    )
  ))

  return(model)
}
