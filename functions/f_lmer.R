f_lmer <- function(myData, myCriterion, myFixedEffects, myRandomEffects) {
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

  # Fit the linear mixed-effects model
  model <- eval(bquote(
    lme4::lmer(.(formula_obj), data = myData)
  ))

  return(model)
}
