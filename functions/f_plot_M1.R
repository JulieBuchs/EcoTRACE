f_plot_M1 <- function(myFit, myFocalPredictor) {
  mb <- modelbased::estimate_means(
    model = myFit,
    by = myFocalPredictor,
    backend = "emmeans"
  ) |>
    tibble::as_tibble()
  p <- mb |>
    ggplot2::ggplot(ggplot2::aes(
      x = !!rlang::sym(myFocalPredictor),
      y = !!rlang::sym(colnames(mb)[2]),
      ymin = CI_low,
      ymax = CI_high
    )) +
    ggplot2::geom_pointrange(ggplot2::aes(
      color = !!rlang::sym(myFocalPredictor)
    )) +
    ggplot2::scale_color_manual(values = c(Rep = "red", Dem = "dodgerblue")) +
    ggplot2::theme_bw()
  return(p)
}
