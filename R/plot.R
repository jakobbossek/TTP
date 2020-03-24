plotTrajectories = function(x) {
  x$iter = 1:nrow(x)
  dfl = reshape2::melt(x, id.vars = "iter", value.name = "Value", variable.name = "Objective")
  g = ggplot2::ggplot(dfl, aes(x = iter, y = Value))
  g = g + ggplot2::geom_line(aes(color = as.factor(Objective), linetype = as.factor(Objective)))
  g = g + ggplot2::theme_bw()
  g = g + ggplot2::scale_color_brewer(palette = "Dark2")
  g = g + ggplot2::labs(
    x = "Iteration",
    y = "Objective value",
    color = "Objective",
    linetype = "Objective"
  )
  return(g)
}
