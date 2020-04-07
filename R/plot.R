plotTrajectories = function(x, obj.cols = c("tsp", "wtsp", "ttp", "wttp"), in.one.plot = TRUE, to01 = FALSE, highlight.which = NULL, highlight.path = FALSE, highlight.start = TRUE, highlight.area = FALSE, ...) {
  # add iteration column (x-axis)
  if (!is.null(x$objective.type)) {
    x = dplyr::group_by(x, objective.type) %>%
      dplyr::mutate(iter = 1:n()) %>%
      dplyr::ungroup()
  } else {
    x$iter = 1:nrow(x)
  }

  # debug
  #kk <<- x

  # normalize
  if (to01) {
    for (obj.col in obj.cols) {
      x[[obj.col]] = normalize(x[[obj.col]])
    }
  }

  # convert to long format
  id.vars = setdiff(colnames(x), obj.cols)
  dfl = reshape2::melt(x, id.vars = id.vars, value.name = "Value", variable.name = "Objective")

  if (is.null(x$objective.type)) {
    g = ggplot2::ggplot(dfl, ggplot2::aes(x = iter, y = Value))
    g = g + ggplot2::geom_line(ggplot2::aes(color = as.factor(Objective), linetype = as.factor(Objective)))
    g = g + ggplot2::theme_bw()
    g = g + ggplot2::scale_color_brewer(palette = "Dark2")
    g = g + ggplot2::labs(
      x = "Iteration",
      y = "Objective value",
      color = "Objective",
      linetype = "Objective"
    )

    # facetting?
    if (!in.one.plot) {
      g = g + ggplot2::facet_wrap(. ~ Objective, scales = "free_y", nrow = 1L)
    }

    # highlight
    if (!is.null(highlight.which)) {
      # get relevant blocks
      traj.selected = x[[highlight.which]]
      blocks = getMonotonicBlocks(traj.selected, ...)
      #print(blocks)
      # are there relevant blocks?
      if (blocks$n > 0L) {
        # get relevant data points
        dfl.hl = dfl[dfl$Objective == highlight.which, , drop = FALSE]
        # filter stuff between start and end
        for (i in 1:blocks$n) {
          if (highlight.path) {
            tmp = dfl.hl[blocks$start[i]:blocks$end[i], , drop = TRUE]
            g = g + ggplot2::geom_line(data = tmp, ggplot2::aes(color = as.factor(Objective), linetype = as.factor(Objective)), size = 1.2)
          }
          if (highlight.area) {
            g = g + ggplot2::annotate(geom = "rect", xmin = blocks$start[i], xmax = blocks$end[i], ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.7)
          }
          if (highlight.start) {
            g = g + ggplot2::geom_vline(xintercept = blocks$start[i], color = "grey", size = 1.1)
          }
        }
      }
    }

    g = g + ggplot2::theme(legend.position = "top")
    return(g)
  } else {
    g = ggplot2::ggplot(dfl, ggplot2::aes(x = iter, y = Value))
    g = g + ggplot2::geom_line(ggplot2::aes(color = as.factor(objective.type), linetype = as.factor(objective.type)))
    g = g + ggplot2::theme_bw()
    g = g + ggplot2::scale_color_brewer(palette = "Dark2")
    g = g + ggplot2::labs(
      x = "Iteration",
      y = "Objective value",
      color = "EA driver",
      linetype = "EA driver")
    g = g + ggplot2::facet_wrap(. ~ Objective, scales = "free_y")
    g = g + ggplot2::theme(legend.position = "top")
    return(g)
  }
}
