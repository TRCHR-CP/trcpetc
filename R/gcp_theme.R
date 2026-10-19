#' GCP ggplot2 theme
#'
#' A black-and-white \pkg{ggplot2} theme with Arial text, black facet strips,
#' and wider plot margins.
#'
#' @format A \code{ggplot2} theme object.
#' @examples
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point() +
#'   gcp_theme
#' @export
gcp_theme <- ggplot2::theme_bw() +
  ggplot2::theme(
    axis.title = ggplot2::element_text(size = 14, family = "Arial"),
    axis.title.x = ggplot2::element_text(
      margin = ggplot2::margin(t = 18, r = 0, b = 0, l = 0)
    ),
    axis.title.y = ggplot2::element_text(
      margin = ggplot2::margin(t = 0, r = 18, b = 0, l = 0)
    ),
    axis.line.x = ggplot2::element_line(color = "black", linewidth = 0.5),
    axis.line.y = ggplot2::element_line(color = "black", linewidth = 0.5),
    panel.border = ggplot2::element_blank(),
    panel.spacing = grid::unit(1, "lines"),
    panel.grid.minor = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(
      family = "Arial", size = 12, color = "white"
    ),
    strip.background = ggplot2::element_rect(fill = "black"),
    strip.placement = "outside",
    legend.text = ggplot2::element_text(family = "Arial", size = 12),
    legend.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
    legend.key = ggplot2::element_rect(
      fill = "transparent", color = "transparent"
    ),
    legend.key.size = grid::unit(24, "points"),
    legend.background = ggplot2::element_rect(fill = "transparent"),
    plot.margin = ggplot2::margin(t = 18, r = 18, b = 48, l = 48)
  )
