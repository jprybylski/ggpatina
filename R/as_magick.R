
#' Render a ggplot/grob/gtable to a magick image
#'
#' @param x ggplot, grob, or gtable.
#' @param width,height Plot size in inches.
#' @param dpi Resolution for rasterization.
#' @param bg Background color.
#' @return A `magick-image`.
#' @export
as_magick <- function(x, width = 6, height = 4, dpi = 300, bg = "white") {
  grob <- if (inherits(x, "ggplot")) ggplot2::ggplotGrob(x) else x
  f <- tempfile(fileext = ".png")
  ragg::agg_png(filename = f, width = width, height = height, units = "in", res = dpi, background = bg)
  grid::grid.newpage(); grid::grid.draw(grob)
  grDevices::dev.off()
  magick::image_read(f)
}
