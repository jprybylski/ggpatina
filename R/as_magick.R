
#' Render a ggplot/grob/gtable to a magick image
#'
#' @param x ggplot, grob, or gtable.
#' @param width,height Plot size in inches.
#' @param dpi Resolution for rasterization.
#' @param bg Background color.
#' @return A `magick-image`.
#' @export
as_magick <- function(x, width = 6, height = 4, dpi = 300, bg = "white") {
  f <- tempfile(fileext = ".png")
  ggsave_st(filename = f, plot=x, width = width, height = height, units = "in", dpi = dpi, bg = bg)
  magick::image_read(f)
}
