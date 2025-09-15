
#' Render a ggplot/grob/gtable to a magick image
#'
#' @param x ggplot, grob, or gtable.
#' @param width,height Plot size in inches.
#' @param dpi Resolution for rasterization.
#' @param bg Background color.
#' @return A `magick-image`.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'     ggplot2::geom_point()
#'   as_magick(p)
#' }
#' @export
as_magick <- function(x, width = 6, height = 4, dpi = 300, bg = "white") {
  f <- tempfile(fileext = ".png")
  ggsave_st(filename = f, plot=x, width = width, height = height, units = "in", dpi = dpi, bg = bg)
  magick::image_read(f, density=dpi)
}
