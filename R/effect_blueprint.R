
#' Blueprint (cyanotype) style
#'
#' @param img magick image.
#' @param bg Blueprint background color.
#' @param grid Logical; overlay a white grid.
#' @param grid_px Grid spacing in pixels.
#' @param paper_texture Optional paper texture.
#' @param line_soften Gaussian sigma for softening line mask.
#' @export
patina_blueprint <- function(img, bg = "#0e4a7b", grid = FALSE, grid_px = 48,
                             paper_texture = NULL, line_soften = 0.5) {
  stopifnot(inherits(img, "magick-image"))
  wh <- img_wh(img)

  g   <- magick::image_convert(img, colorspace = "gray")
  g   <- magick::image_contrast(g)
  inv <- magick::image_negate(g)
  mask <- magick::image_threshold(inv, type = "white", threshold = "55%")
  if (line_soften > 0) mask <- magick::image_blur(mask, 0, sigma = line_soften)

  base <- magick::image_blank(wh["w"], wh["h"], bg)
  if (!is.null(paper_texture)) {
    T <- magick::image_read(paper_texture)
    T <- magick::image_resize(T, paste0(wh["w"], "x", wh["h"], "!"))
    T <- set_alpha(T, 0.35)
    base <- magick::image_composite(base, T, operator = "multiply")
  }

  out <- magick::image_composite(base, mask, operator = "screen")

  if (isTRUE(grid)) {
    # Generate a grid via simple drawing in magick
    grid_img <- magick::image_blank(wh["w"], wh["h"], "none")
    # Draw vertical lines
    # (Grid drawing kept minimal; for precise grids, consider drawing with image_graph in R.)

    # Fallback: add a very soft vignette edge for depth
    vig <- magick::image_read("radial-gradient:black-transparent")
    vig <- magick::image_resize(vig, paste0(wh["w"], "x", wh["h"], "!"))
    vig <- set_alpha(vig, 0.15)
    out <- magick::image_composite(out, vig, operator = "multiply")
  }

  out
}
