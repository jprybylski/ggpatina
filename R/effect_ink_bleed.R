
#' Ink bleeding into paper fibers
#'
#' @param img magick image (rasterized plot).
#' @param radius Bleed radius in pixels.
#' @param strength 0..1 intensity.
#' @param bleed_color Ink color.
#' @param paper_texture Optional path or URL to a paper texture image.
#' @export
patina_ink_bleed <- function(img, radius = 2, strength = 0.5,
                             bleed_color = "#2a2a2a", paper_texture = NULL) {
  stopifnot(inherits(img, "magick-image"))
  wh <- img_wh(img)

  g   <- magick::image_convert(img, colorspace = "gray")
  inv <- magick::image_negate(g)
  th  <- magick::image_threshold(inv, type = "white", threshold = "50%")
  halo <- magick::image_morphology(th, "Dilate", kernel = paste0("Disk:", radius))
  halo <- magick::image_blur(halo, radius = 0, sigma = radius)
  halo <- magick::image_colorize(halo, opacity = 100, color = bleed_color)
  halo <- set_alpha(halo, strength)

  base <- if (!is.null(paper_texture)) {
    T <- magick::image_read(paper_texture)
    magick::image_resize(T, paste0(wh["w"], "x", wh["h"], "!"))
  } else {
    magick::image_blank(wh["w"], wh["h"], "white")
  }

  out <- magick::image_composite(base, halo, operator = "multiply")
  out <- magick::image_composite(out, img,  operator = "over")
  magick::image_blur(out, radius = 0, sigma = 0.3)
}
