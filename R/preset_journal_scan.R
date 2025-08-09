
#' Old journal scan preset (sepia/mono, halftone, paper texture, scanner shadow)
#' @param img magick image
#' @param paper optional path to paper texture
#' @param sepia logical
#' @param dither logical
#' @param tilt_deg small rotation
#' @export
scanify_journal <- function(img, paper = NULL, sepia = TRUE, dither = TRUE, tilt_deg = 0.6) {
  stopifnot(inherits(img, "magick-image"))
  out <- img

  out <- if (sepia) magick::image_sepia_tone(out, threshold = 90) else magick::image_modulate(out, saturation = 10)
  out <- magick::image_contrast(out)
  out <- magick::image_blur(out, radius = 0, sigma = 0.4)
  if (dither) out <- magick::image_ordered_dither(out, "h6x6o")
  out <- magick::image_rotate(out, tilt_deg)

  if (!is.null(paper)) {
    wh <- img_wh(out)
    T <- magick::image_read(paper)
    T <- magick::image_resize(T, paste0(wh['w'],"x",wh['h'],"!"))
    out <- magick::image_composite(out, T, operator = "multiply", compose_args = "100%")
  }

  # Top margin shadow
  wh <- img_wh(out)
  shadow <- magick::image_read("gradient:black-transparent")
  shadow <- magick::image_flip(shadow)
  shadow <- magick::image_resize(shadow, paste0(wh['w'],"x","60!"))
  shadow <- set_alpha(shadow, 0.25)
  out    <- magick::image_composite(out, shadow, offset = "+0+0", operator = "multiply")

  out
}
