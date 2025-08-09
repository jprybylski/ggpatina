
#' Faded, overexposed Kodachrome vibe (warm cast + halation + grain)
#'
#' @param img magick image.
#' @param warmth 0..1 warm colorize.
#' @param fade 0..1 highlight lift.
#' @param bloom 0..1 halation glow strength.
#' @param grain 0..1 grain amount (qualitative).
#' @export
patina_kodachrome <- function(img, warmth = 0.25, fade = 0.25, bloom = 0.35, grain = 0.6) {
  stopifnot(inherits(img, "magick-image"))
  out <- img

  if (fade > 0) {
    white <- magick::image_blank(img_wh(img)["w"], img_wh(img)["h"], "white")
    white <- set_alpha(white, fade)
    out <- magick::image_composite(out, white, operator = "screen")
  }

  if (warmth > 0) {
    out <- magick::image_colorize(out, opacity = round(100*warmth), color = "#ff8a00")
    out <- magick::image_modulate(out, saturation = 105)
  }

  if (bloom > 0) {
    lum  <- magick::image_convert(img, colorspace = "gray")
    mask <- magick::image_threshold(lum, type = "white", threshold = "75%")
    glow <- magick::image_blur(mask, radius = 0, sigma = 6)
    glow <- magick::image_colorize(glow, opacity = 100, color = "#ffb07a")
    glow <- set_alpha(glow, bloom)
    out  <- magick::image_composite(out, glow, operator = "screen")
  }

  out <- magick::image_blur(out, radius = 0, sigma = 0.6)
  if (grain > 0) out <- magick::image_noise(out, "gaussian")
  out
}
