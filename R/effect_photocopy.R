
#' Photocopier double-pass misalignment (ghost + banding)
#'
#' @param img magick image.
#' @param offset length-2 integer vector (x,y) ghost offset in pixels.
#' @param ghost_opacity 0..1 opacity of ghost pass.
#' @param banding 0..1 banding intensity.
#' @param tilt_deg small rotation in degrees to mimic deskew.
#' @export
patina_photocopy <- function(img, offset = c(3, -2), ghost_opacity = 0.35,
                             banding = 0.12, tilt_deg = 0.4) {
  stopifnot(inherits(img, "magick-image"))
  wh <- img_wh(img)

  base  <- magick::image_modulate(img, saturation = 0)
  ghost <- magick::image_modulate(base, brightness = 110, saturation = 0)
  ghost <- set_alpha(ghost, ghost_opacity)
  out <- magick::image_composite(base, ghost, operator = "over",
                                 offset = sprintf("+%d+%d", offset[1], offset[2]))

  noise <- magick::image_blank(wh["w"], wh["h"], "white")
  noise <- magick::image_noise(noise, "gaussian")
  noise <- magick::image_motion_blur(noise, radius = 0, sigma = 12, angle = 0)
  noise <- magick::image_modulate(noise, brightness = 90, saturation = 0)
  noise <- set_alpha(noise, banding)
  out   <- magick::image_composite(out, noise, operator = "multiply")

  out <- magick::image_rotate(out, tilt_deg)
  magick::image_crop(out, paste0(wh["w"], "x", wh["h"], "+0+0"))
}
