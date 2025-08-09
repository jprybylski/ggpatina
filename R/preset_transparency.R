
#' Transparency slide preset (chromatic offset, light leak, vignette, grain)
#' @param img magick image
#' @param leak_strength 0..1
#' @param vignette 0..1
#' @param skew Keystone factor (0..~0.03)
#' @param grain 0..1
#' @export
slideify_transparency <- function(img, leak_strength = 0.25, vignette = 0.35, skew = 0.015, grain = 0.8) {
  stopifnot(inherits(img, "magick-image"))
  out <- img

  # Mild fade & warmth
  out <- magick::image_modulate(out, brightness = 98, saturation = 90)
  out <- magick::image_colorize(out, opacity = 6, color = "orange")

  # Chromatic aberration via channel roll
  ch <- magick::image_separate(out, "RGB")
  r <- magick::image_roll(ch[[1]], 2, 0)
  g <- ch[[2]]
  b <- magick::image_roll(ch[[3]], -2, 0)
  out <- magick::image_combine(c(r, g, b), colorspace = "sRGB")

  # Perspective keystone
  wh <- img_wh(out); w <- wh['w']; h <- wh['h']
  dx <- round(w * skew)
  pts <- c(
    0,0,      dx,0,
    w,0,      w-dx,0,
    0,h,      0,h,
    w,h,      w,h
  )
  out <- magick::image_distort(out, method = "Perspective", points = pts)

  # Light leak
  leak <- magick::image_read("radial-gradient:#ffcc88-#000000")
  leak <- magick::image_resize(leak, paste0(w, "x", h, "!"))
  leak <- magick::image_modulate(leak, brightness = 150, saturation = 200)
  leak <- magick::image_colorize(leak, opacity = 25, color = "orange")
  leak <- set_alpha(leak, leak_strength)
  out  <- magick::image_composite(out, leak, operator = "screen")

  # Vignette
  vign <- magick::image_read("radial-gradient:black-transparent")
  vign <- magick::image_resize(vign, paste0(w, "x", h, "!"))
  vign <- set_alpha(vign, vignette)
  out  <- magick::image_composite(out, vign, operator = "multiply")

  # Grain & slight blur
  out <- magick::image_noise(out, "gaussian")
  out <- magick::image_blur(out, radius = 0, sigma = 0.6)
  if (grain > 0) out <- magick::image_contrast(out, sharpen = TRUE)

  out
}
