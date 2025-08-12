#' Transparency slide preset
#'
#' Applies chromatic offset, light leaks, vignette, and grain for a vintage
#' slide look.
#'
#' @examples
#' if (requireNamespace("magick", quietly = TRUE)) {
#'   img <- magick::image_read("logo:")
#'   slideify_transparency(img)
#' }
#'
#' @param img magick image
#' @param ca_px integer pixel shift for R/B channels (chromatic aberration)
#' @param leak_strength 0..1 intensity of light leak
#' @param leak_pos one of "nw","ne","sw","se","center"
#' @param vignette 0..1 darkness at edges
#' @param vignette_feather 0.3..3 falloff softness (higher = softer)
#' @param skew keystone factor (0..~0.02)
#' @param grain 0..1 film grain strength
#' @export
slideify_transparency <- function(img,
                                  ca_px = 1L,
                                  leak_strength = 0.18,
                                  leak_pos = c("ne","nw","se","sw","center"),
                                  vignette = 0.22,
                                  vignette_feather = 1.6,
                                  skew = 0.008,
                                  grain = 0.35) {
  stopifnot(inherits(img, "magick-image"))
  leak_pos <- match.arg(leak_pos)
  out <- img

  # Gentle warmth & tiny lift (keep legible)
  out <- magick::image_modulate(out, brightness = 102, saturation = 104)
  out <- magick::image_colorize(out, opacity = 5, color = "#ff9a00")

  # Chromatic aberration via channel roll (very small by default)
  r <- magick::image_channel(out, "red")
  g <- magick::image_channel(out, "green")
  b <- magick::image_channel(out, "blue")
  if (ca_px != 0) {
    r <- .im_roll(r,  abs(ca_px), 0)
    b <- .im_roll(b, -abs(ca_px), 0)
  }
  out <- magick::image_combine(c(r, g, b), colorspace = "sRGB")

  # Perspective keystone (subtle)
  wh <- img_wh(out); w <- wh["w"]; h <- wh["h"]
  if (skew != 0) {
    dx <- round(w * skew)
    pts <- c(
      0,0,    dx,0,
      w,0,    w - dx,0,
      0,h,    0,h,
      w,h,    w,h
    )
    out <- magick::image_distort(out, distortion = "perspective", coordinates  = pts)
    wh <- img_wh(out); w <- wh["w"]; h <- wh["h"]
  }

  # Light leak (small, warm, positionable)
  if (leak_strength > 0) {
    leak <- magick::image_read("radial-gradient:#ffd8aa-#000000")
    leak <- magick::image_resize(leak, sprintf("%dx%d!", w, h))
    leak <- magick::image_modulate(leak, brightness = 145, saturation = 180)
    leak <- magick::image_colorize(leak, opacity = 18, color = "#ff8a00")
    leak <- set_alpha(leak, leak_strength)
    grav <- switch(leak_pos, ne="northeast", nw="northwest", se="southeast", sw="southwest", center="center")
    # build protection mask from the (pre-leak) base
    g        <- magick::image_convert(out, colorspace = "gray")
    ink      <- magick::image_negate(g)                    # dark -> bright
    ink      <- magick::image_blur(ink, 0, 1.5)
    protect  <- magick::image_fx(ink, "u*0.7")             # 0..1, tune strength
    # attenuate leak alpha by (1 - protect)
    a        <- magick::image_channel(leak, "alpha")
    atten    <- magick::image_composite(
                  magick::image_negate(protect), a, operator = "multiply"
                )
    leak     <- magick::image_composite(leak, atten, operator = "copy_opacity")
    # then composite (screen or blend)
    out <- magick::image_composite(out, leak, operator = "screen", gravity = grav)
  }

  # Vignette (feathered; multiply darkens edges while keeping mids)
  if (vignette > 0) {
    vig <- magick::image_read("radial-gradient:rgba(0,0,0,0)-black")
    vig <- magick::image_resize(vig, sprintf("%dx%d!", w, h))
    # soften/feather the alpha falloff
    vig <- magick::image_fx(vig, sprintf("u^%g", vignette_feather), channel = "alpha")
    vig <- set_alpha(vig, vignette)
    out <- magick::image_composite(out, vig, operator = "multiply")
  }

  # Film grain (soft-light so it doesn’t crush tones)
  if (grain > 0) {
    base  <- magick::image_blank(w, h, "gray50")
    noise <- magick::image_noise(base, "gaussian")
    noise <- magick::image_blur(noise, 0, 0.6)
    noise <- set_alpha(noise, grain * 0.6)
    out   <- magick::image_composite(out, noise, operator = "soft_light")
  }

  # Tiny lens softness + micro contrast for readability
  out <- magick::image_blur(out, 0, 0.35)
  out <- magick::image_contrast(out, sharpen = TRUE)

  out
}




# roll/shift with wrap-around (dx>0 => right, dy>0 => down)
#'
#' @keywords internal
#' @noRd
.im_roll <- function(im, dx = 0, dy = 0) {
  info <- magick::image_info(im)[1, ]; w <- info$width; h <- info$height
  # horizontal
  s <- ((dx %% w) + w) %% w
  if (s > 0) {
    right <- magick::image_crop(im, sprintf("%dx%d+%d+0", w - s, h, s))
    left  <- magick::image_crop(im, sprintf("%dx%d+0+0",  s,     h))
    im <- magick::image_append(c(right, left), stack = FALSE)
  }
  # vertical
  s <- ((dy %% h) + h) %% h
  if (s > 0) {
    lower <- magick::image_crop(im, sprintf("%dx%d+0+%d", w, h - s, s))
    upper <- magick::image_crop(im, sprintf("%dx%d+0+0", w, s))
    im <- magick::image_append(c(lower, upper), stack = TRUE)
  }
  im
}