#' Internal helper applying a sigmoid contrast curve
#'
#' @keywords internal
#' @noRd
.sigmoid_contrast <- function(img, contrast = 6, midpoint = 0.5, sharpen = TRUE) {
  s  <- if (sharpen) contrast else -contrast
  L0 <- 1/(1 + exp(-s * (0 - midpoint)))
  L1 <- 1/(1 + exp(-s * (1 - midpoint)))
  D  <- L1 - L0
  fx <- sprintf("(1/(1+exp(-(%0.8f)*(u-(%0.8f))))-(%0.8f))/(%0.8f)", s, midpoint, L0, D)
  magick::image_fx(img, fx)
}

#' Photocopier double-pass effect
#'
#' Adds a ghost offset, banding, and adjustable wash while protecting dark lines.
#'
#' @examples
#' if (requireNamespace("magick", quietly = TRUE)) {
#'   img <- magick::image_read("logo:")
#'   patina_photocopy(img)
#' }
#'
#' @param img magick image
#' @param offset c(x,y) ghost offset in px
#' @param ghost_opacity 0..1 opacity of the ghost pass
#' @param banding 0..1 banding strength
#' @param tilt_deg rotate (deskew) degrees
#' @param protect_lines 0..1 how strongly to protect dark strokes (1 = max)
#' @param wash 0..1 how much to lift paper/background (0 = none, 1 = strong)
#' @param line_gain 0..1 re-ink amount applied after effects (0 = none)
#' @param contrast_pop strength of final sigmoid contrast (0 = none)
#' @export
patina_photocopy <- function(img,
                             offset = c(3, -2),
                             ghost_opacity = 0.12,
                             banding = 0.12,
                             tilt_deg = 0.4,
                             protect_lines = 0.75,
                             wash = 0.25,
                             line_gain = 0.18,
                             contrast_pop = 6) {
  stopifnot(inherits(img, "magick-image"))
  wh <- img_wh(img); w <- wh["w"]; h <- wh["h"]

  # grayscale base
  base <- magick::image_modulate(img, saturation = 0)

  # --- build masks ---
  g   <- magick::image_convert(base, colorspace = "gray")
  ink <- magick::image_negate(g)                                      # dark -> bright
  ink <- magick::image_threshold(ink, type = "white", threshold = "48%")
  ink <- magick::image_blur(ink, 0, 1.2)                               # soften edges
  bg  <- magick::image_negate(ink)                                     # background mask

  # --- adjustable background wash (lift paper only) ---
  if (wash > 0) {
    a <- wash * 0.25                                                   # subtle lift per unit wash
    lifted <- magick::image_fx(base, sprintf("u*(1-%f)+%f", a, a))     # lift toward white
    a_lift <- magick::image_fx(bg, sprintf("u*%f", wash), channel = "alpha")
    lifted <- magick::image_composite(lifted, a_lift, operator = "copy_opacity")
    base   <- magick::image_composite(base, lifted, operator = "over")
  }

  # --- ghost pass (attenuated over ink) ---
  ghost <- magick::image_modulate(base, brightness = 100 + wash * 40, saturation = 0)
  a_ghost <- magick::image_fx(bg, sprintf("u*%f", ghost_opacity), channel = "alpha")
  ghost   <- magick::image_composite(ghost, a_ghost, operator = "copy_opacity")
  out     <- magick::image_composite(base, ghost, operator = "over",
                                     offset = sprintf("+%d+%d", offset[1], offset[2]))

  # --- horizontal banding (soft-light; attenuated over ink/background) ---
  if (banding > 0) {
    noise <- magick::image_blank(w, h, "gray50")
    noise <- magick::image_noise(noise, "gaussian")
    noise <- magick::image_motion_blur(noise, radius = 0, sigma = 12, angle = 0)
    noise <- magick::image_modulate(noise, brightness = 95, saturation = 0)
    protect <- magick::image_fx(bg, sprintf("u^%f", protect_lines), channel = "alpha")
    a_noise <- magick::image_fx(protect, sprintf("u*%f", banding), channel = "alpha")
    noise   <- magick::image_composite(noise, a_noise, operator = "copy_opacity")
    out     <- magick::image_composite(out, noise, operator = "soft_light")
  }

  # --- optional re-ink to fight wash-out (applied only where ink mask says) ---
  if (line_gain > 0) {
    reink <- base
    a_ink <- magick::image_fx(ink, sprintf("u*%f", line_gain), channel = "alpha")
    reink <- magick::image_composite(reink, a_ink, operator = "copy_opacity")
    out   <- magick::image_composite(out, reink, operator = "over")
  }

  # --- micro contrast pop (sigmoid) ---
  if (contrast_pop > 0) {
    out <- .sigmoid_contrast(out, contrast = contrast_pop, midpoint = 0.5, sharpen = TRUE)
  }

  # deskew and restore canvas
  out <- magick::image_rotate(out, tilt_deg)
  out <- magick::image_background(out, "white", flatten = TRUE)
  out <- magick::image_crop(out, sprintf("%dx%d+0+0", w, h))

  out
}
