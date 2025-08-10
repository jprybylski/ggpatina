#' Faded, overexposed Kodachrome vibe (warm cast + halation + grain)
#'
#' @param img magick image (rasterized plot).
#' @param warmth 0..1 warm colorize.
#' @param fade 0..1 slight highlight lift.
#' @param bloom 0..1 halation glow strength (around strokes).
#' @param grain 0..1 grain amount.
#' @export
patina_kodachrome <- function(img, warmth = 0.25, fade = 0.25, bloom = 0.35, grain = 0.6) {
  stopifnot(inherits(img, "magick-image"))
  wh <- try(img_wh(img), silent = TRUE)
  if (inherits(wh, "try-error") || is.null(wh)) {
    info <- magick::image_info(img)[1, ]
    wh <- c(w = info$width, h = info$height)
  }
  w <- wh["w"]; h <- wh["h"]

  out <- img

  # 1) Subtle "fade" by lifting the floor a touch (avoid pure white screen)
  if (fade > 0) {
    # lift shadows: u*(1-a) + a, with small a
    a <- fade * 0.12
    out <- magick::image_fx(out, sprintf("u*(1-%f)+%f", a, a))
  }

  # 2) Warmth: gentle orange colorize + slight saturation bump
  if (warmth > 0) {
    out <- magick::image_colorize(out, opacity = round(100 * warmth * 0.6), color = "#ffb07a")
    out <- magick::image_modulate(out, saturation = 100 + round(18 * warmth))
  }

  # 3) Halation/bloom around strokes (derived from dark content, not white background)
  if (bloom > 0) {
    g    <- magick::image_convert(img, colorspace = "gray")
    ink  <- magick::image_negate(g)                                      # dark -> bright
    ink  <- magick::image_threshold(ink, type = "white", threshold = "45%") # pick strokes
    halo <- magick::image_morphology(ink, "Dilate", kernel = "Disk:2")   # push outward
    halo <- magick::image_blur(halo, radius = 0, sigma = 4)
    halo <- magick::image_colorize(halo, opacity = 100, color = "#ffb07a")
    halo <- set_alpha(halo, bloom * 0.65)
    out  <- magick::image_composite(out, halo, operator = "screen")
  }

  # 4) Light lens softness
  out <- magick::image_blur(out, radius = 0, sigma = 0.4)

  # 5) Film grain as an overlay (doesn't obliterate content)
  if (grain > 0) {
    base  <- magick::image_blank(w, h, "gray50")
    noise <- magick::image_noise(base, "gaussian")
    noise <- magick::image_blur(noise, 0, 0.7)
    noise <- set_alpha(noise, grain * 0.55)
    out   <- magick::image_composite(out, noise, operator = "soft_light")
  }

  out
}
