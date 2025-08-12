#' Old newscast effect
#'
#' Adds scanlines, chroma bleed, tube glow, vignette, and noise to mimic a CRT
#' broadcast.
#'
#' @examples
#' if (requireNamespace("magick", quietly = TRUE)) {
#'   img <- magick::image_read("logo:")
#'   patina_newscast(img)
#' }
#'
#' @param img magick image
#' @param sat saturation multiplier (1 = unchanged)
#' @param scan_strength 0..1 strength of scanlines
#' @param scan_period integer px per dark line (e.g., 3–5)
#' @param ca_px integer pixel shift for R/B channels (chromatic aberration)
#' @param glow 0..1 highlight bloom
#' @param vignette 0..1 edge darkening
#' @param noise 0..1 CRT noise/grain
#' @param warp one of "none","full","corner"
#' @param warp_amount curvature amount for warp (typ. 0.03–0.10)
#' @param corner which corner for corner-warp: "ne","nw","se","sw"
#' @param corner_frac size of the warped corner region as fraction of width/height (0.3–0.8)
#' @param corner_margin fraction margin from edges when placing the corner region
#' @export
patina_newscast <- function(
  img,
  sat = 1.00,
  scan_strength = 0.28,
  scan_period = 4L,
  ca_px = 1L,
  glow = 0.25,
  vignette = 0.18,
  noise = 0.25,
  warp = c("none","full","corner"),
  warp_amount = 0.06,
  corner = c("ne","nw","se","sw"),
  corner_frac = 0.52,
  corner_margin = 0.04
) {
  stopifnot(inherits(img, "magick-image"))
  warp  <- match.arg(warp)
  corner<- match.arg(corner)

  wh <- img_wh(img); w <- wh["w"]; h <- wh["h"]

  # gentle contrast & color control
  out <- magick::image_modulate(img, brightness = 102, saturation = round(100 * sat))
  out <- magick::image_contrast(out, sharpen = TRUE)

  # tiny chromatic aberration
  if (ca_px != 0) {
    r <- magick::image_channel(out, "red")
    g <- magick::image_channel(out, "green")
    b <- magick::image_channel(out, "blue")
    r <- .im_roll(r,  abs(ca_px), 0)
    b <- .im_roll(b, -abs(ca_px), 0)
    out <- magick::image_combine(c(r, g, b), colorspace = "sRGB")
  }

  # scanlines: build a tile and stack
  if (scan_strength > 0 && scan_period > 1) {
    line  <- magick::image_blank(w, scan_period, "transparent")
    dark  <- magick::image_blank(w, 1, "black")
    dark  <- set_alpha(dark, scan_strength)
    line  <- magick::image_composite(line, dark, operator = "over",
                                     offset = sprintf("+0+%d", scan_period - 1))
    reps  <- ceiling(h / scan_period)
    slab  <- if (reps > 1) magick::image_append(do.call(c, replicate(reps, line, simplify = FALSE)), stack = TRUE) else line
    slab  <- magick::image_crop(slab, sprintf("%dx%d+0+0", w, h))
    out   <- magick::image_composite(out, slab, operator = "multiply")
  }

  # tube glow: isolate brights, blur, screen back
  if (glow > 0) {
    lum  <- magick::image_convert(out, colorspace = "gray")
    hi   <- magick::image_threshold(lum, type = "white", threshold = "80%")
    halo <- magick::image_blur(hi, 0, 2.5)
    halo <- set_alpha(halo, glow * 0.8)
    out  <- magick::image_composite(out, halo, operator = "screen")
  }

  # vignette (subtle)
  if (vignette > 0) {
    vig <- magick::image_read("radial-gradient:rgba(0,0,0,0)-black")
    vig <- magick::image_resize(vig, sprintf("%dx%d!", w, h))
    vig <- set_alpha(vig, vignette)
    out <- magick::image_composite(out, vig, operator = "multiply")
  }

  # CRT noise
  if (noise > 0) {
    base  <- magick::image_blank(w, h, "gray50")
    n     <- magick::image_noise(base, "gaussian")
    n     <- magick::image_blur(n, 0, 0.6)
    n     <- set_alpha(n, noise * 0.6)
    out   <- magick::image_composite(out, n, operator = "soft_light")
  }

  # ---- Warp controls ---------------------------------------------------------
  if (warp != "none" && warp_amount != 0) {
    if (warp == "full") {
      # gentle barrel curve across the whole frame (negative implode = bulge)
      out <- magick::image_implode(out, factor = -warp_amount)
    } else { # warp == "corner"
      # warp whole image, then reveal only a corner "picture-in-picture" region
      warped <- magick::image_implode(out, factor = -warp_amount)

      # build a rectangular mask at the chosen corner
      cw <- max(1L, floor(w * corner_frac))
      ch <- max(1L, floor(h * corner_frac))
      mx <- max(0L, floor(w * corner_margin))
      my <- max(0L, floor(h * corner_margin))

      off <- switch(corner,
        ne = c(w - cw - mx, my),
        nw = c(mx, my),
        se = c(w - cw - mx, h - ch - my),
        sw = c(mx, h - ch - my)
      )
      mask <- magick::image_blank(w, h, "black")
      cut  <- magick::image_blank(cw, ch, "white")
      mask <- magick::image_composite(mask, cut,
                                      operator = "over",
                                      offset   = sprintf("+%d+%d", off[1], off[2]))
      mask <- magick::image_blur(mask, 0, 1.2)  # feather edges slightly

      warped <- magick::image_composite(warped, mask, operator = "copy_opacity")
      out    <- magick::image_composite(out, warped, operator = "over")
    }
  }

  # mild softness
  magick::image_blur(out, 0, 0.35)
}