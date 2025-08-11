# ========= utilities =========================================================
# Soft rectangular mask from pixel coords
rect_mask_px <- function(w, h, left, top, right, bottom, feather = 8) {
  left <- max(0L, round(left)); top <- max(0L, round(top))
  right <- min(w, round(right)); bottom <- min(h, round(bottom))
  pw <- max(1L, right - left); ph <- max(1L, bottom - top)
  m <- magick::image_blank(w, h, "black")
  white <- magick::image_blank(pw, ph, "white")
  m <- magick::image_composite(m, white, "over", offset = sprintf("+%d+%d", left, top))
  if (feather > 0) m <- magick::image_blur(m, 0, feather)
  m
}

# Fade warp near frame edges (prevents “fake margins”)
edge_guard_mask <- function(w, h, guard_frac = 0.03, feather = 3) {
  pad <- max(1L, round(min(w, h) * guard_frac))
  rect_mask_px(w, h, pad, pad, w - pad, h - pad, feather)
}

#' Pixel mask for the ggplot plotting region (panel + axes, optional)
#'
#' Builds a feathered rectangular mask in *device pixels* that tightly covers
#' the visible plotting region. By default it unions the panel viewport(s) and
#' axis viewports, which matches user expectations when you want the wobble
#' confined to the “plot area with axes” while excluding titles/margins/legends.
#'
#' @param p ggplot object.
#' @param width,height,dpi Output device size (inches, DPI) used for both layout and mask.
#' @param feather Gaussian feather radius (px) for the mask edge.
#' @param expand_px Integer vector `c(x, y)` padding (px) added to each side.
#' @param include_axes Logical; if TRUE (default) include axis viewports in the union.
#' @param vp_min_in Numeric vector `c(w_in, h_in)` minimum inches to accept a viewport.
#' @param debug Logical; if TRUE, prints viewport names and bbox in pixels.
#' @param return_bbox Logical; if TRUE, returns a list with `mask` and `bbox`
#'   (useful for caching or visual debug). Default FALSE returns just the mask.
#' @return A `magick-image` mask (white = inside, black = outside); or a list if `return_bbox=TRUE`.
panel_mask_from_plot <- function(
  p, width, height, dpi,
  feather      = 10,
  expand_px    = c(0L, 0L),
  include_axes = TRUE,
  vp_min_in    = c(0.25, 0.25),
  debug        = FALSE,
  return_bbox  = FALSE
) {
  stopifnot(inherits(p, "ggplot"))
  stopifnot(requireNamespace("ragg", quietly = TRUE),
            requireNamespace("magick", quietly = TRUE))

  # materialize vps at exact device size
  tmp <- tempfile(fileext = ".png")
  ragg::agg_png(tmp, width = width, height = height, units = "in",
                res = dpi, background = "transparent")
  on.exit(grDevices::dev.off(), add = TRUE)
  g <- ggplot2::ggplotGrob(p)
  grid::grid.newpage(); grid::grid.draw(g); grid::grid.force()

  # collect candidate viewports
  listing  <- grid::grid.ls(viewports = TRUE, grobs = FALSE, print = FALSE)
  vp_names <- tryCatch(listing$name, error = function(e) unlist(listing))
  want <- grepl("panel", vp_names, ignore.case = TRUE)
  if (isTRUE(include_axes)) want <- want | grepl("axis", vp_names, ignore.case = TRUE)
  cand <- unique(vp_names[want])

  # measure each candidate in device inches via deviceLoc(npc→device)
  measure_vp <- function(nm) {
    ok <- try(grid::seekViewport(nm), silent = TRUE)
    if (inherits(ok, "try-error")) return(NULL)
    xy0 <- grid::deviceLoc(grid::unit(0, "npc"), grid::unit(0, "npc"), valueOnly = TRUE)
    xy1 <- grid::deviceLoc(grid::unit(1, "npc"), grid::unit(1, "npc"), valueOnly = TRUE)
    grid::upViewport()
    w_in <- abs(xy1$x - xy0$x); h_in <- abs(xy1$y - xy0$y)
    if (w_in >= vp_min_in[1] || h_in >= vp_min_in[2]) {
      list(name = nm,
           x0 = min(xy0$x, xy1$x), x1 = max(xy0$x, xy1$x),
           y0 = min(xy0$y, xy1$y), y1 = max(xy0$y, xy1$y))
    } else NULL
  }
  boxes <- Filter(Negate(is.null), lapply(cand, measure_vp))
  if (!length(boxes)) stop("No eligible panel/axis viewports found.")

  # union in inches
  L_in <- min(vapply(boxes, `[[`, numeric(1), "x0"))
  R_in <- max(vapply(boxes, `[[`, numeric(1), "x1"))
  T_in <- min(vapply(boxes, `[[`, numeric(1), "y0"))
  B_in <- max(vapply(boxes, `[[`, numeric(1), "y1"))

  # inches → pixels + pad + clamp
  wpx <- as.integer(round(width * dpi)); hpx <- as.integer(round(height * dpi))
  ex  <- as.integer(if (length(expand_px) >= 1) expand_px[1] else 0L)
  ey  <- as.integer(if (length(expand_px) >= 2) expand_px[2] else ex)
  L <- max(0L, floor(L_in * dpi) - ex)
  R <- min(wpx, ceiling(R_in * dpi) + ex)
  T <- max(0L, floor(T_in * dpi) - ey)
  B <- min(hpx, ceiling(B_in * dpi) + ey)

  if (debug) {
    used <- paste(vapply(boxes, `[[`, character(1), "name"), collapse = ", ")
    cat("viewports used:\n  ", used, "\n")
    cat(sprintf("vp-union bbox px: L=%d T=%d R=%d B=%d (w=%d h=%d)\n", L, T, R, B, wpx, hpx))
  }

  mask <- rect_mask_px(wpx, hpx, L, T, R, B, feather = feather)
  if (return_bbox) {
    invisible(list(mask = mask, bbox = c(L = L, T = T, R = R, B = B),
                   used = vapply(boxes, `[[`, character(1), "name")))
  } else mask
}




# ---- image-only fallback (no ggplot object) ---------------------------------
auto_panel_mask_img <- function(img, feather = 10, expand_frac = c(0.015, 0.025), work_w = 520) {
  wh <- img_wh(img); w <- wh["w"]; h <- wh["h"]
  g  <- magick::image_convert(img, colorspace = "gray")
  g  <- magick::image_resize(g, sprintf("%dx%d!", work_w, round(work_w * h / w)))
  e  <- magick::image_edge(g)
  e  <- magick::image_blur(e, 0, 1.0)

  to_mat <- function(im) {
    arr <- magick::image_data(im, channels = "gray")
    matrix(as.integer(arr[1,,]), nrow = dim(arr)[2], ncol = dim(arr)[3])
  }
  M  <- to_mat(e); xs <- colSums(M); ys <- rowSums(M)
  Lx <- length(xs); Ly <- length(ys)

  li <- 2:floor(0.45 * Lx);  ri <- ceiling(0.55 * Lx):max(Lx-1, 1)
  ti <- 2:floor(0.45 * Ly);  bi <- ceiling(0.55 * Ly):max(Ly-1, 1)
  l <- li[which.max(xs[li])]; r <- ri[which.max(xs[ri])]
  t <- ti[which.max(ys[ti])]; b <- bi[which.max(ys[bi])]

  if (any(is.na(c(l, r, t, b))) || l >= r || t >= b) {
    # conservative center
    return(rect_mask_px(w, h, 0.08*w, 0.08*h, 0.94*w, 0.92*h, feather))
  }

  ex <- expand_frac[1]; ey <- ifelse(length(expand_frac) > 1, expand_frac[2], expand_frac[1])
  bbox <- c((l-1)/Lx - ex, (t-1)/Ly - ey, r/Lx + ex, b/Ly + ey)
  # clamp normalized bbox to [0,1]
  bbox[1] <- max(0, bbox[1]); bbox[2] <- max(0, bbox[2])
  bbox[3] <- min(1, bbox[3]); bbox[4] <- min(1, bbox[4])

  rect_mask_px(w, h, bbox[1]*w, bbox[2]*h, bbox[3]*w, bbox[4]*h, feather)
}


# ====== auto scale ==========================================================
calc_auto_scale <- function(w, h, dpi, freq, strength = 0.7) {
  px_min   <- min(w, h)
  px_ref   <- 30        # reference canvas (px)
  base_px  <- 2.6         # amplitude at ref size, dpi=150, freq=0.6
  size_fac <- (px_min / px_ref)^0.85
  dpi_fac  <- (dpi / 150)^0.25
  freq_fac <- (0.6 / max(0.15, min(1.5, freq)))^0.70
  str_fac  <- 0.5 + 0.9 * max(0, min(1, strength))
  s <- base_px * size_fac * dpi_fac * freq_fac * str_fac
  pmax(1.0, pmin(s,px_ref))
}

# ====== displacement map & filter ===========================================
displace_map <- function(w, h, freq = 0.6, seed = 3, gate = NULL, edge_guard = 0.03) {
  neutral <- magick::image_blank(w, h, "gray50")
  set.seed(as.integer(seed) %% .Machine$integer.max)
  r <- magick::image_noise(neutral, "gaussian")
  g <- magick::image_noise(neutral, "gaussian")
  sigma <- max(0.3, (1.2 - max(0.05, min(1.5, freq))) * 6)
  r <- magick::image_blur(r, 0, sigma); g <- magick::image_blur(g, 0, sigma)

  guard <- edge_guard_mask(w, h, guard_frac = edge_guard, feather = 2)
  mask  <- if (is.null(gate)) guard else magick::image_composite(guard, gate, operator = "multiply")

  apply_mask <- function(chan) {
    chan <- magick::image_composite(chan, mask, operator = "copy_opacity")
    magick::image_composite(neutral, chan, operator = "over")
  }
  r <- apply_mask(r); g <- apply_mask(g)
  magick::image_combine(c(r, g, neutral), colorspace = "sRGB")
}

# ========= main API ==========================================================
#' Hand-drawn wobble (post-processing; auto scale; exact panel mask for ggplot)
#'
#' @param x ggplot/grob or magick-image
#' @param width,height,dpi Used when x is ggplot/grob
#' @param scale numeric px or "auto" (default)
#' @param strength 0..1 intensity when `scale="auto"` (default 0.7)
#' @param freq noise frequency (0.2 broad … 1.2 fine)
#' @param seed RNG seed
#' @param affect_text if FALSE, confine warp to panel region
#' @param focus "panel" (default if ggplot), "full", or "auto_image"
#' @param expand_px integer px to expand the panel box (ggplot path)
#' @param edge_guard fade warp near canvas edges (0..0.2)
#' @param fill_bg final background (e.g., "white" or "transparent")
#' @return magick-image
#' @export
hand_drawn_wiggle <- function(
  x, width = 6, height = 4, dpi = 300,
  scale = "auto", strength = 0.7,
  freq = 0.6, seed = 3,
  affect_text = TRUE,
  focus = c("panel","full","auto_image"),
  expand_px = c(6L, 6L),
  edge_guard = 0.03,
  fill_bg = NULL
) {
  stopifnot(requireNamespace("magick", quietly = TRUE))
  is_plot <- inherits(x, "ggplot") || inherits(x, "gtable")
  focus <- match.arg(focus)

  img <- if (inherits(x, "magick-image")) x else as_magick(x, width, height, dpi, bg = "transparent")
  wh <- img_wh(img); w <- wh["w"]; h <- wh["h"]

  if (is.character(scale) && tolower(scale) == "auto") {
    scale <- calc_auto_scale(w, h, dpi, freq, strength = strength)
  }

  gate <- NULL
  if (!affect_text) {
    if (is_plot && focus == "panel" && inherits(x, "ggplot")) {
      gate <- suppressMessages({
        panel_mask_from_plot(x, width, height, dpi, feather = 12, expand_px = expand_px)
      })
    } else if (focus == "auto_image") {
      gate <- auto_panel_mask_img(img, feather = 12, expand_frac = c(0.02, 0.03))
    } else {
      gate <- NULL  # full-frame
    }
  }

  map <- displace_map(w, h, freq = freq, seed = seed, gate = gate, edge_guard = edge_guard)
  out <- magick::image_composite(img, map, operator = "displace",
                                 compose_args = sprintf("%gx%g", scale, scale))
  if (!is.null(fill_bg)) out <- magick::image_background(out, fill_bg, flatten = TRUE)
  out
}
