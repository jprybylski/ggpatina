#' Hand-drawn “wiggle” for ggplot/grobs
#'
#' Apply a hand-drawn wobble at exact pixel dimensions. Shapes are warped with an
#' SVG displacement filter; text is rendered with `ragg` so any font that works
#' in ggplot works here too. If `affect_text = TRUE`, the same kind of warp is
#' applied to the text layer using a raster displacement map, so labels wobble
#' as well without losing fonts/theme styling.
#'
#' @param x A ggplot object or grid grob.
#' @param width,height Plot size in inches.
#' @param dpi Output resolution (pixels = `width*dpi` by `height*dpi`).
#' @param scale Displacement amplitude (roughly pixels; larger = wobblier).
#' @param freq  Noise “frequency” (lower → broader waves, higher → finer jitter).
#' @param seed  Integer seed for reproducible noise (affects text raster warp).
#' @param affect_text Logical; if `FALSE` text stays crisp, if `TRUE` it’s warped too.
#' @param fill_bg Background color for the returned image (e.g., `"white"` or `"transparent"`).
#'
#' @return A `magick-image` of size `width*dpi` by `height*dpi`.
#'
#' @details
#' Shapes are warped in SVG (vector stage) and rasterized at the requested size.
#' Text is always drawn with `ragg` (so Google fonts you registered via
#' `sysfonts/showtext` are honored). When `affect_text = TRUE`, a displacement map
#' (generated to roughly match the SVG turbulence) is applied to the text layer
#' so its wobble visually matches the shapes.
#'
#'
#' @examples
#' \dontrun{
#' p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point() + ggplot2::geom_smooth(se = FALSE) +
#'   ggplot2::theme_minimal(12)
#'
#' # Shapes only wobble (text crisp)
#' img1 <- hand_drawn_svg(p, 6, 4, dpi = 300, scale = 3, freq = 0.5, seed = 7)
#'
#' # Shapes + text wobble
#' img2 <- hand_drawn_svg(p, 6, 4, dpi = 300, scale = 3, freq = 0.5, seed = 7, affect_text = TRUE)
#'}
#'
#' @name hand_drawn
NULL

# ---- internals ---------------------------------------------------------------

# small infix for defaulting NULL
`%||%` <- function(a, b) if (is.null(a)) b else a

.preq <- function(pkgs) {
  miss <- !vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  if (any(miss)) stop("Requires: ", paste(pkgs[miss], collapse = ", "), call. = FALSE)
}

.to_grob <- function(x) if (inherits(x, "ggplot")) ggplot2::ggplotGrob(x) else x

# conservative text detection; extend as you find new classes
.is_text_grob <- function(g) {
  any(inherits(g, c("text", "titleGrob", "richtext_grob", "textpath_grob"))) ||
    (!is.null(g$name) && grepl("(text|label|title|caption)", g$name))
}

.strip_grob <- function(g, keep = c("shapes","text")) {
  keep <- match.arg(keep)
  if (inherits(g, "gtable")) { g$grobs <- lapply(g$grobs, .strip_grob, keep = keep); return(g) }
  if (grid::is.grob(g)) {
    if (inherits(g, "gTree")) { g$children <- lapply(g$children, .strip_grob, keep = keep); return(g) }
    txt <- .is_text_grob(g)
    if (keep == "shapes" && txt) return(grid::nullGrob())
    if (keep == "text"   && !txt) return(grid::nullGrob())
  }
  g
}

# Warp a PNG raster using the same SVG wiggle filter (embed as data: URI)
.warp_raster_with_svg <- function(img_png, width, height, dpi, scale, freq, seed) {
  .preq(c("magick", "base64enc"))
  px_w <- as.integer(width * dpi); px_h <- as.integer(height * dpi)

  # Encode PNG as data URI (no external file refs, works in librsvg)
  raw_png <- magick::image_write(img_png, format = "png")
  uri <- paste0("data:image/png;base64,", base64enc::base64encode(raw_png))

  svg_txt <- sprintf('
    <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"
         width="%dpx" height="%dpx" viewBox="0 0 %d %d" preserveAspectRatio="xMidYMid meet">
      <defs>
        <filter id="ggpatina-wiggle" x="-10%%" y="-10%%" width="120%%" height="120%%">
          <feTurbulence type="fractalNoise" baseFrequency="%g" numOctaves="1" seed="%d" result="noise"/>
          <feDisplacementMap in="SourceGraphic" in2="noise" scale="%g"
                              xChannelSelector="R" yChannelSelector="G"/>
        </filter>
      </defs>
      <image x="0" y="0" width="%d" height="%d" xlink:href="%s" filter="url(#ggpatina-wiggle)"/>
    </svg>',
    px_w, px_h, px_w, px_h, freq, as.integer(seed), scale, px_w, px_h, uri
  )

  magick::image_read_svg(svg_txt, width = px_w, height = px_h)
}



# SVG warp for shapes (keeps <style> at root so theme/fonts survive)
.warp_svg_shapes <- function(grob, width, height, dpi, scale, freq, seed) {
  .preq(c("svglite","xml2","magick"))
  px_w <- as.integer(width * dpi); px_h <- as.integer(height * dpi)
  vb_w <- width * 72;              vb_h <- height * 72

  svg_txt <- svglite::stringSVG(code = grid::grid.draw(grob),
                                width = width, height = height, bg = "transparent")
  doc  <- xml2::read_xml(as.character(svg_txt))
  root <- xml2::xml_find_first(doc, '/*[local-name()="svg"]')
  xml2::xml_set_attr(root, "width",  paste0(px_w, "px"))
  xml2::xml_set_attr(root, "height", paste0(px_h, "px"))
  xml2::xml_set_attr(root, "viewBox", sprintf("0 0 %g %g", vb_w, vb_h))
  xml2::xml_set_attr(root, "preserveAspectRatio", "xMidYMid meet")

  defs <- xml2::xml_find_first(root, './*[local-name()="defs"]')
  if (inherits(defs, "xml_missing")) defs <- xml2::xml_add_child(root, "defs")
  filt <- sprintf(
    '<filter id="ggpatina-wiggle" x="-10%%" y="-10%%" width="120%%" height="120%%">
       <feTurbulence type="fractalNoise" baseFrequency="%g" numOctaves="1" seed="%d" result="noise"/>
       <feDisplacementMap in="SourceGraphic" in2="noise" scale="%g"
                           xChannelSelector="R" yChannelSelector="G"/>
     </filter>', freq, as.integer(seed), scale)
  xml2::xml_add_child(defs, xml2::read_xml(filt))

  kids <- xml2::xml_children(root)
  move_idx <- !(xml2::xml_name(kids) %in% c("defs","style"))  # keep CSS at root
  wrap <- xml2::xml_add_child(root, "g")
  xml2::xml_set_attr(wrap, "filter", "url(#ggpatina-wiggle)")
  for (k in kids[move_idx]) xml2::xml_add_child(wrap, k)

  magick::image_read_svg(as.character(doc), width = px_w, height = px_h)
}

# Render a grob with ragg (transparent PNG)
.render_ragg <- function(grob, width, height, dpi) {
  .preq(c("magick","ragg"))
  f <- tempfile(fileext = ".png")
  ragg::agg_png(filename = f, width = width, height = height,
                units = "in", res = dpi, background = "transparent")
  grid::grid.newpage(); grid::grid.draw(grob)
  grDevices::dev.off()
  magick::image_read(f)
}

# Build a 2-channel displacement map (R=dx, G=dy) for magick's "displace"
# freq→sigma is heuristic; tweak if you want a different “grain”
.build_displace_map <- function(px_w, px_h, freq, seed) {
  .preq("magick")
  set.seed(as.integer(seed) %% .Machine$integer.max)
  neutral <- magick::image_blank(px_w, px_h, "gray50")
  r <- magick::image_noise(neutral, "gaussian")
  g <- magick::image_noise(neutral, "gaussian")
  sigma <- max(0.3, (1.2 - max(0.05, min(1, freq))) * 6)  # freq≈0.2→broad, 1→fine
  r <- magick::image_blur(r, 0, sigma)
  g <- magick::image_blur(g, 0, sigma)
  magick::image_combine(c(r, g, neutral), colorspace = "sRGB")
}

# Apply displacement to an image
.apply_displace <- function(img, map, scale) {
  magick::image_composite(img, map, operator = "displace",
                          compose_args = sprintf("%gx%g", scale, scale))
}

# ---- exported wrappers -------------------------------------------------------

#' @rdname hand_drawn
#' @export
hand_drawn_svg <- function(x, width = 6, height = 4, dpi = 300,
                           scale = 2.0, freq = 0.6, seed = 3,
                           affect_text = FALSE, fill_bg = NULL) {

  g <- .to_grob(x)
  g_shapes <- .strip_grob(g, keep = "shapes")
  g_text   <- .strip_grob(g, keep = "text")

  # shapes: SVG warp
  img_shapes <- .warp_svg_shapes(g_shapes, width, height, dpi, scale, freq, seed)

  # text: always via ragg; optionally warp with the same-style displacement
  img_text <- .render_ragg(g_text, width, height, dpi)
  if (isTRUE(affect_text)) {
    img_text <- .warp_raster_with_svg(img_text, width, height, dpi, scale, freq, seed)
  }

  out <- magick::image_composite(img_shapes, img_text, operator = "over")
  if (!is.null(fill_bg)) out <- magick::image_background(out, fill_bg, flatten = TRUE)
  out
}
