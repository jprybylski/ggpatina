#' Hand-drawn wobble via SVG filter, rendered at exact pixels
#' (deps: svglite, xml2, magick; no rsvg)
#'
#' @param x ggplot or grob
#' @param width,height Size in inches
#' @param dpi Output DPI (pixels = width*dpi by height*dpi)
#' @param scale Displacement amplitude in px (increase for more wobble)
#' @param freq  feTurbulence baseFrequency (lower = wider waves)
#' @param seed  Random seed for turbulence
#' @param affect_text Logical; if FALSE, leave text crisp
#' @export
hand_drawn_svg <- function(x, width = 6, height = 4, dpi = 300,
                           scale = 2.0, freq = 0.6, seed = 3,
                           affect_text = TRUE, fill_bg = NULL) {
  if (!requireNamespace("svglite", quietly = TRUE) ||
      !requireNamespace("xml2", quietly = TRUE) ||
      !requireNamespace("magick", quietly = TRUE)) {
    stop("Requires svglite, xml2, magick.")
  }

  # 1) Draw to SVG (coordinates in points = 72/in)
  svg_txt <- svglite::stringSVG(
    code = { if (inherits(x, "ggplot")) print(x) else grid::grid.draw(x) },
    width = width, height = height, bg = "transparent"
  )

  # 2) Parse, inject filter, and wrap content in <g filter=...>
  doc  <- xml2::read_xml(as.character(svg_txt))
  root <- xml2::xml_find_first(doc, '/*[local-name()="svg"]')

  # canvas size in pixels
  px_w <- as.integer(round(width * dpi))
  px_h <- as.integer(round(height * dpi))
  # keep viewBox in points to match svglite's coordinate system
  vb_w <- width * 72
  vb_h <- height * 72

  xml2::xml_set_attr(root, "width",  paste0(px_w, "px"))
  xml2::xml_set_attr(root, "height", paste0(px_h, "px"))
  xml2::xml_set_attr(root, "viewBox", sprintf("0 0 %g %g", vb_w, vb_h))
  xml2::xml_set_attr(root, "preserveAspectRatio", "xMidYMid meet")

  # ensure <defs>
  defs <- xml2::xml_find_first(root, './*[local-name()="defs"]')
  if (inherits(defs, "xml_missing")) defs <- xml2::xml_add_child(root, "defs")

  # add wiggle filter
  filt <- sprintf('
    <filter id="ggpatina-wiggle" x="-10%%" y="-10%%" width="120%%" height="120%%">
      <feTurbulence type="fractalNoise" baseFrequency="%g" numOctaves="1" seed="%d" result="noise"/>
      <feDisplacementMap in="SourceGraphic" in2="noise" scale="%g" xChannelSelector="R" yChannelSelector="G"/>
    </filter>', freq, as.integer(seed), scale)
  xml2::xml_add_child(defs, xml2::read_xml(filt))

  # move children under a filtered <g>
  kids <- xml2::xml_children(root)
  keep <- xml2::xml_name(kids) != "defs"
  wrap <- xml2::xml_add_child(root, "g")
  xml2::xml_set_attr(wrap, "filter", "url(#ggpatina-wiggle)")
  for (k in kids[keep]) xml2::xml_add_child(wrap, k)

  # 3) Rasterize at exact pixel size
  img <- magick::image_read_svg(as.character(doc), width = px_w, height = px_h)

  # Optional: flatten transparency to a background color (e.g., "white")
  if (!is.null(fill_bg)) img <- magick::image_background(img, fill_bg, flatten = TRUE)

  img
}



#' Hand-drawn warp that never touches text (grob split + composite)
#' Uses svglite+xml2+magick for warping shapes, and ragg for crisp text.
#'
#' @param x ggplot or grob
#' @param width,height size in inches
#' @param dpi output DPI (pixels = width*dpi by height*dpi)
#' @param scale displacement amplitude for the wiggle (px-ish)
#' @param freq  feTurbulence baseFrequency (lower = wider waves)
#' @param seed  random seed for turbulence
#' @param bg    background for the final image (e.g., "white" or "transparent")
#' @return magick-image of exact size width*dpi by height*dpi
#' @export
hand_drawn_warp_grob <- function(x, width = 6, height = 4, dpi = 300,
                                 scale = 2.0, freq = 0.6, seed = 3,
                                 bg = "transparent") {
  stopifnot(requireNamespace("svglite", quietly = TRUE),
            requireNamespace("xml2",     quietly = TRUE),
            requireNamespace("magick",   quietly = TRUE))
  # ---- helpers --------------------------------------------------------------
  to_grob <- function(obj) if (inherits(obj, "ggplot")) ggplot2::ggplotGrob(obj) else obj

  is_text_grob <- function(g) {
    any(inherits(g, c("text", "titleGrob", "richtext_grob", "textpath_grob")))
  }

  strip_grob <- function(g, keep = c("shapes","text")) {
    keep <- match.arg(keep)
    if (inherits(g, "gtable")) {
      g$grobs <- lapply(g$grobs, strip_grob, keep = keep)
      return(g)
    }
    if (grid::is.grob(g)) {
      if (inherits(g, "gTree")) {
        g$children <- lapply(g$children, strip_grob, keep = keep)
        return(g)
      }
      txt <- is_text_grob(g) || grepl("(text|label|title|caption)", g$name)
      if (keep == "shapes" && txt) return(grid::nullGrob())
      if (keep == "text"   && !txt) return(grid::nullGrob())
    }
    g
  }

  draw_svg_filtered <- function(grob_shapes) {
    px_w <- as.integer(width * dpi); px_h <- as.integer(height * dpi)
    vb_w <- width * 72;             vb_h <- height * 72

    svg_txt <- svglite::stringSVG(
      code  = grid::grid.draw(grob_shapes),
      width = width, height = height, bg = "transparent"
    )
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
    wrap <- xml2::xml_add_child(root, "g")
    xml2::xml_set_attr(wrap, "filter", "url(#ggpatina-wiggle)")
    for (k in kids[xml2::xml_name(kids) != "defs"]) xml2::xml_add_child(wrap, k)

    magick::image_read_svg(as.character(doc), width = px_w, height = px_h)
  }

  draw_ragg <- function(grob_text) {
    # render text-only with transparent bg so fonts from sysfonts/showtext work
    f <- tempfile(fileext = ".png")
    ragg::agg_png(filename = f, width = width, height = height,
                  units = "in", res = dpi, background = "transparent")
    grid::grid.newpage(); grid::grid.draw(grob_text)
    grDevices::dev.off()
    magick::image_read(f)
  }
  # ---- split & render -------------------------------------------------------
  g <- to_grob(x)
  g_shapes <- strip_grob(g, keep = "shapes")
  g_text   <- strip_grob(g, keep = "text")

  img_shapes <- draw_svg_filtered(g_shapes)     # warped shapes
  img_text   <- draw_ragg(g_text)               # crisp text

  out <- magick::image_composite(img_shapes, img_text, operator = "over")
  magick::image_background(out, bg, flatten = TRUE)
}