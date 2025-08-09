
#' Optional: vector-stage hand-drawn wobble via SVG filters, then rasterize
#' @param x ggplot or grob
#' @param width,height inches
#' @export
hand_drawn_svg <- function(x, width = 6, height = 4) {
  if (!requireNamespace("svglite", quietly = TRUE) ||
      !requireNamespace("xml2", quietly = TRUE) ||
      !requireNamespace("rsvg", quietly = TRUE)) {
    stop("Requires svglite, xml2, rsvg.")
  }
  svg_txt <- svglite::stringSVG(
    print(if (inherits(x, "ggplot")) x else grid::grid.draw(x)),
    width = width, height = height
  )
  doc <- xml2::read_xml(svg_txt)

  defs <- xml2::read_xml('
    <defs>
      <filter id="wiggle" x="-10%" y="-10%" width="120%" height="120%">
        <feTurbulence type="fractalNoise" baseFrequency="0.9" numOctaves="1" seed="3" result="noise"/>
        <feDisplacementMap in="SourceGraphic" in2="noise" scale="0.6" xChannelSelector="R" yChannelSelector="G"/>
      </filter>
    </defs>
  ')
  xml2::xml_add_child(xml2::xml_find_first(doc, "//svg"), defs)

  nodes <- xml2::xml_find_all(doc, "//*[@stroke or name()='path' or name()='polyline' or name()='line']")
  xml2::xml_set_attr(nodes, "filter", "url(#wiggle)")

  raw <- charToRaw(as.character(doc))
  png <- rsvg::rsvg_png(raw)
  magick::image_read(png)
}
