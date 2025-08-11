#' Newspaper-style theme (off-white, crisp axes, dashed majors)
#'
#' @param base_size base text size
#' @param base_family font family (optional)
#' @param bg panel background color
#' @param paper overall page color
#' @param grid_col major grid line color
#' @export
theme_newspaper <- function(base_size = 11, base_family = NULL,
                            bg = "#f7f3e8", paper = "#f5f0e6",
                            grid_col = "#6b6b6b") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = paper, color = NA),
      panel.background = ggplot2::element_rect(fill = bg, color = NA),
      panel.grid.major = ggplot2::element_line(color = grid_col, linewidth = 0.3, linetype = "dashed"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line        = ggplot2::element_line(color = "#111111", linewidth = 0.4),
      axis.ticks       = ggplot2::element_line(color = "#111111", linewidth = 0.3),
      strip.background = ggplot2::element_rect(fill = "#e8e0cf", color = "#111111", linewidth = 0.3),
      strip.text       = ggplot2::element_text(margin = ggplot2::margin(4,4,4,4)),
      plot.margin      = ggplot2::margin(10, 12, 12, 12)
    )
}

#' Newscast/CRT theme (dark UI, light ink, faint grid)
#'
#' @param base_size base text size
#' @param base_family font family (optional)
#' @param bg panel background
#' @param page overall background
#' @param grid_col grid color (low alpha)
#' @export
theme_newscast <- function(base_size = 11, base_family = NULL,
                           bg = "#151515", page = "#111111",
                           grid_col = "#ffffff22") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = page, color = NA),
      panel.background = ggplot2::element_rect(fill = bg,   color = NA),
      panel.grid.major = ggplot2::element_line(color = grid_col, linewidth = 0.35),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text        = ggplot2::element_text(color = "#f2f2f2"),
      axis.title       = ggplot2::element_text(color = "#ffffff"),
      axis.line        = ggplot2::element_line(color = "#f0f0f0", linewidth = 0.4),
      axis.ticks       = ggplot2::element_line(color = "#e8e8e8", linewidth = 0.3),
      legend.background= ggplot2::element_rect(fill = "transparent", color = NA),
      legend.key       = ggplot2::element_rect(fill = "transparent", color = NA),
      strip.background = ggplot2::element_rect(fill = "#1c1c1c", color = "#303030"),
      strip.text       = ggplot2::element_text(color = "#ffffff"),
      plot.margin      = ggplot2::margin(8, 10, 10, 10)
    )
}

#' Educational-film theme (warm page, dotted majors, gentle border)
#'
#' @param base_size base text size
#' @param base_family font family (optional)
#' @param bg panel background
#' @param page overall background
#' @param grid_col major grid color
#' @export
theme_edu_film <- function(base_size = 11, base_family = NULL,
                           bg = "#e9e1c9", page = "#e5dcc5",
                           grid_col = "#5b533c66") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = page, color = NA),
      panel.background = ggplot2::element_rect(fill = bg,   color = NA),
      panel.grid.major = ggplot2::element_line(color = grid_col, linewidth = 0.35, linetype = "dotted"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line        = ggplot2::element_line(color = "#403a2a88", linewidth = 0.4),
      axis.ticks       = ggplot2::element_line(color = "#403a2a88", linewidth = 0.3),
      axis.text        = ggplot2::element_text(color = "#2e2a1f"),
      axis.title       = ggplot2::element_text(color = "#2e2a1f"),
      panel.border     = ggplot2::element_rect(fill = NA, color = "#403a2a44", linewidth = 0.4),
      strip.background = ggplot2::element_rect(fill = "#ddd1ad", color = "#403a2a66"),
      strip.text       = ggplot2::element_text(margin = ggplot2::margin(3,5,3,5)),
      plot.margin      = ggplot2::margin(10, 12, 14, 12)
    )
}

#' Transparency-slide theme (clean white, thin grid, generous margins)
#'
#' @param base_size base text size
#' @param base_family font family (optional)
#' @param page page background
#' @param grid_col grid color
#' @export
theme_transparency <- function(base_size = 11, base_family = NULL,
                               page = "white", grid_col = "#bdbdbd66") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = page, color = NA),
      panel.background = ggplot2::element_rect(fill = page, color = NA),
      panel.grid.major = ggplot2::element_line(color = grid_col, linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line        = ggplot2::element_line(color = "#111111", linewidth = 0.4),
      axis.ticks       = ggplot2::element_line(color = "#111111", linewidth = 0.3),
      plot.margin      = ggplot2::margin(16, 18, 16, 18)  # room for vignette/leaks
    )
}

#' Blueprint theme (navy page, cyan lines, high-contrast text)
#'
#' @param base_size base text size
#' @param base_family font family (optional)
#' @param page page/navy background
#' @param ink line/text color
#' @param grid_col grid color
#' @export
theme_blueprint <- function(base_size = 11, base_family = NULL,
                            page = "#0b1d3a", ink = "#e6f2ff",
                            grid_col = "#78b2ff33") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = page, color = NA),
      panel.background = ggplot2::element_rect(fill = page, color = NA),
      panel.grid.major = ggplot2::element_line(color = grid_col, linewidth = 0.4, linetype = "dashed"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text        = ggplot2::element_text(color = ink),
      axis.title       = ggplot2::element_text(color = ink),
      axis.line        = ggplot2::element_line(color = "#9ad1ff", linewidth = 0.5),
      axis.ticks       = ggplot2::element_line(color = "#9ad1ff", linewidth = 0.4),
      strip.background = ggplot2::element_rect(fill = NA, color = "#68aef7"),
      strip.text       = ggplot2::element_text(color = ink),
      legend.text      = ggplot2::element_text(color = ink),
      legend.title     = ggplot2::element_text(color = ink),
      plot.margin      = ggplot2::margin(12, 14, 12, 14)
    )
}
