# ggpatina

Period aesthetics and analog-aging effects for ggplot.

## Install (local)

```r
# unzip ggpatina.zip somewhere, then
install.packages("path/to/ggpatina", repos = NULL, type = "source")
# or devtools::load_all("path/to/ggpatina")
```

## Quick start

```r
library(ggplot2); library(ggpatina)

p <- ggplot(mtcars, aes(wt, mpg)) + geom_point() +
  labs(title = "Fuel economy vs weight") +
  ggpatina::period_font_theme(era = "journal-1960s")

img <- ggpatina::as_magick(p, width = 6, height = 4, dpi = 300)

slide <- ggpatina::slideify_transparency(img)
scan  <- ggpatina::scanify_journal(img)

bleed   <- ggpatina::patina_ink_bleed(img, radius = 2, strength = 0.5)
kchrome <- ggpatina::patina_kodachrome(img, warmth = 0.3, fade = 0.25, bloom = 0.4)
copy    <- ggpatina::patina_photocopy(img, offset = c(4, -3), ghost_opacity = 0.4)
blue    <- ggpatina::patina_blueprint(img, grid = FALSE, line_soften = 0.5)
```
