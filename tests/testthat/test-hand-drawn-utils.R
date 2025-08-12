test_that("hand-drawn masks are generated", {
  skip_if_not_installed("magick")
  m <- ggpatina:::rect_mask_px(10, 10, 2, 2, 8, 8)
  expect_s3_class(m, "magick-image")
  e <- ggpatina:::edge_guard_mask(10, 10)
  expect_s3_class(e, "magick-image")
})

test_that("auto_panel_mask_img detects panel", {
  skip_if_not_installed("magick")
  img <- magick::image_blank(40, 40, "white")
  mask <- ggpatina:::auto_panel_mask_img(img, feather = 0)
  expect_s3_class(mask, "magick-image")
})

test_that("calc_auto_scale and displace_map work", {
  skip_if_not_installed("magick")
  s <- ggpatina:::calc_auto_scale(100, 100, 150, 0.6)
  expect_gt(s, 0)
  map <- ggpatina:::displace_map(20, 20, freq = 0.5, seed = 1)
  expect_s3_class(map, "magick-image")
})

test_that("panel_mask_from_plot returns mask", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("magick")
  skip_if_not_installed("ragg")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  mask <- ggpatina:::panel_mask_from_plot(p, width = 1, height = 1, dpi = 72)
  expect_s3_class(mask, "magick-image")
})
