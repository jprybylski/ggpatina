test_that("as_magick requires showtext", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("magick")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  withr::with_libpaths(tempdir(), {
    if ("showtext" %in% loadedNamespaces()) unloadNamespace("showtext")
    expect_error(
      as_magick(p, width = 1, height = 1, dpi = 72),
      "Please install 'showtext'",
      fixed = TRUE
    )
  }, action = "replace")
})

test_that("patina_blueprint handles grid and texture", {
  skip_if_not_installed("magick")
  img <- magick::image_blank(50, 50, "white")
  tex <- magick::image_blank(50, 50, "grey90")
  tf <- tempfile(fileext = ".png")
  magick::image_write(tex, path = tf)
  out <- patina_blueprint(img, grid = TRUE, paper_texture = tf)
  expect_s3_class(out, "magick-image")
})

test_that("patina_edu_film adds dust and scratches", {
  skip_if_not_installed("magick")
  img <- magick::image_blank(80, 80, "white")
  out <- patina_edu_film(img, dust_n = 1, scratches = 1, seed = 123,
                         dust_polarity = "black")
  expect_s3_class(out, "magick-image")
})

test_that("animate_edu_film creates multiple frames", {
  skip_if_not_installed("magick")
  img <- magick::image_blank(50, 50, "white")
  anim <- animate_edu_film(img, n_frames = 3, dust = 0)
  expect_s3_class(anim, "magick-image")
  info <- magick::image_info(anim)
  expect_gt(nrow(info), 1)
})

test_that("patina_ink_bleed supports paper texture", {
  skip_if_not_installed("magick")
  img <- magick::image_blank(50, 50, "white")
  tex <- magick::image_blank(50, 50, "grey90")
  tf <- tempfile(fileext = ".png")
  magick::image_write(tex, tf)
  out <- patina_ink_bleed(img, paper_texture = tf)
  expect_s3_class(out, "magick-image")
})

test_that("patina_kodachrome outputs magick image", {
  skip_if_not_installed("magick")
  img <- magick::image_blank(100, 100, "white")
  out <- patina_kodachrome(img)
  expect_s3_class(out, "magick-image")
})

test_that("patina_newscast corner warp", {
  skip_if_not_installed("magick")
  img <- magick::image_blank(60, 60, "white")
  out <- patina_newscast(img, warp = "corner", corner = "nw")
  expect_s3_class(out, "magick-image")
})

test_that("patina_newspaper supports paper and misregister", {
  skip_if_not_installed("magick")
  img <- magick::image_blank(80, 80, "white")
  tex <- magick::image_blank(80, 80, "#dddddd")
  tf <- tempfile(fileext = ".png")
  magick::image_write(tex, path = tf)
  out <- patina_newspaper(img, misregister_px = 1, paper = tf)
  expect_s3_class(out, "magick-image")
})

test_that("patina_photocopy outputs magick image", {
  skip_if_not_installed("magick")
  img <- magick::image_blank(100, 100, "white")
  out <- patina_photocopy(img)
  expect_s3_class(out, "magick-image")
})

test_that("hand_drawn_wiggle auto mask", {
  skip_if_not_installed("magick")
  img <- magick::image_blank(40, 40, "white")
  out <- hand_drawn_wiggle(img, affect_text = FALSE, focus = "auto_image",
                           fill_bg = "white")
  expect_s3_class(out, "magick-image")
})

test_that("slideify_transparency outputs magick image", {
  skip_if_not_installed("magick")
  img <- magick::image_blank(80, 80, "white")
  out <- slideify_transparency(img)
  expect_s3_class(out, "magick-image")
})

test_that("scanify_journal outputs magick image", {
  skip_if_not_installed("magick")
  img <- magick::image_blank(80, 80, "white")
  out <- scanify_journal(img, dither = FALSE, sepia = FALSE)
  expect_s3_class(out, "magick-image")
})

test_that("scanify_journal supports dither and sepia", {
  skip_if_not_installed("magick")
  img <- magick::image_blank(30, 30, "white")
  out <- scanify_journal(img, dither = TRUE, sepia = TRUE)
  expect_s3_class(out, "magick-image")
})
