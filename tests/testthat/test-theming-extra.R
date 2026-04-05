test_that("theme_this returns a ggplot2 theme", {
    t <- theme_this()
    expect_s3_class(t, "theme")
})

test_that("theme_this accepts aspect.ratio", {
    t <- theme_this(aspect.ratio = 1)
    expect_s3_class(t, "theme")
})

test_that("theme_this accepts base_size", {
    t <- theme_this(base_size = 14)
    expect_s3_class(t, "theme")
})

test_that("theme_blank returns a list of ggplot layers", {
    t <- theme_blank()
    expect_true(is.list(t))
    expect_true(length(t) > 0)
})

test_that("theme_blank with add_coord = FALSE returns a list", {
    t <- theme_blank(add_coord = FALSE)
    expect_true(is.list(t))
    expect_true(length(t) > 0)
})

test_that("theme_box returns a list of ggplot layers", {
    t <- theme_box()
    expect_true(is.list(t))
    expect_true(length(t) > 0)
})

test_that("theme_this can be applied to a ggplot", {
    p <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point() +
        theme_this()
    expect_s3_class(p, "ggplot")
})

test_that("theme_blank can be applied to a ggplot", {
    p <- ggplot2::ggplot(data.frame(x = 1:5, y = 1:5), ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point() +
        theme_blank()
    expect_s3_class(p, "ggplot")
})

test_that("show_palettes returns palette names (character)", {
    result <- show_palettes()
    expect_true(is.character(result))
    expect_true(length(result) > 0)
})

test_that("show_palettes with type = 'discrete' works", {
    result <- show_palettes(type = "discrete")
    expect_true(is.character(result))
    expect_true(length(result) > 0)
})

test_that("show_palettes with type = 'continuous' works", {
    result <- show_palettes(type = "continuous")
    expect_true(is.character(result))
    expect_true(length(result) > 0)
})

test_that("palette_list is a valid list", {
    expect_true(is.list(palette_list))
    expect_true(length(palette_list) > 0)
    expect_true("Paired" %in% names(palette_list))
    expect_true("Spectral" %in% names(palette_list))
})
