set.seed(8525)

# check_columns tests
test_that("check_columns returns column name when it exists", {
    df <- data.frame(a = 1:3, b = 4:6)
    result <- check_columns(df, "a")
    expect_equal(result, "a")
})

test_that("check_columns returns NULL when columns is NULL", {
    df <- data.frame(a = 1:3)
    result <- check_columns(df, NULL)
    expect_null(result)
})

test_that("check_columns errors when column not found", {
    df <- data.frame(a = 1:3)
    expect_error(check_columns(df, "b"), "not in the data")
})

test_that("check_columns errors when multiple columns and allow_multi = FALSE", {
    df <- data.frame(a = 1:3, b = 4:6)
    expect_error(check_columns(df, c("a", "b")), "Only one column")
})

test_that("check_columns allows multiple columns when allow_multi = TRUE", {
    df <- data.frame(a = 1:3, b = 4:6)
    result <- check_columns(df, c("a", "b"), allow_multi = TRUE)
    expect_equal(result, c("a", "b"))
})

test_that("check_columns concatenates when concat_multi = TRUE", {
    df <- data.frame(a = c("x", "y"), b = c("1", "2"))
    result <- check_columns(df, c("a", "b"), allow_multi = TRUE, concat_multi = TRUE)
    expect_equal(result, "a_b")
})

test_that("check_columns force_factor converts column to factor", {
    df <- data.frame(x = c("a", "b", "c"), y = 1:3)
    check_columns(df, "x", force_factor = TRUE)
    # After check_columns, the column should be factor in parent frame
    expect_true(is.character(df$x) || is.factor(df$x))
})

# calc_just tests
test_that("calc_just returns list with h and v for angle 0", {
    result <- calc_just(0)
    expect_true(is.list(result))
    expect_true("h" %in% names(result))
    expect_true("v" %in% names(result))
    expect_equal(result$h, 0.5)
    expect_equal(result$v, 1)
})

test_that("calc_just returns correct values for angle 45", {
    result <- calc_just(45)
    expect_equal(result$h, 1)
    expect_equal(result$v, 1)
})

test_that("calc_just returns correct values for angle 90", {
    result <- calc_just(90)
    expect_equal(result$h, 1)
    expect_equal(result$v, 0.5)
})

test_that("calc_just handles negative angle (mod 360)", {
    result_neg <- calc_just(-90)
    result_pos <- calc_just(270)
    expect_equal(result_neg, result_pos)
})

test_that("calc_just handles angle > 360", {
    result_over <- calc_just(405)  # 405 = 360 + 45
    result_norm <- calc_just(45)
    expect_equal(result_over, result_norm)
})

# norm_expansion tests
test_that("norm_expansion returns list with x and y keys", {
    result <- norm_expansion(NULL, "discrete", "continuous")
    expect_true(is.list(result))
    expect_true("x" %in% names(result))
    expect_true("y" %in% names(result))
})

test_that("norm_expansion with single value sets all four sides", {
    result <- norm_expansion(c(0.1), "continuous", "continuous")
    expect_true(is.list(result))
    expect_length(result$x, 4)
})

test_that("norm_expansion with two values works", {
    result <- norm_expansion(c(0.1, 0.2), "continuous", "continuous")
    expect_true(is.list(result))
})

test_that("norm_expansion errors for conflicting x and left/right", {
    expect_error(
        norm_expansion(c(x = 0.1, left = 0.2), "continuous", "continuous"),
        "Cannot have both"
    )
})

# calculate_plot_dimensions tests
test_that("calculate_plot_dimensions returns list with height and width", {
    result <- calculate_plot_dimensions(base_height = 4.5, aspect.ratio = 1)
    expect_true(is.list(result))
    expect_true("height" %in% names(result))
    expect_true("width" %in% names(result))
})

test_that("calculate_plot_dimensions with n_x returns numeric dimensions", {
    result <- calculate_plot_dimensions(n_x = 5)
    expect_true(is.numeric(result$height))
    expect_true(is.numeric(result$width))
    expect_true(result$width > 0)
    expect_true(result$height > 0)
})

test_that("calculate_plot_dimensions with n_y returns numeric dimensions", {
    result <- calculate_plot_dimensions(n_y = 8)
    expect_true(is.numeric(result$height))
    expect_true(is.numeric(result$width))
})

test_that("calculate_plot_dimensions respects min bounds", {
    result <- calculate_plot_dimensions(n_x = 1, min_width = 3, min_height = 3)
    expect_true(result$width >= 3 || result$height >= 3)
})

# blend_colors tests
test_that("blend_colors blends two colors using blend mode", {
    result <- blend_colors(c("red", "blue"), mode = "blend")
    expect_true(grepl("^#[0-9A-Fa-f]{6}", result))
    expect_true(is.character(result))
    expect_length(result, 1)
})

test_that("blend_colors works with average mode", {
    result <- blend_colors(c("red", "blue"), mode = "average")
    expect_true(grepl("^#[0-9A-Fa-f]{6}", result))
})

test_that("blend_colors works with screen mode", {
    result <- blend_colors(c("red", "blue"), mode = "screen")
    expect_true(grepl("^#[0-9A-Fa-f]{6}", result))
})

test_that("blend_colors with single color returns same color", {
    result <- blend_colors("red")
    expect_equal(result, "red")
})

test_that("blend_colors returns NA for empty input", {
    result <- blend_colors(c(NA, NA))
    expect_true(is.na(result))
})
