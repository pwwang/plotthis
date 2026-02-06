# Basic functionality tests
test_that("palette_this works with discrete values", {
    colors <- palette_this(c("a", "b", "c"), palette = "Paired")
    expect_length(colors, 3)
    expect_named(colors, c("a", "b", "c"))
    expect_true(all(grepl("^#[0-9A-F]{6}$", colors)))
})

test_that("palette_this works with continuous values", {
    colors <- palette_this(1:10, n = 10, palette = "Spectral")
    expect_length(colors, 10)
    expect_true(all(grepl("^#[0-9A-F]{6}$", colors)))
})

test_that("palette_this auto-detects type", {
    discrete <- palette_this(c("a", "b", "c"), type = "auto")
    expect_named(discrete, c("a", "b", "c"))

    continuous <- palette_this(1:5, type = "auto", n = 5)
    expect_length(continuous, 5)
})

test_that("palette_this works with missing x", {
    colors <- palette_this(n = 50, palette = "Spectral")
    expect_length(colors, 50)
})

# Named palcolor tests (new behavior)
test_that("palette_this works with named palcolor - replaces specific colors", {
    colors <- palette_this(
        c("a", "b", "c"),
        palette = "Paired",
        palcolor = c("c" = "red", "b" = "blue", "a" = "green")
    )
    colors <- as.list(colors)[sort(names(colors))]
    expect_equal(colors, list(a = "green", b = "blue", c = "red"))
})

test_that("palette_this works with named palcolor but missed items - uses palette for missing", {
    colors <- palette_this(
        c("a", "b", "c"),
        palette = "Paired",
        palcolor = c("b" = "blue", "a" = "green")
    )
    expect_equal(colors[["a"]], "green")
    expect_equal(colors[["b"]], "blue")
    # c should get color from Paired palette
    expect_true(grepl("^#[0-9A-F]{6}$", colors[["c"]]))
})

test_that("palette_this partial named palcolor uses palette as base", {
    colors <- palette_this(
        c("a", "b", "c", "d"),
        palette = "Paired",
        palcolor = c("c" = "red")
    )
    expect_equal(colors[["c"]], "red")
    expect_named(colors, c("a", "b", "c", "d"))
    expect_true(all(grepl("^#[0-9A-F]{6}$", colors[c("a", "b", "d")])))
})

# Positional palcolor tests (continuous) - evenly distributed replacement
test_that("palette_this works with positional palcolor for continuous - evenly distributed", {
    # Get base palette colors
    base_palette <- plotthis::palette_list[["Spectral"]]

    # With 3 palcolor values, they should replace positions 1, middle, and last
    # in the base palette evenly
    colors_base <- palette_this(1:100, n = 100, palette = "Spectral")
    colors_custom <- palette_this(1:100, n = 100, palette = "Spectral", palcolor = c("red", NA, "blue"))

    # First and last should be different due to replacement
    expect_false(colors_custom[[1]] == colors_base[[1]])
    expect_false(colors_custom[[100]] == colors_base[[100]])
})

test_that("palette_this positional replacement evenly distributes in base palette", {
    # Test that replacement happens in the base palette, not final colors
    # With a 2-value palcolor, should replace first and last of base palette
    colors <- palette_this(1:100, n = 50, palette = "Spectral", palcolor = c("yellow", "darkred"))
    expect_length(colors, 50)
    # The gradient should go from yellow to darkred
    expect_true(grepl("^#[0-9A-F]{6}$", colors[[1]]))
    expect_true(grepl("^#[0-9A-F]{6}$", colors[[50]]))
})

test_that("palette_this evenly distributed replacement with 3 values", {
    # With 3 values: first, middle, last of base palette
    colors <- palette_this(1:100, n = 20, palette = "Spectral", palcolor = c("red", NA, "blue"))
    expect_length(colors, 20)
    # Should create a smooth gradient
    expect_true(all(grepl("^#[0-9A-F]{6}$", colors)))
})

# NA handling tests
test_that("palette_this handles NA values with default NA_color", {
    colors <- palette_this(c("a", "b", NA), palette = "Paired", NA_keep = TRUE)
    expect_length(colors, 3)
    expect_equal(colors[["NA"]], "grey80")
})

test_that("palette_this handles NA values with custom NA_color", {
    colors <- palette_this(c("a", "b", NA), palette = "Paired", NA_keep = TRUE, NA_color = "purple")
    expect_length(colors, 3)
    expect_equal(colors[["NA"]], "purple")
})

test_that("palette_this handles NA color from palcolor", {
    colors <- palette_this(
        c("a", "b", NA),
        palette = "Paired",
        palcolor = c("a" = "red", "NA" = "orange"),
        NA_keep = TRUE
    )
    expect_length(colors, 3)
    expect_equal(colors[["a"]], "red")
    expect_equal(colors[["NA"]], "orange")
})

test_that("palette_this NA_keep = FALSE removes NA color", {
    colors <- palette_this(c("a", "b", NA), palette = "Paired", NA_keep = FALSE)
    expect_length(colors, 2)
    expect_false("NA" %in% names(colors))
})

test_that("palette_this continuous with NA uses palcolor NA", {
    colors <- palette_this(
        c(1, 2, 3, NA),
        n = 3,
        palette = "Spectral",
        palcolor = c("NA" = "yellow"),
        NA_keep = TRUE
    )
    expect_equal(colors[["NA"]], "yellow")
})

# matched parameter tests
test_that("palette_this matched = TRUE returns color per value", {
    x <- c("a", "b", "c", "a", "b")
    colors <- palette_this(x, palette = "Paired", matched = TRUE)
    expect_length(colors, 5)
    expect_equal(colors[[1]], colors[[4]])
    expect_equal(colors[[2]], colors[[5]])
})

test_that("palette_this matched = FALSE returns unique colors", {
    x <- c("a", "b", "c", "a", "b")
    colors <- palette_this(x, palette = "Paired", matched = FALSE)
    expect_length(colors, 3)
    expect_named(colors, c("a", "b", "c"))
})

test_that("palette_this matched with NA values", {
    x <- c("a", "b", NA, "a")
    colors <- palette_this(x, palette = "Paired", matched = TRUE, NA_keep = TRUE)
    expect_length(colors, 4)
    expect_equal(colors[[3]], "grey80")
})

# reverse parameter tests
test_that("palette_this reverse = TRUE inverts color order", {
    colors_normal <- palette_this(c("a", "b", "c"), palette = "Paired", reverse = FALSE)
    colors_reversed <- palette_this(c("a", "b", "c"), palette = "Paired", reverse = TRUE)
    expect_equal(colors_normal[["a"]], colors_reversed[["c"]])
    expect_equal(colors_normal[["c"]], colors_reversed[["a"]])
})

test_that("palette_this reverse works with continuous", {
    colors_normal <- palette_this(n = 5, palette = "Spectral", reverse = FALSE)
    colors_reversed <- palette_this(n = 5, palette = "Spectral", reverse = TRUE)
    expect_equal(colors_normal[[1]], colors_reversed[[5]])
})

# keep_names parameter tests
test_that("palette_this keep_names = FALSE removes names", {
    colors <- palette_this(c("a", "b", "c"), palette = "Paired", keep_names = FALSE)
    expect_null(names(colors))
})

test_that("palette_this keep_names = TRUE keeps names", {
    colors <- palette_this(c("a", "b", "c"), palette = "Paired", keep_names = TRUE)
    expect_named(colors, c("a", "b", "c"))
})

# alpha parameter tests
test_that("palette_this applies alpha with transparent = TRUE", {
    colors <- palette_this(c("a", "b", "c"), palette = "Paired", alpha = 0.5, transparent = TRUE)
    expect_true(all(grepl("^#[0-9A-F]{8}$", colors)))  # 8 characters with alpha
})

test_that("palette_this applies alpha with transparent = FALSE", {
    colors <- palette_this(c("a", "b", "c"), palette = "Paired", alpha = 0.5, transparent = FALSE)
    expect_true(all(grepl("^#[0-9A-F]{6}$", colors)))  # 6 characters, adjusted but not transparent
})

test_that("palette_this alpha = 1 does not modify colors", {
    colors_alpha1 <- palette_this(c("a", "b", "c"), palette = "Paired", alpha = 1)
    colors_default <- palette_this(c("a", "b", "c"), palette = "Paired")
    expect_equal(colors_alpha1, colors_default)
})

# Factor handling tests
test_that("palette_this works with factor input", {
    x <- factor(c("a", "b", "c"), levels = c("c", "b", "a"))
    colors <- palette_this(x, palette = "Paired")
    expect_named(colors, c("c", "b", "a"))
})

test_that("palette_this preserves factor level order", {
    x <- factor(c("a", "b", "c"), levels = c("b", "c", "a"))
    colors <- palette_this(x, palette = "Paired")
    expect_equal(names(colors), c("b", "c", "a"))
})

# Continuous edge cases
test_that("palette_this handles all NA continuous values", {
    colors <- palette_this(c(NA, NA, NA), n = 3, palette = "Spectral", type = "continuous")
    expect_length(colors, 1)
})

test_that("palette_this handles single unique continuous value", {
    colors <- palette_this(c(5, 5, 5), n = 3, palette = "Spectral", type = "continuous")
    expect_length(colors, 1)
})

test_that("palette_this matched continuous returns correct length", {
    x <- c(1, 5, 10, 3, 7)
    colors <- palette_this(x, n = 5, palette = "Spectral", matched = TRUE, type = "continuous")
    expect_length(colors, 5)
})

# Error handling tests
test_that("palette_this throws error for invalid palette", {
    expect_error(
        palette_this(c("a", "b"), palette = "InvalidPalette"),
        "invalid"
    )
})

test_that("palette_this throws error for invalid type", {
    expect_error(
        palette_this(c("a", "b"), type = "invalid"),
        "must be one of"
    )
})

test_that("palette_this throws error for non-numeric continuous", {
    expect_error(
        palette_this(c("a", "b", "c"), type = "continuous"),
        "must be type of numeric"
    )
})

# List palcolor handling
test_that("palette_this handles list palcolor", {
    colors <- palette_this(
        c("a", "b", "c"),
        palette = "Paired",
        palcolor = list("a" = "red", "b" = "blue")
    )
    expect_equal(colors[["a"]], "red")
    expect_equal(colors[["b"]], "blue")
})

# Empty palcolor handling
test_that("palette_this handles empty string palcolor", {
    colors <- palette_this(c("a", "b", "c"), palette = "Paired", palcolor = "")
    expect_length(colors, 3)
    expect_named(colors, c("a", "b", "c"))
})
