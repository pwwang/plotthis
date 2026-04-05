set.seed(8525)
data <- data.frame(
    x = factor(rep(c("A", "B", "C", "D"), each = 25), levels = c("A", "B", "C", "D")),
    y = rnorm(100),
    size_col = runif(100, 1, 10),
    group1 = factor(rep(c("g1", "g2"), 50)),
    group2 = factor(rep(c("f1", "f2"), each = 50))
)

test_that("JitterPlot returns a ggplot for basic usage", {
    p <- JitterPlot(data, x = "x", y = "y")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("JitterPlot sets title and labels", {
    p <- JitterPlot(data, x = "x", y = "y",
                    title = "Jitter", xlab = "X", ylab = "Y")
    expect_equal(p$labels$title, "Jitter")
    expect_equal(p$labels$x, "X")
    expect_equal(p$labels$y, "Y")
})

test_that("JitterPlot with group_by works", {
    p <- JitterPlot(data, x = "x", y = "y", group_by = "group1")
    expect_s3_class(p, "ggplot")
})

test_that("JitterPlot with split_by and combine = FALSE returns list", {
    plots <- JitterPlot(data, x = "x", y = "y", split_by = "group2", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
    expect_s3_class(plots[[1]], "ggplot")
})

test_that("JitterPlot with facet_by works", {
    p <- JitterPlot(data, x = "x", y = "y", facet_by = "group2")
    expect_s3_class(p, "ggplot")
})

test_that("JitterPlot with flip = TRUE works", {
    p <- JitterPlot(data, x = "x", y = "y", flip = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("JitterPlot with size_by column works", {
    p <- JitterPlot(data, x = "x", y = "y", size_by = "size_col")
    expect_s3_class(p, "ggplot")
})

test_that("JitterPlot with sort_x works", {
    p <- JitterPlot(data, x = "x", y = "y", sort_x = "mean_asc")
    expect_s3_class(p, "ggplot")
})

test_that("JitterPlot with add_bg = TRUE works", {
    p <- JitterPlot(data, x = "x", y = "y", add_bg = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("JitterPlot with highlight expression works", {
    p <- JitterPlot(data, x = "x", y = "y", highlight = "y > 1")
    expect_s3_class(p, "ggplot")
})

test_that("JitterPlot width/height attributes are numeric", {
    p <- JitterPlot(data, x = "x", y = "y")
    expect_true(is.numeric(attr(p, "width")))
    expect_true(is.numeric(attr(p, "height")))
})
