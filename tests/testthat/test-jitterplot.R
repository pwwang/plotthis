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

test_that("JitterPlot with multiple split_by columns returns a list with one plot per combination", {
    plots <- suppressMessages(JitterPlot(data, x = "x", y = "y", split_by = c("group1", "group2"), combine = FALSE))
    expect_true(is.list(plots))
    expect_length(plots, 4)
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

test_that("JitterPlot raster = TRUE uses scattermore layers", {
    n_sc <- function(p) sum(vapply(p$layers, function(l) {
        inherits(l$geom, "GeomScattermore")
    }, logical(1)))
    # default border "black": border disc + panel-colour erase disc +
    # translucent fill dot
    p <- JitterPlot(data, x = "x", y = "y", raster = TRUE)
    expect_equal(n_sc(p), 3)
    expect_false(any(vapply(p$layers, function(l) {
        inherits(l$geom, "GeomPoint")
    }, logical(1))))
    # highlight stays as sharp vector points on top
    p2 <- JitterPlot(data, x = "x", y = "y", raster = TRUE,
                     highlight = c(1, 5, 10))
    expect_equal(n_sc(p2), 3)
    expect_true(any(vapply(p2$layers, function(l) {
        inherits(l$geom, "GeomPoint")
    }, logical(1))))
})

test_that("JitterPlot raster border follows border param", {
    n_sc <- function(p) sum(vapply(p$layers, function(l) {
        inherits(l$geom, "GeomScattermore")
    }, logical(1)))
    # border disabled or shape without fill: fill dots only
    p0 <- JitterPlot(data, x = "x", y = "y", raster = TRUE, border = FALSE)
    expect_equal(n_sc(p0), 1)
    p1 <- JitterPlot(data, x = "x", y = "y", raster = TRUE, shape = 16)
    expect_equal(n_sc(p1), 1)
    # constant colour border: opaque border disc with that colour, larger
    # than the translucent fill dot; the erase disc matches the panel
    p2 <- JitterPlot(data, x = "x", y = "y", raster = TRUE, border = "black")
    expect_equal(n_sc(p2), 3)
    disc <- p2$layers[vapply(p2$layers, function(l) {
        inherits(l$geom, "GeomScattermore") && is.null(l$mapping$colour) &&
            identical(l$aes_params$colour, "black") && identical(l$aes_params$alpha, 1)
    }, logical(1))]
    expect_length(disc, 1)
    fill <- p2$layers[vapply(p2$layers, function(l) {
        inherits(l$geom, "GeomScattermore") && !is.null(l$mapping$colour)
    }, logical(1))]
    expect_length(fill, 1)
    expect_true(disc[[1]]$geom_params$pointsize > fill[[1]]$geom_params$pointsize)
    erase <- p2$layers[vapply(p2$layers, function(l) {
        inherits(l$geom, "GeomScattermore") && is.null(l$mapping$colour) &&
            !identical(l$aes_params$colour, "black") && identical(l$aes_params$alpha, 1)
    }, logical(1))]
    expect_length(erase, 1)
})

test_that("JitterPlot raster works with group_by, split_by and facet_by", {
    n_sc <- function(p) sum(vapply(p$layers, function(l) {
        inherits(l$geom, "GeomScattermore")
    }, logical(1)))
    p <- JitterPlot(data, x = "x", y = "y", group_by = "group1", raster = TRUE)
    expect_equal(n_sc(p), 3)
    expect_s3_class(p, "ggplot")
    plots <- JitterPlot(data, x = "x", y = "y", raster = TRUE,
                        split_by = "group2", combine = FALSE)
    expect_length(plots, 2)
    expect_true(all(vapply(plots, n_sc, numeric(1)) == 3))
    p2 <- JitterPlot(data, x = "x", y = "y", raster = TRUE, facet_by = "group1")
    expect_s3_class(p2, "ggplot")
})

test_that("JitterPlot raster falls back to vector points when size_by is mapped", {
    expect_warning(
        p <- JitterPlot(data, x = "x", y = "y", raster = TRUE, size_by = "size_col"),
        "raster"
    )
    expect_true(any(vapply(p$layers, function(l) {
        inherits(l$geom, "GeomPoint")
    }, logical(1))))
    expect_false(any(vapply(p$layers, function(l) {
        inherits(l$geom, "GeomScattermore")
    }, logical(1))))
})

test_that("JitterPlot raster auto-enables above 1e3 points", {
    big <- do.call(rbind, rep(list(data), 12))[seq_len(1200), ]
    p <- JitterPlot(big, x = "x", y = "y")
    expect_true(any(vapply(p$layers, function(l) {
        inherits(l$geom, "GeomScattermore")
    }, logical(1))))
    # single raster_dpi value is recycled
    p2 <- JitterPlot(big, x = "x", y = "y", raster_dpi = 256)
    expect_s3_class(p2, "ggplot")
})
