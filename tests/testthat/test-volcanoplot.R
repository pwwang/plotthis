set.seed(8525)
n <- 200
data <- data.frame(
    logFC = rnorm(n, mean = 0, sd = 2),
    pvalue = runif(n, min = 1e-10, max = 1),
    gene = paste0("gene", seq_len(n)),
    group = factor(rep(c("g1", "g2"), each = n / 2))
)
rownames(data) <- data$gene

test_that("VolcanoPlot returns a ggplot for basic usage", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0)
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("VolcanoPlot sets title and labels", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                     title = "Volcano", xlab = "log2FC", ylab = "-log10(p)")
    expect_equal(p$labels$title, "Volcano")
    expect_equal(p$labels$x, "log2FC")
    expect_equal(p$labels$y, "-log10(p)")
})

test_that("VolcanoPlot with x_cutoff and y_cutoff works", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                     x_cutoff = 1, y_cutoff = 0.05)
    expect_s3_class(p, "ggplot")
})

test_that("VolcanoPlot with nlabel > 0 works", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 5)
    expect_s3_class(p, "ggplot")
})

test_that("VolcanoPlot with specific labels works", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                     labels = rownames(data)[1:3])
    expect_s3_class(p, "ggplot")
})

test_that("VolcanoPlot with label_by works", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 3, label_by = "gene")
    expect_s3_class(p, "ggplot")
})

test_that("VolcanoPlot with color_by (discrete) works", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0, color_by = "group")
    expect_s3_class(p, "ggplot")
})

test_that("VolcanoPlot with split_by returns combined plot", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                     split_by = "group", combine = TRUE)
    expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})

test_that("VolcanoPlot with split_by and combine = FALSE returns list", {
    plots <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                         split_by = "group", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
})

test_that("VolcanoPlot with multiple split_by columns returns a list with one plot per combination", {
    data2 <- data
    data2$split2 <- factor(rep(c("s1", "s2"), 100))
    plots <- suppressMessages(VolcanoPlot(data2, x = "logFC", y = "pvalue", nlabel = 0,
                                          split_by = c("group", "split2"), combine = FALSE))
    expect_true(is.list(plots))
    expect_length(plots, 4)
    expect_s3_class(plots[[1]], "ggplot")
})

test_that("VolcanoPlot with multiple split_by columns returns combined plot", {
    data2 <- data
    data2$split2 <- factor(rep(c("s1", "s2"), 100))
    p <- suppressMessages(VolcanoPlot(data2, x = "logFC", y = "pvalue", nlabel = 0,
                                      split_by = c("group", "split2"), combine = TRUE))
    expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})

test_that("VolcanoPlot with facet_by works", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                     facet_by = "group")
    expect_s3_class(p, "ggplot")
})

test_that("VolcanoPlot with trim works", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                     trim = c(0.01, 0.99))
    expect_s3_class(p, "ggplot")
})

test_that("VolcanoPlot errors on invalid trim", {
    expect_error(
        VolcanoPlot(data, x = "logFC", y = "pvalue", trim = c(0, 1, 0.5)),
        "trim"
    )
})

test_that("VolcanoPlot with flip_negatives works", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                     flip_negatives = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("VolcanoPlot with highlight works", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                     highlight = rownames(data)[1:5])
    expect_s3_class(p, "ggplot")
})

test_that("VolcanoPlot width/height attributes are numeric", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0)
    expect_true(is.numeric(attr(p, "width")))
    expect_true(is.numeric(attr(p, "height")))
})

test_that("VolcanoPlot with raster = TRUE uses scattermore layers", {
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0, raster = TRUE)
    expect_s3_class(p, "ggplot")
    expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomScattermore"), logical(1))))
    # highlight stays as sharp vector points
    p2 <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                      raster = TRUE, highlight = rownames(data)[1:5])
    expect_s3_class(p2, "ggplot")
})

test_that("VolcanoPlot raster border uses scattermore discs following pt_border_color/pt_border_size", {
    n_scatter <- function(p) sum(vapply(p$layers, function(l) inherits(l$geom, "GeomScattermore"), logical(1)))
    # default shape 21 + TRUE border: border disc + panel-colour erase disc +
    # translucent fill dot per data subset
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0, raster = TRUE)
    expect_equal(n_scatter(p), 9)
    # border disabled (FALSE / size 0 / shape without fill): fill dots only
    p0 <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                      raster = TRUE, pt_border_color = FALSE)
    expect_equal(n_scatter(p0), 3)
    p1 <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                      raster = TRUE, pt_shape = 16)
    expect_equal(n_scatter(p1), 3)
    p3 <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                      raster = TRUE, pt_border_size = 0)
    expect_equal(n_scatter(p3), 3)
    # constant colour border: opaque border disc with that colour, larger than
    # the translucent fill dot; the erase disc matches the panel background
    p2 <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                      raster = TRUE, pt_border_color = "black")
    expect_equal(n_scatter(p2), 9)
    discs <- p2$layers[vapply(p2$layers, function(l) {
        inherits(l$geom, "GeomScattermore") && is.null(l$mapping$colour) &&
            identical(l$aes_params$colour, "black") && identical(l$aes_params$alpha, 1)
    }, logical(1))]
    expect_length(discs, 3)
    fills <- p2$layers[vapply(p2$layers, function(l) {
        inherits(l$geom, "GeomScattermore") && !is.null(l$mapping$colour) &&
            identical(l$aes_params$alpha, 0.5)
    }, logical(1))]
    expect_length(fills, 3)
    expect_true(all(mapply(
        function(d, f) d$geom_params$pointsize > f$geom_params$pointsize,
        discs, fills
    )))
    erases <- p2$layers[vapply(p2$layers, function(l) {
        inherits(l$geom, "GeomScattermore") && is.null(l$mapping$colour) &&
            !identical(l$aes_params$colour, "black") && identical(l$aes_params$alpha, 1)
    }, logical(1))]
    expect_length(erases, 3)
})

test_that("VolcanoPlot pt_shape/pt_border_color control vector point layers", {
    # default shape 21 + TRUE border: single layer with fill+colour mapped
    p <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                     color_by = "group")
    point_layers <- p$layers[vapply(p$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))]
    expect_true(any(vapply(point_layers, function(l) !is.null(l$mapping$fill), logical(1))))
    expect_true(any(vapply(point_layers, function(l) !is.null(l$mapping$colour), logical(1))))
    # shape without fill: colour aesthetic instead
    p1 <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                      color_by = "group", pt_shape = 16)
    point_layers1 <- p1$layers[vapply(p1$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))]
    expect_true(any(vapply(point_layers1, function(l) !is.null(l$mapping$colour), logical(1))))
    # constant border colour: translucent fill + opaque ring on top
    p2 <- VolcanoPlot(data, x = "logFC", y = "pvalue", nlabel = 0,
                      color_by = "group", pt_border_color = "black")
    point_layers2 <- p2$layers[vapply(p2$layers, function(l) inherits(l$geom, "GeomPoint"), logical(1))]
    expect_true(any(vapply(point_layers2, function(l) identical(l$aes_params$colour, "black"), logical(1))))
    expect_true(any(vapply(point_layers2, function(l) identical(l$aes_params$alpha, 1), logical(1))))
    expect_true(any(vapply(point_layers2, function(l) identical(l$aes_params$stroke, 0.5), logical(1))))
})

test_that("VolcanoPlot raster auto-enables above 1e3 points", {
    big <- rbind(data, data, data, data, data, data, data, data, data, data, data, data)[seq_len(1200), ]
    p <- VolcanoPlot(big, x = "logFC", y = "pvalue", nlabel = 0)
    expect_true(any(vapply(p$layers, function(l) inherits(l$geom, "GeomScattermore"), logical(1))))
    # single raster_dpi value is recycled
    p2 <- VolcanoPlot(big, x = "logFC", y = "pvalue", nlabel = 0, raster_dpi = 256)
    expect_s3_class(p2, "ggplot")
})
