set.seed(8525)
# AreaPlot data: x = time points, group_by = group, y = value (if provided)
data <- data.frame(
    time = factor(rep(paste0("T", 1:6), each = 20), levels = paste0("T", 1:6)),
    group = factor(rep(c("A", "B", "C", "D"), 30)),
    value = runif(120, 0, 100),
    facet = factor(rep(c("f1", "f2"), 60))
)

test_that("AreaPlot returns a ggplot for basic usage", {
    p <- AreaPlot(data, x = "time", group_by = "group")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("AreaPlot works with y column", {
    p <- AreaPlot(data, x = "time", y = "value", group_by = "group")
    expect_s3_class(p, "ggplot")
})

test_that("AreaPlot works without group_by", {
    p <- AreaPlot(data, x = "time", y = "value")
    expect_s3_class(p, "ggplot")
})

test_that("AreaPlot sets title and labels", {
    p <- AreaPlot(data, x = "time", group_by = "group",
                  title = "Area Title", xlab = "Time", ylab = "Count")
    expect_equal(p$labels$title, "Area Title")
})

test_that("AreaPlot with scale_y = TRUE works", {
    p <- AreaPlot(data, x = "time", group_by = "group", scale_y = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("AreaPlot with split_by returns combined plot", {
    p <- AreaPlot(data, x = "time", group_by = "group",
                  split_by = "facet", combine = TRUE)
    expect_true(inherits(p, "gg") || inherits(p, "patchwork"))
})

test_that("AreaPlot with split_by and combine = FALSE returns list", {
    plots <- AreaPlot(data, x = "time", group_by = "group",
                      split_by = "facet", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
})

test_that("AreaPlot with facet_by works", {
    p <- AreaPlot(data, x = "time", group_by = "group", facet_by = "facet")
    expect_s3_class(p, "ggplot")
})

test_that("AreaPlot width/height attributes are numeric", {
    p <- AreaPlot(data, x = "time", group_by = "group")
    expect_true(is.numeric(attr(p, "width")))
    expect_true(is.numeric(attr(p, "height")))
})
