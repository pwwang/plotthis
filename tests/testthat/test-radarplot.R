set.seed(8525)
# Long-format data for radar/spider plot:
# x = trait, y = numeric value, group_by = group
radar_data <- data.frame(
    trait = rep(c("Str", "Spd", "Def", "Int", "Luck"), times = 6),
    value = c(80, 60, 70, 90, 50, 70, 85, 65, 75, 55,
              60, 90, 55, 80, 70, 75, 80, 70, 85, 65,
              50, 70, 80, 60, 90, 65, 75, 60, 70, 75),
    group = rep(c("A", "B", "C"), each = 10),
    split = factor(rep(c("S1", "S2"), each = 15))
)

test_that("RadarPlot basic usage works", {
    p <- RadarPlot(radar_data, x = "trait", y = "value")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("RadarPlot with group_by works", {
    p <- RadarPlot(radar_data, x = "trait", y = "value", group_by = "group")
    expect_s3_class(p, "ggplot")
})

test_that("RadarPlot with title and subtitle works", {
    p <- RadarPlot(radar_data, x = "trait", y = "value", group_by = "group",
                   title = "Radar Title", subtitle = "Subtitle")
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Radar Title")
    expect_equal(p$labels$subtitle, "Subtitle")
})

test_that("RadarPlot with split_by returns patchwork", {
    p <- RadarPlot(radar_data, x = "trait", y = "value", group_by = "group",
                   split_by = "split", combine = TRUE)
    expect_true(inherits(p, "patchwork") || inherits(p, "gg"))
})

test_that("RadarPlot with combine = FALSE returns list", {
    plots <- RadarPlot(radar_data, x = "trait", y = "value", group_by = "group",
                       split_by = "split", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
    expect_s3_class(plots[[1]], "ggplot")
})

test_that("RadarPlot with scale_y = 'global' works", {
    p <- RadarPlot(radar_data, x = "trait", y = "value", group_by = "group",
                   scale_y = "global")
    expect_s3_class(p, "ggplot")
})

test_that("RadarPlot with fill = FALSE works", {
    p <- RadarPlot(radar_data, x = "trait", y = "value", group_by = "group",
                   fill = FALSE)
    expect_s3_class(p, "ggplot")
})

test_that("SpiderPlot basic usage works", {
    p <- SpiderPlot(radar_data, x = "trait", y = "value", group_by = "group")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("SpiderPlot with title works", {
    p <- SpiderPlot(radar_data, x = "trait", y = "value", title = "Spider Title")
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Spider Title")
})

test_that("SpiderPlot with split_by works", {
    p <- SpiderPlot(radar_data, x = "trait", y = "value", group_by = "group",
                    split_by = "split", combine = TRUE)
    expect_true(inherits(p, "patchwork") || inherits(p, "gg"))
})
