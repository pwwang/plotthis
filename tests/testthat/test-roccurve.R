set.seed(8525)
roc_data <- data.frame(
    truth = factor(c(rep("pos", 60), rep("neg", 60))),
    score = c(rnorm(60, mean = 0.7, sd = 0.2), rnorm(60, mean = 0.3, sd = 0.2)),
    score2 = c(rnorm(60, mean = 0.6, sd = 0.25), rnorm(60, mean = 0.4, sd = 0.25)),
    group = factor(rep(c("G1", "G2"), 60))
)

test_that("ROCCurve basic usage works", {
    skip_if_not_installed("pROC")
    p <- ROCCurve(roc_data, truth_by = "truth", score_by = "score", pos_label = "pos")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("ROCCurve with title and subtitle works", {
    skip_if_not_installed("pROC")
    p <- ROCCurve(roc_data, truth_by = "truth", score_by = "score", pos_label = "pos",
                  title = "ROC Title", subtitle = "ROC Subtitle")
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "ROC Title")
    expect_equal(p$labels$subtitle, "ROC Subtitle")
})

test_that("ROCCurve with group_by works", {
    skip_if_not_installed("pROC")
    p <- ROCCurve(roc_data, truth_by = "truth", score_by = c("score", "score2"), pos_label = "pos")
    expect_s3_class(p, "ggplot")
})

test_that("ROCCurve with split_by returns patchwork", {
    skip_if_not_installed("pROC")
    p <- ROCCurve(roc_data, truth_by = "truth", score_by = "score", pos_label = "pos",
                  split_by = "group", combine = TRUE)
    expect_true(inherits(p, "patchwork") || inherits(p, "gg"))
})

test_that("ROCCurve with combine = FALSE returns list", {
    skip_if_not_installed("pROC")
    plots <- ROCCurve(roc_data, truth_by = "truth", score_by = "score", pos_label = "pos",
                      split_by = "group", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
    expect_s3_class(plots[[1]], "ggplot")
})

test_that("ROCCurve with show_auc = 'plot' works", {
    skip_if_not_installed("pROC")
    p <- ROCCurve(roc_data, truth_by = "truth", score_by = "score", pos_label = "pos",
                  show_auc = "plot")
    expect_s3_class(p, "ggplot")
})

test_that("ROCCurve with x_axis_reverse = TRUE works", {
    skip_if_not_installed("pROC")
    p <- ROCCurve(roc_data, truth_by = "truth", score_by = "score", pos_label = "pos",
                  x_axis_reverse = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("ROCCurve with facet_by works", {
    skip_if_not_installed("pROC")
    p <- ROCCurve(roc_data, truth_by = "truth", score_by = "score", pos_label = "pos",
                  facet_by = "group")
    expect_s3_class(p, "ggplot")
})
