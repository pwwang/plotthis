set.seed(8525)
# WordCloudPlot can work with word_by (individual word column)
# or sentence_by (text column that gets tokenized)
wc_data <- data.frame(
    word = c("apple", "banana", "cherry", "date", "elderberry",
             "fig", "grape", "honeydew", "kiwi", "lemon",
             "mango", "nectarine", "orange", "papaya", "quince"),
    count = c(50, 40, 30, 20, 60, 35, 45, 25, 55, 15, 70, 10, 80, 65, 12),
    score = runif(15, -2, 2),
    group = factor(rep(c("G1", "G2"), c(8, 7)))
)

test_that("WordCloudPlot with word_by and count_by works", {
    skip_if_not_installed("ggwordcloud")
    p <- WordCloudPlot(wc_data, word_by = "word", count_by = "count")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("WordCloudPlot with score_by works", {
    skip_if_not_installed("ggwordcloud")
    p <- WordCloudPlot(wc_data, word_by = "word", count_by = "count", score_by = "score")
    expect_s3_class(p, "ggplot")
})

test_that("WordCloudPlot with title and subtitle works", {
    skip_if_not_installed("ggwordcloud")
    p <- WordCloudPlot(wc_data, word_by = "word", count_by = "count",
                       title = "Word Cloud Title", subtitle = "Subtitle")
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Word Cloud Title")
    expect_equal(p$labels$subtitle, "Subtitle")
})

test_that("WordCloudPlot with split_by returns patchwork", {
    skip_if_not_installed("ggwordcloud")
    p <- WordCloudPlot(wc_data, word_by = "word", count_by = "count",
                       split_by = "group", combine = TRUE)
    expect_true(inherits(p, "patchwork") || inherits(p, "gg"))
})

test_that("WordCloudPlot with combine = FALSE returns list", {
    skip_if_not_installed("ggwordcloud")
    plots <- WordCloudPlot(wc_data, word_by = "word", count_by = "count",
                           split_by = "group", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
    expect_s3_class(plots[[1]], "ggplot")
})

test_that("WordCloudPlot with top_words limit works", {
    skip_if_not_installed("ggwordcloud")
    p <- WordCloudPlot(wc_data, word_by = "word", count_by = "count", top_words = 10)
    expect_s3_class(p, "ggplot")
})

test_that("WordCloudPlot with sentence_by works", {
    skip_if_not_installed("ggwordcloud")
    sentence_data <- data.frame(
        text = c(
            "the quick brown fox jumps over the lazy dog",
            "to be or not to be that is the question",
            "all that glitters is not gold",
            "a rose by any other name would smell as sweet"
        )
    )
    p <- WordCloudPlot(sentence_data, sentence_by = "text")
    expect_s3_class(p, "ggplot")
})
