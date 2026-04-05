set.seed(8525)
# ChordPlot expects long-format data with from/to columns
# or can compute counts automatically
chord_data <- data.frame(
    from = factor(c("A", "A", "B", "B", "C", "C", "A", "B", "C"),
                  levels = c("A", "B", "C")),
    to   = factor(c("X", "Y", "X", "Z", "Y", "Z", "Z", "Y", "X"),
                  levels = c("X", "Y", "Z")),
    value = c(10, 20, 15, 8, 12, 18, 5, 9, 14),
    group = factor(c("G1", "G1", "G1", "G1", "G2", "G2", "G2", "G2", "G2"))
)

test_that("ChordPlot basic usage works", {
    p <- ChordPlot(chord_data, from = "from", to = "to")
    expect_true(!is.null(p))
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("ChordPlot with y (value) column works", {
    p <- ChordPlot(chord_data, from = "from", to = "to", y = "value")
    expect_true(!is.null(p))
})

test_that("ChordPlot with title and subtitle works", {
    p <- ChordPlot(chord_data, from = "from", to = "to",
                   title = "Chord Title", subtitle = "Subtitle")
    expect_true(!is.null(p))
    expect_true(!is.null(attr(p, "height")))
})

test_that("ChordPlot with split_by returns patchwork", {
    p <- ChordPlot(chord_data, from = "from", to = "to",
                   split_by = "group", combine = TRUE)
    expect_true(inherits(p, "patchwork") || inherits(p, "gg"))
})

test_that("ChordPlot with combine = FALSE returns list", {
    plots <- ChordPlot(chord_data, from = "from", to = "to",
                       split_by = "group", combine = FALSE)
    expect_true(is.list(plots))
    expect_length(plots, 2)
})

test_that("ChordPlot with flip = TRUE works", {
    p <- ChordPlot(chord_data, from = "from", to = "to", flip = TRUE)
    expect_true(!is.null(p))
})

test_that("ChordPlot with links_color = 'to' works", {
    p <- ChordPlot(chord_data, from = "from", to = "to", links_color = "to")
    expect_true(!is.null(p))
})

test_that("CircosPlot is an alias for ChordPlot", {
    p <- CircosPlot(chord_data, from = "from", to = "to")
    expect_true(!is.null(p))
})
