data("enrich_example", package = "plotthis")
data("enrich_multidb_example", package = "plotthis")

test_that("EnrichMap basic usage with clusterProfiler data works", {
    p <- EnrichMap(enrich_example)
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("EnrichMap with title and subtitle works", {
    p <- EnrichMap(enrich_example, title = "EnrichMap Title", subtitle = "Subtitle")
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "EnrichMap Title")
    expect_equal(p$labels$subtitle, "Subtitle")
})

test_that("EnrichMap with top_term parameter works", {
    p <- EnrichMap(enrich_example, top_term = 5)
    expect_s3_class(p, "ggplot")
})

test_that("EnrichMap with split_by returns patchwork", {
    p <- EnrichMap(enrich_multidb_example, split_by = "Database", combine = TRUE)
    expect_true(inherits(p, "patchwork") || inherits(p, "gg"))
})

test_that("EnrichMap with split_by combine = FALSE returns list", {
    plots <- EnrichMap(enrich_multidb_example, split_by = "Database", combine = FALSE)
    expect_true(is.list(plots))
    expect_s3_class(plots[[1]], "ggplot")
})

test_that("EnrichMap with cluster = 'walktrap' works", {
    p <- EnrichMap(enrich_example, cluster = "walktrap")
    expect_s3_class(p, "ggplot")
})

test_that("EnrichMap with show_keyword = TRUE works", {
    p <- EnrichMap(enrich_example, show_keyword = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("EnrichNetwork basic usage works", {
    p <- EnrichNetwork(enrich_example)
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("EnrichNetwork with title works", {
    p <- EnrichNetwork(enrich_example, title = "EnrichNetwork Title")
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "EnrichNetwork Title")
})

test_that("EnrichNetwork with top_term parameter works", {
    p <- EnrichNetwork(enrich_example, top_term = 5)
    expect_s3_class(p, "ggplot")
})

test_that("EnrichNetwork with split_by returns patchwork", {
    p <- EnrichNetwork(enrich_multidb_example, split_by = "Database", combine = TRUE)
    expect_true(inherits(p, "patchwork") || inherits(p, "gg"))
})

test_that("EnrichNetwork with combine = FALSE returns list", {
    plots <- EnrichNetwork(enrich_multidb_example, split_by = "Database", combine = FALSE)
    expect_true(is.list(plots))
    expect_s3_class(plots[[1]], "ggplot")
})
