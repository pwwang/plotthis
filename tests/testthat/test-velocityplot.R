set.seed(8525)
# VelocityPlot takes embedding and v_embedding (both matrix/data.frame)
# group_by is a vector (not a column name)
n_cells <- 100
embedding <- data.frame(
    UMAP1 = rnorm(n_cells),
    UMAP2 = rnorm(n_cells)
)
v_embedding <- data.frame(
    UMAP1 = rnorm(n_cells, sd = 0.1),
    UMAP2 = rnorm(n_cells, sd = 0.1)
)
groups <- factor(sample(c("TypeA", "TypeB", "TypeC"), n_cells, replace = TRUE))

test_that("VelocityPlot with plot_type = 'raw' works", {
    p <- VelocityPlot(embedding, v_embedding, plot_type = "raw")
    expect_s3_class(p, "ggplot")
    expect_true(!is.null(attr(p, "height")))
    expect_true(!is.null(attr(p, "width")))
})

test_that("VelocityPlot with group_by works", {
    p <- VelocityPlot(embedding, v_embedding, plot_type = "raw", group_by = groups)
    expect_s3_class(p, "ggplot")
})

test_that("VelocityPlot with plot_type = 'grid' works", {
    p <- VelocityPlot(embedding, v_embedding, plot_type = "grid")
    expect_s3_class(p, "ggplot")
})

test_that("VelocityPlot with plot_type = 'stream' works", {
    p <- VelocityPlot(embedding, v_embedding, plot_type = "stream")
    expect_s3_class(p, "ggplot")
})

test_that("streamline_width controls stream line thickness", {
    # The stream branch maps `linewidth = after_stat(step)`; scaling `size` instead left the
    # default ggplot linewidth scale in place, so every streamline rendered 1-6 wide and the
    # argument did nothing. The graph has three GeomStreamline layers -- a fixed-width halo, the
    # scaled streamline layer, and a fixed-width arrow layer -- so assertions target the scaled
    # one (the only layer with more than one distinct linewidth).
    scaled_max <- function(w) {
        p <- VelocityPlot(embedding, v_embedding, plot_type = "stream",
                          streamline_width = w)
        expect_true(any(vapply(p$scales$scales,
            function(s) "linewidth" %in% s$aesthetics, logical(1))))
        expect_false(any(vapply(p$scales$scales,
            function(s) "size" %in% s$aesthetics, logical(1))))
        lws <- lapply(ggplot2::ggplot_build(p)$data, function(d) {
            if ("linewidth" %in% names(d)) unique(d$linewidth) else numeric()
        })
        scaled <- Filter(function(v) length(v) > 1, lws)
        expect_length(scaled, 1L)
        max(scaled[[1]])
    }
    thin <- scaled_max(c(0, 0.2))
    thick <- scaled_max(c(0, 0.8))
    expect_lte(thin, 0.2)
    expect_lte(thick, 0.8)
    expect_lt(thin, thick)
})

test_that("VelocityPlot with title and subtitle works", {
    p <- VelocityPlot(embedding, v_embedding, plot_type = "raw",
                      title = "Velocity Title", subtitle = "Subtitle")
    expect_s3_class(p, "ggplot")
    expect_equal(p$labels$title, "Velocity Title")
    expect_equal(p$labels$subtitle, "Subtitle")
})

test_that("VelocityPlot with matrix inputs works", {
    emb_mat <- as.matrix(embedding)
    v_mat <- as.matrix(v_embedding)
    p <- VelocityPlot(emb_mat, v_mat, plot_type = "raw")
    expect_s3_class(p, "ggplot")
})

test_that("VelocityPlot errors on mismatched dimensions", {
    expect_error(VelocityPlot(embedding, v_embedding[1:50, ], plot_type = "raw"))
})
