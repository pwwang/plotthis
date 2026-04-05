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
