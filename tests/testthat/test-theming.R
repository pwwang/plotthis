test_that("palette_this works with named palcolor (pwwang/scplotter#12)", {
    colors <- palette_this(
        c("a", "b", "c"),
        palette = "Paired",
        palcolor = c("c" = "red", "b" = "blue", "a" = "green")
    )
    colors <- as.list(colors)[sort(names(colors))]
    expect_equal(colors, list(a = "green", b = "blue", c = "red"))
})

test_that("palette_this works with named palcolor but missed items", {
    colors <- palette_this(
        c("a", "b", "c"),
        palette = "Paired",
        palcolor = c("b" = "blue", "a" = "green")
    )
    colors <- as.list(colors)[sort(names(colors))]
    expect_equal(colors, list(a = "green", b = "blue"))
})
