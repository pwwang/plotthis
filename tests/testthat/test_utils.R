test_that("check_keep_na works for atomic values", {
    expect_equal(check_keep_na(TRUE), NA)
    expect_equal(check_keep_na(FALSE), FALSE)
    expect_equal(check_keep_na("missing"), "missing")
})

test_that("check_keep_na works for named list", {
    res <- check_keep_na(list(col1 = TRUE, col2 = FALSE, col3 = "missing"))
    expect_equal(res$col1, NA)
    expect_equal(res$col2, FALSE)
    expect_equal(res$col3, "missing")
})

test_that("check_keep_na works with default column", {
    res <- check_keep_na(list(col1 = TRUE, col2 = FALSE), x = "missing")
    expect_equal(res$col1, NA)
    expect_equal(res$col2, FALSE)
    expect_equal(length(res), 2)

    res <- check_keep_na(TRUE, x = "missing")
    expect_equal(res, list(missing = NA))
})

test_that("check_keep_empty works for atomic values", {
    expect_equal(check_keep_empty(TRUE), "true")
    expect_equal(check_keep_empty("true"), "true")
    expect_equal(check_keep_empty(FALSE), "false")
    expect_equal(check_keep_empty("false"), "false")
    expect_equal(check_keep_empty("level"), "level")
    expect_equal(check_keep_empty("levels"), "level")
})

test_that("check_keep_empty works for named list", {
    res <- check_keep_empty(list(col1 = TRUE, col2 = FALSE, col3 = "level"))
    expect_equal(res$col1, "true")
    expect_equal(res$col2, "false")
    expect_equal(res$col3, "level")
})

test_that("check_keep_empty works with default column", {
    res <- check_keep_empty(list(col1 = TRUE, col2 = FALSE), x = "level")
    expect_equal(res$col1, "true")
    expect_equal(res$col2, "false")
    expect_equal(length(res), 2)

    res <- check_keep_empty(TRUE, x = "level")
    expect_equal(res, list(level = "true"))
})

test_that("process_keep_na_empty keep_na works", {
    data <- data.frame(
        col1 = factor(c("A", "B", NA, "A", "C", NA)),
        col2 = c(1, 2, NA, 4, 5, NA)
    )
    keep_na <- list(col1 = "<NA>", col2 = "missing")
    processed_data <- process_keep_na_empty(data, keep_na)
    expect_equal(as.character(processed_data$col1), c("A", "B", "<NA>", "A", "C", "<NA>"))
    expect_equal(processed_data$col2, c(1, 2, "missing", 4, 5, "missing"))

    data <- data.frame(
        col1 = factor(c("A", "B", NA, "A", "C", NA)),
        col2 = c(1, 2, NA, 4, 5, NA)
    )
    keep_na <- list(col1 = FALSE, col2 = FALSE)
    processed_data <- process_keep_na_empty(data, keep_na)
    expect_equal(as.character(processed_data$col1), c("A", "B", "A", "C"))
    expect_equal(processed_data$col2, c(1, 2, 4, 5))
})

test_that("process_keep_na_empty keep_na works with given column", {
    data <- data.frame(
        col1 = factor(c("A", "B", NA, "A", "C", NA)),
        col2 = c(1, 2, NA, 4, 5, NA)
    )
    keep_na <- list(col1 = "<NA>", col2 = "missing")
    processed_data <- process_keep_na_empty(data, keep_na, col = "col1")
    expect_equal(as.character(processed_data$col1), c("A", "B", "<NA>", "A", "C", "<NA>"))
    expect_equal(processed_data$col2, c(1, 2, NA, 4, 5, NA))
})

test_that("process_keep_na_empty keep_empty works", {
    data <- data.frame(
        col1 = factor(c("A", "B"), levels = c("A", "B", "C", "D", "E")),
        col2 = factor(c("X", "Y"), levels = c("X", "Y", "Z"))
    )
    keep_empty <- list(col1 = "level", col2 = "true")
    processed_data <- process_keep_na_empty(data, keep_empty = keep_empty)
    expect_equal(levels(processed_data$col1), c("A", "B", "C", "D", "E"))
    expect_equal(levels(processed_data$col2), c("X", "Y", "Z"))

    keep_empty <- list(col1 = "false", col2 = "false")
    processed_data <- process_keep_na_empty(data, keep_empty = keep_empty)
    expect_equal(levels(processed_data$col1), c("A", "B"))
    expect_equal(levels(processed_data$col2), c("X", "Y"))
})

test_that("process_keep_na_empty keep_empty works with given column", {
    data <- data.frame(
        col1 = factor(c("A", "B"), levels = c("A", "B", "C", "D", "E")),
        col2 = factor(c("X", "Y"), levels = c("X", "Y", "Z"))
    )
    keep_empty <- list(col1 = "level", col2 = "true")
    processed_data <- process_keep_na_empty(data, keep_empty = keep_empty, col = "col1")
    expect_equal(levels(processed_data$col1), c("A", "B", "C", "D", "E"))
    expect_equal(levels(processed_data$col2), c("X", "Y", "Z"))

    keep_empty <- list(col1 = "false", col2 = "false")
    processed_data <- process_keep_na_empty(data, keep_empty = keep_empty, col = "col2")
    expect_equal(levels(processed_data$col1), c("A", "B", "C", "D", "E"))
    expect_equal(levels(processed_data$col2), c("X", "Y"))
})
