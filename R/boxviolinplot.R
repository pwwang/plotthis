#' Atomic Box/Violin plot
#'
#' @inheritParams common_args
#' @param x A character string of the column name to plot on the x-axis.
#'  A character/factor column is expected. If multiple columns are provided, the columns will be concatenated with `x_sep`.
#' @param x_sep A character string to concatenate the columns in `x`, if multiple columns are provided.
#'  When `in_form` is "wide", `x` columns will not be concatenated.
#' @param y A character string of the column name to plot on the y-axis. A numeric column is expected.
#'  When `in_form` is "wide", `y` is not required. The values under `x` columns will be used as y-values.
#' @param base A character string to specify the base plot type. Either "box", "violin" or "none" (used by BeeswarmPlot).
#' @param in_form A character string to specify the input data type. Either "long" or "wide".
#' @param sort_x A character string to specify the sorting of x-axis, chosen from "none", "mean_asc", "mean_desc", "mean", "median_asc", "median_desc", "median".
#' * `none` means no sorting (as-is).
#' * `mean_asc` sorts the x-axis by ascending mean of y-values.
#' * `mean_desc` sorts the x-axis by descending mean of y-values.
#' * `mean` is an alias for `mean_asc`.
#' * `median_asc` sorts the x-axis by ascending median of y-values.
#' * `median_desc` sorts the x-axis by descending median of y-values.
#' * `median` is an alias for `median_asc`.
#' @param flip A logical value to flip the plot.
#' @param keep_empty A logical value to keep the empty levels in the x-axis.
#' @param group_by A character string of the column name to dodge the boxes/violins
#' @param group_by_sep A character string to concatenate the columns in `group_by`, if multiple columns are provided.
#' @param group_name A character string to name the legend of dodge.
#' @param paired_by A character string of the column name identifying paired observations for paired tests.
#' @param fill_mode A character string to specify the fill mode. Either "dodge", "x", "mean", "median".
#' @param fill_reverse A logical value to reverse the fill colors for gradient fill (mean/median).
#' @param add_point A logical value to add (jitter) points to the plot.
#' @param pt_color A character string to specify the color of the points.
#' @param pt_size A numeric value to specify the size of the points.
#' @param pt_alpha A numeric value to specify the transparency of the points.
#' @param jitter_width A numeric value to specify the width of the jitter.
#' Defaults to 0.5, but when paired_by is provided, it will be set to 0.
#' @param jitter_height A numeric value to specify the height of the jitter.
#' @param add_beeswarm A logical value to add beeswarm points to the plot instead of jittered points.
#'  When TRUE, points are positioned using the beeswarm algorithm to avoid overlap while showing density.
#'  Requires the ggbeeswarm package to be installed.
#' @param beeswarm_method A character string to specify the beeswarm method. Either "swarm", "compactswarm", "hex",
#'  "square", or "center". Default is "swarm". See ggbeeswarm::geom_beeswarm for details.
#' @param beeswarm_cex A numeric value to specify the scaling for adjusting point spacing in beeswarm.
#'  Default is 1. Larger values space out points more.
#' @param beeswarm_priority A character string to specify point layout priority. Either "ascending", "descending",
#'  "density", or "random". Default is "ascending".
#' @param beeswarm_dodge A numeric value to specify the dodge width for beeswarm points when group_by is provided.
#'  Default is 0.9
#' @param stack A logical value whether to stack the facetted plot by 'facet_by'.
#' @param y_max A numeric value or a character string to specify the maximum value of the y-axis.
#' You can also use quantile notation like "q95" to specify the 95th percentile.
#' When comparisons are set and a numeric y_max is provided, it will be used to set the y-axis limit, including
#' the significance labels.
#' @param y_min A numeric value or a character string to specify the minimum value of the y-axis.
#' You can also use quantile notation like "q5" to specify the 5th percentile.
#' @param y_trans A character string to specify the transformation of the y-axis.
#' @param y_nbreaks A numeric value to specify the number of breaks in the y-axis.
#' @param step_increase A numeric value to specify the step increase in fraction of total height for every
#' additional comparison of the significance labels.
#' @param symnum_args A list of arguments to pass to the function `symnum` for symbolic number coding of p-values.
#' For example, `symnum_args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf), symbols = c("****", "***", "**", "*", "ns"))`.
#' In other words, we use the following convention for symbols indicating statistical significance:
#' * `ns`: p > 0.05
#' * `*`: p <= 0.05
#' * `**`: p <= 0.01
#' * `***`: p <= 0.001
#' * `****`: p <= 0.0001
#' @param add_box A logical value to add box plot to the plot.
#' @param box_color A character string to specify the color of the box plot.
#' @param box_width A numeric value to specify the width of the box plot.
#' @param box_ptsize A numeric value to specify the size of the box plot points in the middle.
#' @param add_trend A logical value to add trend line to the plot.
#' @param trend_color A character string to specify the color of the trend line.
#' This won't work when `group_by` is specified, the trend line will be colored by the `group_by` variable.#'
#' @param trend_linewidth A numeric value to specify the width of the trend line.
#' @param trend_ptsize A numeric value to specify the size of the trend line points.
#' @param add_stat A character string to add statistical test to the plot.
#' @param stat_name A character string to specify the name of the stat legend.
#' @param stat_color A character string to specify the color of the statistical test.
#' @param stat_size A numeric value to specify the size of the statistical test.
#' @param stat_stroke A numeric value to specify the stroke of the statistical test.
#' @param stat_shape A numeric value to specify the shape of the statistical test.
#' @param add_bg A logical value to add background to the plot.
#' @param bg_palette A character string to specify the palette of the background.
#' @param bg_palcolor A character vector to specify the colors of the background.
#' @param bg_alpha A numeric value to specify the transparency of the background.
#' @param add_line A character string to add a line to the plot.
#' @param line_color A character string to specify the color of the line.
#' @param line_width A numeric value to specify the size of the line.
#' @param line_type A numeric value to specify the type of the line.
#' @param highlight A vector of character strings to highlight the points.
#'   It should be a subset of the row names of the data.
#'   If TRUE, it will highlight all points.
#' @param highlight_color A character string to specify the color of the highlighted points.
#' @param highlight_size A numeric value to specify the size of the highlighted points.
#' @param highlight_alpha A numeric value to specify the transparency of the highlighted points.
#' @param comparisons A logical value or a list of vectors to perform pairwise comparisons.
#' If `TRUE`, it will perform pairwise comparisons for all pairs.
#' @param ref_group A character string to specify the reference group for comparisons.
#' @param pairwise_method A character string to specify the pairwise comparison method.
#' @param multiplegroup_comparisons A logical value to perform multiple group comparisons.
#' @param multiple_method A character string to specify the multiple group comparison method.
#' @param sig_label A character string to specify the label of the significance test.
#' For multiple group comparisons (`multiplegroup_comparisons = TRUE`), it must be either "p.format" or "p.signif".
#' For pairwise comparisons, it can be:
#' * the column containing the label (e.g.: label = "p" or label = "p.adj"), where p is the p-value.
#'   Other possible values are "p.signif", "p.adj.signif", "p.format", "p.adj.format".
#' * an expression that can be formatted by the glue() package.
#'   For example, when specifying `label = "Wilcoxon, p = {p}"`, the expression `{p}` will be replaced by its value.
#' * a combination of plotmath expressions and glue expressions.
#'   You may want some of the statistical parameter in italic; for example: `label = "Wilcoxon, p= {p}"`
#'   See https://rpkgs.datanovia.com/ggpubr/reference/geom_pwc.html for more details.
#' @param sig_labelsize A numeric value to specify the size of the significance test label.
#' @param hide_ns A logical value to hide the non-significant comparisons.
#' @return A ggplot object
#' @keywords internal
#' @importFrom utils combn
#' @importFrom stats median quantile
#' @importFrom rlang sym syms parse_expr
#' @importFrom dplyr mutate ungroup first
#' @importFrom ggplot2 geom_boxplot geom_violin geom_jitter geom_point geom_line geom_hline geom_vline layer_data
#' @importFrom ggplot2 scale_fill_manual scale_color_manual scale_shape_manual scale_linetype_manual stat_summary
#' @importFrom ggplot2 labs theme element_line element_text position_dodge position_jitter coord_flip layer_scales
#' @importFrom ggplot2 position_jitterdodge scale_shape_identity scale_size_manual scale_alpha_manual scale_y_continuous
BoxViolinPlotAtomic <- function(
    data, x, x_sep = "_", y = NULL, base = c("box", "violin", "none"), in_form = c("long", "wide"),
    sort_x = c("none", "mean_asc", "mean_desc", "mean", "median_asc", "median_desc", "median"),
    flip = FALSE, keep_empty = FALSE, group_by = NULL, group_by_sep = "_", group_name = NULL,
    paired_by = NULL, x_text_angle = ifelse(isTRUE(flip) && isTRUE(stack), 90, 45), step_increase = 0.1,
    fill_mode = ifelse(!is.null(group_by), "dodge", "x"), fill_reverse = FALSE, symnum_args = NULL,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = NULL, legend.position = "right", legend.direction = "vertical",
    add_point = FALSE, pt_color = if (isTRUE(add_beeswarm)) NULL else "grey30", pt_size = NULL, pt_alpha = 1, y_nbreaks = 4,
    jitter_width = NULL, jitter_height = 0, stack = FALSE, y_max = NULL, y_min = NULL, y_trans = "identity",
    add_beeswarm = FALSE, beeswarm_method = "swarm", beeswarm_cex = 1, beeswarm_priority = "ascending",
    beeswarm_dodge = 0.9, add_box = FALSE, box_color = "black", box_width = 0.1, box_ptsize = 2.5,
    add_trend = FALSE, trend_color = NULL, trend_linewidth = 1, trend_ptsize = 2,
    add_stat = NULL, stat_name = NULL, stat_color = "black", stat_size = 1, stat_stroke = 1, stat_shape = 25,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    add_line = NULL, line_color = "red2", line_width = .6, line_type = 2,
    highlight = NULL, highlight_color = "red2", highlight_size = 1, highlight_alpha = 1,
    comparisons = NULL, ref_group = NULL, pairwise_method = "wilcox.test",
    multiplegroup_comparisons = FALSE, multiple_method = "kruskal.test",
    sig_label = "p.format", sig_labelsize = 3.5, hide_ns = FALSE,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525, ...) {
    set.seed(seed)
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    in_form <- match.arg(in_form)
    if (in_form == "wide") {
        data <- data %>% pivot_longer(cols = x, names_to = ".x", values_to = ".y")
        x <- ".x"
        y <- ".y"
    }
    x <- check_columns(data, x, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = x_sep)
    y <- check_columns(data, y)

    group_by <- check_columns(data, group_by,
        force_factor = TRUE,
        allow_multi = TRUE, concat_multi = TRUE, concat_sep = group_by_sep
    )
    facet_by <- check_columns(data, facet_by, force_factor = TRUE, allow_multi = TRUE)
    paired_by <- check_columns(data, paired_by, force_factor = TRUE)
    base_size <- theme_args$base_size %||% 12
    sig_labelsize <- sig_labelsize * base_size / 12

    # Validate beeswarm parameters
    if (isTRUE(add_beeswarm)) {
        if (!requireNamespace("ggbeeswarm", quietly = TRUE)) {
            stop("Package 'ggbeeswarm' is required for beeswarm plots. Please install it with: install.packages('ggbeeswarm')")
        }
        add_point <- TRUE
        if (!is.null(paired_by)) {
            warning("'add_beeswarm' is not fully compatible with 'paired_by'. Using jittered points instead for paired data.")
            add_beeswarm <- FALSE
        }
    }

    if (!is.null(paired_by)) {
        if (!isTRUE(add_point)) {
            warning("Forcing 'add_point' = TRUE when 'paired_by' is provided.")
            add_point <- TRUE
        }

        if (any(is.na(data[[paired_by]]))) {
            warning("'paired_by' contains missing values, removing corresponding rows.")
            data <- data[!is.na(data[[paired_by]]), , drop = FALSE]
        }
        n_total_col <- paste0(".n_total_", paired_by)
        sym_ntc <- sym(n_total_col)
        if (!is.null(group_by)) {
            # We should have exactly two groups for each x value
            # and for a pair, the two observations must belong to different groups
            # and the same paired_by value
            problem_groups <- data %>%
                dplyr::group_by(!!!syms(c(x, paired_by, group_by))) %>%
                dplyr::summarise(.n = dplyr::n(), .groups = "drop") %>%
                dplyr::add_count(!!!syms(c(x, paired_by)), name = n_total_col) %>%
                dplyr::filter(!!sym(".n") != 1 | !!sym_ntc != 2) %>%
                dplyr::mutate(
                    .n = ifelse(!!sym(".n") == 1, !!sym(".n"), paste0(!!sym(".n"), " (expecting 1)")),
                    !!sym_ntc := ifelse(!!sym_ntc == 2, !!sym_ntc, paste0(!!sym_ntc, " (expecting 2)"))
                )
            # If not, indicate which group (x, paired_by) has the problem
            if (nrow(problem_groups) > 0) {
                stop("When 'paired_by' and 'group_by' are both provided, each combination of 'x' and 'paired_by' must have exactly two observations, one for each group in 'group_by'. The following combinations do not satisfy this requirement:\n",
                    paste0(
                        apply(problem_groups[, c(x, paired_by, group_by, ".n", n_total_col)], 1, function(row) {
                            paste(paste(names(row), row, sep = "="), collapse = ", ")
                        }),
                        collapse = "\n"
                    )
                )
            }
        } else if (dplyr::n_distinct(data[[x]], na.rm = TRUE) != 2) {
            stop("Exactly two unique values of 'x' are required when 'paired_by' is provided without 'group_by'.")
        } else {
            problem_groups <- data %>%
                dplyr::group_by(!!!syms(c(x, paired_by))) %>%
                dplyr::summarise(.n = dplyr::n(), .groups = "drop") %>%
                dplyr::add_count(!!!syms(paired_by), name = n_total_col) %>%
                dplyr::filter(!!sym(".n") != 1 | !!sym_ntc != 2) %>%
                dplyr::mutate(
                    .n = ifelse(!!sym(".n") == 1, !!sym(".n"), paste0(!!sym(".n"), " (expecting 1)")),
                    !!sym_ntc := ifelse(!!sym_ntc == 2, !!sym_ntc, paste0(!!sym_ntc, " (expecting 2)"))
                )
            if (nrow(problem_groups) > 0) {
                stop("When 'paired_by' is provided without 'group_by', each combination of 'x' and 'paired_by' must have exactly two observations, one for each value of 'x'. The following combinations do not satisfy this requirement:\n",
                    paste0(
                        apply(problem_groups[, c(x, paired_by, ".n", n_total_col)], 1, function(row) {
                            paste(paste(names(row), row, sep = "="), collapse = ", ")
                        }),
                        collapse = "\n"
                    )
                )
            }
        }

        # For paired tests, ensure data is sorted by paired_by so that
        # corresponding observations across groups are in the same order
        data <- data %>% dplyr::arrange(!!!syms(unique(c(paired_by, x, group_by))))
    }
    if (isTRUE(comparisons) && is.null(group_by)) {
        # stop("'group_by' must be provided to when 'comparisons' is TRUE.")
        comparisons <- combn(levels(data[[x]]), 2, simplify = FALSE)
    }
    if (length(comparisons) > 0) {
        if (!is.list(comparisons) && !isTRUE(comparisons)) {
            comparisons <- list(comparisons)
        }
        ncomp <- sapply(comparisons, length)
        if (any(ncomp) > 2) {
            stop("'comparisons' must be a list in which all elements must be vectors of length 2")
        }
    }
    if (!isFALSE(multiplegroup_comparisons)) {
        stopifnot(
            "'sig_label' must be 'p.format' or 'p.signif' when 'multiplegroup_comparisons' is TRUE." =
            sig_label %in% c("p.format", "p.signif")
        )
    }


    sort_x <- match.arg(sort_x)
    if (sort_x != "none" && !is.null(facet_by)) {
        stop("Cannot sort x-axis when facet_by is provided.")
    }
    data <- data %>%
        dplyr::group_by(!!!syms(unique(c(x, group_by, facet_by)))) %>%
        mutate(.y_mean = mean(!!sym(y)), .y_median = median(!!sym(y))) %>%
        ungroup()

    values <- data[[y]][is.finite(data[[y]])]
    if (is.character(y_max)) {
        q_max <- as.numeric(sub("(^q)(\\d+)", "\\2", y_max)) / 100
        y_max_use <- quantile(values, q_max, na.rm = TRUE)
    } else {
        y_max_use <- max(values, na.rm = TRUE)
    }
    if (is.null(y_min)) {
        y_min_use <- min(values, na.rm = TRUE)
    } else if (is.character(y_min)) {
        q_min <- as.numeric(sub("(^q)(\\d+)", "\\2", y_min)) / 100
        y_min_use <- quantile(values, q_min, na.rm = TRUE)
    } else {
        y_min_use <- y_min
    }
    rm(values)

    if (!is.null(highlight)) {
        if (isTRUE(highlight)) {
            data$.highlight <- TRUE
        } else if (is.numeric(highlight)) {
            data$.highlight <- 1:nrow(data) %in% highlight
        } else if (is.character(highlight) && length(highlight) == 1) {
            data <- mutate(data, .highlight = !!parse_expr(highlight))
        } else if (is.null(rownames(data))) {
            stop("No row names in the data, please provide a vector of indexes to highlight.")
        } else {
            data$.highlight <- rownames(data) %in% highlight
        }
        if (isFALSE(add_point)) {
            warning("Forcing add_point = TRUE when highlight is provided.")
            add_point <- TRUE
        }
    } else {
        data$.highlight <- FALSE
    }
    data$.highlight <- factor(as.character(data$.highlight), levels = c("TRUE", "FALSE"))

    if (sort_x == "mean" || sort_x == "mean_asc") {
        data[[x]] <- stats::reorder(data[[x]], data$.y_mean)
    } else if (sort_x == "mean_desc") {
        data[[x]] <- stats::reorder(data[[x]], -data$.y_mean)
    } else if (sort_x == "median" || sort_x == "median_asc") {
        data[[x]] <- stats::reorder(data[[x]], data$.y_median)
    } else if (sort_x == "median_desc") {
        data[[x]] <- stats::reorder(data[[x]], -data$.y_median)
    }

    if (isTRUE(flip)) {
        data[[x]] <- factor(data[[x]], levels = rev(levels(data[[x]])))
        aspect.ratio <- 1 / aspect.ratio
        if (length(aspect.ratio) == 0 || is.na(aspect.ratio)) {
            aspect.ratio <- NULL
        }
    }

    base <- match.arg(base)
    if (isTRUE(add_box) && base == "box") {
        stop("Cannot add box plot to box plot.")
    }
    fill_mode <- match.arg(fill_mode, c("dodge", "x", "mean", "median"))
    if (fill_mode == "dodge") {
        fill_by <- group_by
    } else if (fill_mode == "x") {
        fill_by <- x
    } else if (fill_mode == "mean") {
        fill_by <- ".y_mean"
    } else {
        fill_by <- ".y_median"
    }
    p <- ggplot(data, aes(x = !!sym(x), y = !!sym(y), fill = !!sym(fill_by)))
    if (isTRUE(add_bg)) {
        p <- p + bg_layer(data, x, bg_palette, bg_palcolor, bg_alpha, keep_empty, facet_by)
    }

    if (base == "box" || (base == "none" && isTRUE(add_box))) {
        p <- p + geom_boxplot(
            position = position_dodge(width = 0.9), color = "black", width = 0.8, outlier.shape = NA
        )
    } else if (base == "violin") {
        p <- p + geom_violin(
            # There is a bug in ggplot2 with preserve = "single" for violin plots
            # See https://github.com/tidyverse/ggplot2/issues/2801
            # There is a fix but not yet released
            position = position_dodge(width = 0.9), scale = "width", trim = TRUE,
            alpha = alpha, width = 0.8
        )
    }
    if (fill_mode == "dodge") {
        p <- p + scale_fill_manual(
            name = group_name %||% group_by,
            values = palette_this(levels(data[[group_by]]), palette = palette, palcolor = palcolor)
        )
    } else if (fill_mode == "x") {
        p <- p + scale_fill_manual(
            name = x,
            values = palette_this(levels(data[[x]]), palette = palette, palcolor = palcolor)
        )
    } else {
        p <- p + scale_fill_gradientn(
            name = paste0(y, " (", fill_mode, ")"),
            n.breaks = 3,
            colors = palette_this(palette = palette, palcolor = palcolor, reverse = fill_reverse),
            na.value = "grey80",
            guide = guide_colorbar(frame.colour = "black", ticks.colour = "black", title.hjust = 0)
        )
    }

    # when base is none, boxes are added as base
    if (isTRUE(add_box) && base != "none") {
        p <- p +
            new_scale_fill() +
            geom_boxplot(
                position = position_dodge(width = 0.9), fill = box_color, color = box_color,
                width = box_width, show.legend = FALSE, outlier.shape = NA
            ) +
            stat_summary(
                fun = first, geom = "point", mapping = aes(y = !!sym(".y_median")),
                position = position_dodge(width = 0.9), color = "black", fill = "white",
                size = box_ptsize, shape = 21
            )
    }

    if (length(comparisons) > 0) {
        if (isTRUE(comparisons)) {
            # group_use <- names(which(rowSums(table(data[[x]], data[[group_by]]) >= 2) >= 2))
            # print(group_use)
            if (any(rowSums(table(data[[x]], data[[group_by]]) >= 2) >= 3)) {
                message("Detected more than 2 groups. Use multiple_method for comparison")
                # method <- multiple_method
                multiplegroup_comparisons <- TRUE
            } else {
                method <- pairwise_method

                if (!identical(fill_mode, "dodge")) {
                    stop("`comparisons` can only be used with `fill_mode = 'dodge'`.")
                }

                # Preprocess data to avoid test failures
                # Check each x/facet combination for problematic data
                split_cols <- c(x, y, group_by)
                grouping_vars <- x
                if (!is.null(facet_by)) {
                    split_cols <- c(split_cols, facet_by)
                    grouping_vars <- c(grouping_vars, facet_by)
                }

                # Create grouping key for x and facet combinations
                if (length(grouping_vars) > 1) {
                    split_key <- interaction(data[grouping_vars], drop = TRUE, sep = " // ")
                } else {
                    split_key <- data[[grouping_vars]]
                }

                data_groups <- split(data[, split_cols, drop = FALSE], split_key)
                needs_fix <- FALSE

                # Check if any group will cause test failures
                for (group_data in data_groups) {
                    gs <- unique(as.character(group_data[[group_by]]))
                    if (length(gs) >= 2) {
                        yval1 <- group_data[[y]][group_data[[group_by]] == gs[1]]
                        yval2 <- group_data[[y]][group_data[[group_by]] == gs[2]]
                        # Check for zero variance or all NA
                        if (all(is.na(yval1)) || all(is.na(yval2)) ||
                            (length(unique(yval1[!is.na(yval1)])) <= 1 &&
                             length(unique(yval2[!is.na(yval2)])) <= 1)) {
                            needs_fix <- TRUE
                            break
                        }
                    }
                }

                pwc_data <- data
                if (needs_fix) {
                    warning("Some pairwise comparisons may fail due to insufficient variability. Adjusting data to ensure valid comparisons.")

                    # Split by facet if present
                    if (!is.null(facet_by)) {
                        facet_key <- interaction(data[facet_by], drop = TRUE, sep = " // ")
                        facet_splits <- split(data[, split_cols, drop = FALSE], facet_key)
                    } else {
                        facet_splits <- list(data[, split_cols, drop = FALSE])
                    }

                    fixed_data_list <- lapply(facet_splits, function(facet_data) {
                        xdata <- split(facet_data, facet_data[[x]])
                        all_gs <- unique(as.character(facet_data[[group_by]]))[1:2]

                        for (xval in names(xdata)) {
                            df <- xdata[[xval]]
                            gs <- unique(as.character(df[[group_by]]))

                            if (length(gs) < 2) {
                                # Create minimal data for both groups
                                df <- data.frame(x = xval, y = c(0, 1), group_by = all_gs)
                                colnames(df) <- c(x, y, group_by)
                                if (!is.null(facet_by)) {
                                    df[facet_by] <- unique(facet_data[facet_by])
                                }
                            } else {
                                yval1 <- df[[y]][df[[group_by]] == gs[1]]
                                yval2 <- df[[y]][df[[group_by]] == gs[2]]

                                # Handle all NA cases
                                if (all(is.na(yval1))) {
                                    yval1 <- c(0, rep(NA, length(yval1) - 1))
                                }
                                if (all(is.na(yval2))) {
                                    yval2 <- c(1, rep(NA, length(yval2) - 1))
                                }

                                # Handle zero variance cases
                                unique_y1 <- unique(yval1[!is.na(yval1)])
                                unique_y2 <- unique(yval2[!is.na(yval2)])

                                if (length(unique_y1) == 1 && length(unique_y2) == 1) {
                                    # Both groups have same single value - add minimal relative variance
                                    # Calculate a small epsilon relative to the data scale
                                    all_y <- c(yval1, yval2)
                                    all_y_finite <- all_y[is.finite(all_y)]

                                    if (length(all_y_finite) > 0) {
                                        y_abs <- abs(all_y_finite)
                                        if (max(y_abs) > 0) {
                                            epsilon <- max(y_abs) * 1e-10
                                        } else {
                                            epsilon <- 1e-10
                                        }
                                    } else {
                                        epsilon <- 1e-10
                                    }

                                    # Add variance within each group while maintaining the same mean
                                    # This ensures the test will return p ≈ 1 (no significant difference)
                                    non_na_idx_1 <- which(!is.na(yval1))
                                    non_na_idx_2 <- which(!is.na(yval2))

                                    if (length(non_na_idx_1) >= 2) {
                                        yval1[non_na_idx_1[1]] <- unique_y1[1] - epsilon
                                        yval1[non_na_idx_1[2]] <- unique_y1[1] + epsilon
                                    } else if (length(non_na_idx_1) == 1) {
                                        yval1[non_na_idx_1[1]] <- unique_y1[1]
                                    }

                                    if (length(non_na_idx_2) >= 2) {
                                        yval2[non_na_idx_2[1]] <- unique_y2[1] - epsilon
                                        yval2[non_na_idx_2[2]] <- unique_y2[1] + epsilon
                                    } else if (length(non_na_idx_2) == 1) {
                                        yval2[non_na_idx_2[1]] <- unique_y2[1]
                                    }
                                }

                                df[[y]][df[[group_by]] == gs[1]] <- yval1
                                df[[y]][df[[group_by]] == gs[2]] <- yval2
                            }
                            xdata[[xval]] <- df
                        }
                        do.call(rbind, xdata)
                    })
                    pwc_data <- do.call(rbind, fixed_data_list)
                }

                # Now call geom_pwc once with the preprocessed data
                # Add paired test support when paired_by is provided
                pwc_call <- list(
                    data = pwc_data,
                    label = sig_label,
                    label.size = sig_labelsize,
                    y.position = y_max_use,
                    step.increase = step_increase,
                    symnum.args = symnum_args,
                    tip.length = 0.03,
                    vjust = 0,
                    ref.group = ref_group,
                    method = method,
                    hide.ns = hide_ns
                )

                # Add paired test parameters if paired_by is provided
                if (!is.null(paired_by)) {
                    pwc_call$method.args <- c(pwc_call$method.args, list(paired = TRUE))
                }

                p <- p + do.call(ggpubr::geom_pwc, pwc_call)

                y_max_use <- layer_scales(p)$y$range$range[2]
            }
        } else if (!isTRUE(multiplegroup_comparisons)) {
            # if (!is.null(group_by)) {
            #     stop("`comparisons` can only be used when `group_by` is NULL is TRUE.")
            # }
            # Convert comparisons to indices
            comparisons <- lapply(
                comparisons,
                function(el) {
                    if (!is.numeric(el)) {
                        which(levels(data[[x]]) %in% el)
                    } else {
                        el
                    }
                }
            )

            # Preprocess data to avoid test failures (same as above for group_by case)
            split_cols <- if (!is.null(group_by)) c(x, y, group_by) else c(x, y)
            grouping_vars <- x
            if (!is.null(facet_by)) {
                split_cols <- c(split_cols, facet_by)
                grouping_vars <- c(grouping_vars, facet_by)
            }

            # Create grouping key for x and facet combinations
            if (length(grouping_vars) > 1) {
                split_key <- interaction(data[grouping_vars], drop = TRUE, sep = " // ")
            } else {
                split_key <- data[[grouping_vars]]
            }

            data_groups <- split(data[, split_cols, drop = FALSE], split_key)
            needs_fix <- FALSE

            # For exact comparisons, we need to check x groups, not group_by groups
            # Check if any x group has zero variance
            for (group_data in data_groups) {
                yval <- group_data[[y]]
                # Check for zero variance or all NA
                if (all(is.na(yval)) || length(unique(yval[!is.na(yval)])) <= 1) {
                    needs_fix <- TRUE
                    break
                }
            }

            pwc_data <- data
            if (needs_fix) {
                warning("Some pairwise comparisons may fail due to insufficient variability. Adjusting data to ensure valid comparisons.")

                # Split by facet if present
                if (!is.null(facet_by)) {
                    facet_key <- interaction(data[facet_by], drop = TRUE, sep = " // ")
                    facet_splits <- split(data[, split_cols, drop = FALSE], facet_key)
                } else {
                    facet_splits <- list(data[, split_cols, drop = FALSE])
                }

                fixed_data_list <- lapply(facet_splits, function(facet_data) {
                    xdata <- split(facet_data, facet_data[[x]])

                    for (xval in names(xdata)) {
                        df <- xdata[[xval]]
                        if (nrow(df) < 2) {
                            xdata[[xval]] <- df
                            next
                        }
                        yval <- df[[y]]

                        # Handle all NA cases
                        if (all(is.na(yval))) {
                            yval <- c(0, 1, rep(NA, length(yval) - 2))
                        }

                        # Handle zero variance cases
                        unique_y <- unique(yval[!is.na(yval)])

                        if (length(unique_y) == 1) {
                            # Single value - add minimal relative variance
                            # Calculate a small epsilon relative to the data scale
                            all_y_finite <- yval[is.finite(yval)]

                            if (length(all_y_finite) > 0) {
                                y_abs <- abs(all_y_finite)
                                if (max(y_abs) > 0) {
                                    epsilon <- max(y_abs) * 1e-10
                                } else {
                                    epsilon <- 1e-10
                                }
                            } else {
                                epsilon <- 1e-10
                            }

                            # Add symmetric variance around the mean
                            non_na_idx <- which(!is.na(yval))
                            if (length(non_na_idx) >= 2) {
                                yval[non_na_idx[1]] <- unique_y[1] - epsilon
                                yval[non_na_idx[2]] <- unique_y[1] + epsilon
                            }
                        }

                        df[[y]] <- yval
                        xdata[[xval]] <- df
                    }
                    do.call(rbind, xdata)
                })
                pwc_data <- do.call(rbind, fixed_data_list)
            }

            # Add paired test support when paired_by is provided
            method_args <- list(comparisons = comparisons)
            if (!is.null(paired_by)) {
                method_args$paired <- TRUE
            }

            p <- p + ggpubr::geom_pwc(
                data = pwc_data,
                label = sig_label,
                label.size = sig_labelsize,
                y.position = y_max_use,
                step.increase = step_increase,
                symnum.args = symnum_args,
                tip.length = 0.03,
                vjust = 0,
                # comparisons = comparisons,
                ref.group = ref_group,
                method = pairwise_method,
                method.args = method_args,
                hide.ns = hide_ns
            )
            y_max_use <- layer_scales(p)$y$range$range[1] + (layer_scales(p)$y$range$range[2] - layer_scales(p)$y$range$range[1]) * 1.15
        }
    }

    if (isTRUE(multiplegroup_comparisons)) {
        p <- p + ggpubr::stat_compare_means(
            mapping = if (!is.null(group_by)) {
                aes(x = !!sym(x), y = !!sym(y), group = !!sym(group_by))
            } else {
                aes(x = !!sym(x), y = !!sym(y))
            },
            inherit.aes = FALSE,
            method = multiple_method,
            symnum.args = symnum_args,
            label.y = y_max_use,
            size = sig_labelsize,
            label = sig_label,
            vjust = -0.5,
            hjust = ifelse(is.null(group_by), 0, 0.5)
        )
        y_max_use <- layer_scales(p)$y$range$range[1] + (layer_scales(p)$y$range$range[2] - layer_scales(p)$y$range$range[1]) * 1.15
    }
    if (!is.null(y_max) && is.numeric(y_max)) {
        y_max_use <- max(y_max_use, y_max)
    }

    if (isTRUE(add_point)) {
        if (!is.null(paired_by)) {
            if (is.null(group_by)) {
                p <- p + geom_line(
                    data = data,
                    mapping = aes(x = !!sym(x), y = !!sym(y), group = !!sym(paired_by)),
                    color = pt_color,
                    alpha = pt_alpha,
                    linewidth = 0.3,
                    inherit.aes = FALSE
                )
            } else {
                line_data <- data
                # re-calculate x
                # for the first group, x = integer(x) - n
                # for the second group, x = integer(x) + n
                line_data$.xint <- as.numeric(line_data[[x]])
                groups <- levels(line_data[[group_by]])
                line_data$.x <- ifelse(
                    line_data[[group_by]] == groups[1],
                    line_data$.xint - .225,  # n = 0.225 = 0.9 / 2 / 2
                    line_data$.xint + .225
                )
                line_data$.line_group <- paste(line_data[[paired_by]], line_data[[x]], sep = " // ")
                p <- p + geom_line(
                    data = line_data,
                    mapping = aes(x = !!sym(".x"), y = !!sym(y), group = !!sym(".line_group")),
                    color = pt_color,
                    alpha = pt_alpha,
                    linewidth = 0.3,
                    inherit.aes = FALSE
                )
            }
        }

        # Use beeswarm or jittered points
        if (isTRUE(add_beeswarm)) {
            # Use ggbeeswarm for non-overlapping point layout
            if (!is.null(pt_color)) {
                p <- p +
                    ggbeeswarm::geom_beeswarm(
                        aes(size = !!sym(".highlight"), alpha = !!sym(".highlight")),
                        color = pt_color,
                        method = beeswarm_method,
                        cex = beeswarm_cex,
                        priority = beeswarm_priority,
                        dodge.width = beeswarm_dodge,
                        show.legend = FALSE
                    )

            } else {
                p <- p +
                    ggbeeswarm::geom_beeswarm(
                        aes(color = !!sym(fill_by), size = !!sym(".highlight"), alpha = !!sym(".highlight")),
                        method = beeswarm_method,
                        cex = beeswarm_cex,
                        priority = beeswarm_priority,
                        dodge.width = beeswarm_dodge
                    ) +
                    scale_color_manual(
                        values = palette_this(levels(data[[fill_by]]), palette = palette, palcolor = palcolor),
                        guide = "legend"
                    )
            }
            p <- p +
                scale_size_manual(
                    values = c("TRUE" = highlight_size, "FALSE" = pt_size %||% min(3000 / nrow(data), 0.6)),
                    guide = "none"
                ) +
                scale_alpha_manual(
                    values = c("TRUE" = highlight_alpha, "FALSE" = pt_alpha),
                    guide = "none"
                )
        } else {
            # Use regular jittered points
            p <- p +
                geom_point(
                    aes(fill = !!sym(fill_by), color = !!sym(".highlight"), size = !!sym(".highlight"), alpha = !!sym(".highlight")),
                    position = position_jitterdodge(
                        jitter.width = jitter_width %||% ifelse(!is.null(paired_by), 0, 0.5),
                        jitter.height = jitter_height, dodge.width = 0.9, seed = seed
                    ),
                    show.legend = FALSE
                ) +
                scale_color_manual(values = c("TRUE" = highlight_color, "FALSE" = pt_color)) +
                scale_size_manual(values = c("TRUE" = highlight_size, "FALSE" = pt_size %||% min(3000 / nrow(data), 0.6))) +
                scale_alpha_manual(values = c("TRUE" = highlight_alpha, "FALSE" = pt_alpha))
        }
    }

    if (isTRUE(add_trend)) {
        if (is.null(trend_color)) {
            p <- p + stat_summary(
                fun = first, geom = "line", mapping = if (!is.null(group_by)) {
                    aes(y = !!sym(".y_median"), group = !!sym(group_by), color = !!sym(group_by))
                } else {
                    aes(y = !!sym(".y_median"), group = 1)
                },
                position = position_dodge(width = 0.9), linewidth = trend_linewidth
            )
            if (!is.null(group_by)) {
                p <- p + scale_color_manual(
                    values = palette_this(levels(data[[group_by]]), palette = palette, palcolor = palcolor),
                    guide = "none"
                )
            }
        } else {
            p <- p + stat_summary(
                fun = first, geom = "line", mapping = if (!is.null(group_by)) {
                    aes(y = !!sym(".y_median"), group = !!sym(group_by))
                } else {
                    aes(y = !!sym(".y_median"), group = 1)
                },
                position = position_dodge(width = 0.9), color = trend_color, linewidth = trend_linewidth
            )
        }

        p <- p + stat_summary(
            fun = first, geom = "point", mapping = if (!is.null(group_by)) {
                aes(y = !!sym(".y_median"), group = !!sym(group_by))
            } else {
                aes(y = !!sym(".y_median"), group = 1)
            },
            position = position_dodge(width = 0.9), color = "black", fill = "white",
            size = trend_ptsize, shape = 21
        )
    }

    if (!is.null(add_line)) {
        p <- p + geom_hline(
            yintercept = add_line,
            color = line_color, linetype = line_type, linewidth = line_width
        )
    }

    if (!is.null(add_stat)) {
        p <- p + stat_summary(
            fun = add_stat, geom = "point", mapping = if (!is.null(group_by)) {
                aes(shape = !!sym("stat_shape"), group = !!sym(group_by))
            } else {
                aes(shape = !!sym("stat_shape"), group = 1)
            },
            position = position_dodge(width = 0.9), color = stat_color, fill = stat_color, size = stat_size, stroke = stat_stroke,
        ) + scale_shape_identity(
            labels = stat_name %||% paste0(y, " (", deparse(substitute(add_stat)), ")"),
            guide = guide_legend(title = "", order = 2)
        )
    }

    just <- calc_just(x_text_angle)
    p <- p +
        scale_x_discrete(drop = !keep_empty) +
        labs(title = title, subtitle = subtitle, x = xlab %||% x, y = ylab %||% y)

    p <- p + scale_y_continuous(trans = y_trans, n.breaks = y_nbreaks)

    height <- width <- 0
    if (!identical(legend.position, "none")) {
        if (legend.position %in% c("right", "left")) {
            width <- width + 1
        } else if (legend.direction == "horizontal") {
            height <- height + 1
        } else {
            height <- height + 2
        }
    }
    x_maxchars <- max(nchar(levels(data[[x]])))
    nx <- nlevels(data[[x]])
    nd <- ifelse(is.null(group_by), 1, nlevels(data[[group_by]]))
    facet_free <- !is.null(facet_by) && (
        identical(facet_scales, "free") ||
            (!flip && identical(facet_scales, "free_y")) ||
            (flip && identical(facet_scales, "free_x"))
    )
    if (isTRUE(flip) && isTRUE(stack)) {
        facet_nrow <- facet_nrow %||% 1
        strip_position <- "top"
        p <- p + ggplot2::theme(
            # strip.text.x = element_text(angle = 90),
            panel.grid.major.x = element_line(color = "grey", linetype = 2),
            panel.spacing.x = unit(-1, "pt")
        )
        if (facet_free) {
            p <- p + coord_flip()
        } else {
            p <- p + coord_flip(ylim = c(y_min_use, y_max_use))
        }
        width <- max(3, width + 2 + x_maxchars * 0.05)
        height <- height + nx * nd * 0.3
    } else if (isTRUE(flip) && isFALSE(stack)) {
        strip_position <- "top"
        p <- p + ggplot2::theme(
            strip.text.y = element_text(angle = 0),
            panel.grid.major.x = element_line(color = "grey", linetype = 2),
        )
        if (facet_free) {
            p <- p + coord_flip()
        } else {
            p <- p + coord_flip(ylim = c(y_min_use, y_max_use))
        }
        width <- max(3, width + 2.2 + x_maxchars * 0.05)
        height <- height + nx * nd * 0.3
    } else if (isTRUE(stack)) {
        facet_ncol <- facet_ncol %||% 1
        strip_position <- "right"
        p <- p + ggplot2::theme(
            panel.spacing.y = unit(-1, "pt"),
            strip.text.y = element_text(angle = 0, hjust = 0),
            panel.grid.major.y = element_line(color = "grey", linetype = 2),
        )
        if (!facet_free) {
            p <- p + coord_cartesian(ylim = c(y_min_use, y_max_use))
        }
        height <- height + 4 + x_maxchars * 0.05
        width <- width + nx * nd * 0.3
    } else {
        strip_position <- "top"
        p <- p + ggplot2::theme(
            strip.text.x = element_text(angle = 0),
            panel.grid.major.x = element_line(color = "grey", linetype = 2),
        )
        if (!facet_free) {
            p <- p + coord_cartesian(ylim = c(y_min_use, y_max_use))
        }
        height <- max(3, height + 2 + x_maxchars * 0.05)
        width <- width + nx * nd * 0.3
    }

    p <- p +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            axis.text.x = element_text(angle = x_text_angle, hjust = just$h, vjust = just$v),
            legend.position = legend.position,
            legend.direction = legend.direction,
        )

    attr(p, "height") <- height
    attr(p, "width") <- max(width, height)

    facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow,
        strip.position = strip_position, legend.position = legend.position,
        legend.direction = legend.direction
    )
}

#' Box/Violin plot
#'
#' @rdname BoxViolinPlot-internal
#' @inheritParams common_args
#' @inheritParams BoxViolinPlotAtomic
#' @return A combined ggplot object or wrap_plots object or a list of ggplot objects
#' @keywords internal
#' @importFrom rlang %||%
BoxViolinPlot <- function(
    data, x, x_sep = "_", y = NULL, base = c("box", "violin"), in_form = c("long", "wide"),
    split_by = NULL, split_by_sep = "_", symnum_args = NULL,
    sort_x = c("none", "mean_asc", "mean_desc", "mean", "median_asc", "median_desc", "median"),
    flip = FALSE, keep_empty = FALSE, group_by = NULL, group_by_sep = "_", group_name = NULL,
    paired_by = NULL, x_text_angle = ifelse(isTRUE(flip) && isTRUE(stack), 90, 45), step_increase = 0.1,
    fill_mode = ifelse(!is.null(group_by), "dodge", "x"), fill_reverse = FALSE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = NULL, legend.position = "right", legend.direction = "vertical",
    add_point = FALSE, pt_color = if(isTRUE(add_beeswarm)) NULL else "grey30", pt_size = NULL, pt_alpha = 1,
    jitter_width = NULL, jitter_height = 0, stack = FALSE, y_max = NULL, y_min = NULL,
    add_beeswarm = FALSE, beeswarm_method = "swarm", beeswarm_cex = 1, beeswarm_priority = "ascending",
    beeswarm_dodge = 0.9, add_box = FALSE, box_color = "black", box_width = 0.1, box_ptsize = 2.5,
    add_trend = FALSE, trend_color = NULL, trend_linewidth = 1, trend_ptsize = 2,
    add_stat = NULL, stat_name = NULL, stat_color = "black", stat_size = 1, stat_stroke = 1, stat_shape = 25,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    add_line = NULL, line_color = "red2", line_width = .6, line_type = 2,
    highlight = NULL, highlight_color = "red2", highlight_size = 1, highlight_alpha = 1,
    comparisons = NULL, ref_group = NULL, pairwise_method = "wilcox.test",
    multiplegroup_comparisons = FALSE, multiple_method = "kruskal.test",
    sig_label = "p.format", sig_labelsize = 3.5, hide_ns = FALSE,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    axes = NULL, axis_titles = axes, guides = NULL, design = NULL, ...) {
    validate_common_args(seed)
    theme <- process_theme(theme)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = split_by_sep)

    if (!is.null(split_by)) {
        data[[split_by]] <- droplevels(data[[split_by]])
        datas <- split(data, data[[split_by]])
        # keep the order of levels
        datas <- datas[levels(data[[split_by]])]
    } else {
        datas <- list(data)
        names(datas) <- "..."
    }

    palette <- check_palette(palette, names(datas))
    palcolor <- check_palcolor(palcolor, names(datas))
    legend.direction <- check_legend(legend.direction, names(datas), "legend.direction")
    legend.position <- check_legend(legend.position, names(datas), "legend.position")

    stat_name <- stat_name %||% paste0(y, " (", deparse(substitute(add_stat)), ")")

    plots <- lapply(
        names(datas), function(nm) {
            default_title <- if (length(datas) == 1 && identical(nm, "...")) NULL else nm
            if (is.function(title)) {
                title <- title(default_title)
            } else {
                title <- title %||% default_title
            }
            BoxViolinPlotAtomic(datas[[nm]],
                x = x, x_sep = x_sep, y = y, base = base, in_form = in_form,
                sort_x = sort_x, flip = flip, keep_empty = keep_empty, group_by = group_by, group_by_sep = group_by_sep, group_name = group_name,
                paired_by = paired_by, x_text_angle = x_text_angle, fill_mode = fill_mode, fill_reverse = fill_reverse, step_increase = step_increase,
                theme = theme, theme_args = theme_args, palette = palette[[nm]], palcolor = palcolor[[nm]], alpha = alpha,
                aspect.ratio = aspect.ratio, legend.position = legend.position[[nm]], legend.direction = legend.direction[[nm]],
                add_point = add_point, pt_color = pt_color, pt_size = pt_size, pt_alpha = pt_alpha, symnum_args = symnum_args,
                jitter_width = jitter_width, jitter_height = jitter_height, stack = stack, y_max = y_max, y_min = y_min,
                add_beeswarm = add_beeswarm, beeswarm_method = beeswarm_method, beeswarm_cex = beeswarm_cex, beeswarm_priority = beeswarm_priority,
                beeswarm_dodge = beeswarm_dodge, add_box = add_box, box_color = box_color, box_width = box_width, box_ptsize = box_ptsize,
                add_trend = add_trend, trend_color = trend_color, trend_linewidth = trend_linewidth, trend_ptsize = trend_ptsize,
                add_stat = add_stat, stat_name = stat_name, stat_color = stat_color, stat_size = stat_size, stat_stroke = stat_stroke, stat_shape = stat_shape,
                add_bg = add_bg, bg_palette = bg_palette, bg_palcolor = bg_palcolor, bg_alpha = bg_alpha,
                add_line = add_line, line_color = line_color, line_width = line_width, line_type = line_type,
                highlight = highlight, highlight_color = highlight_color, highlight_size = highlight_size, highlight_alpha = highlight_alpha,
                comparisons = comparisons, ref_group = ref_group, pairwise_method = pairwise_method,
                multiplegroup_comparisons = multiplegroup_comparisons, multiple_method = multiple_method,
                sig_label = sig_label, sig_labelsize = sig_labelsize, hide_ns = hide_ns,
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, seed = seed, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, design = design)
}

#' Box / Violin Plot
#'
#' @description
#'  Box plot or violin plot with optional jitter points, trend line, statistical test, background, line, and highlight.
#' @rdname boxviolinplot
#' @return The Box / Violin plot(s).
#'  When `split_by` is not provided, it returns a ggplot object.
#'  When `split_by` is provided, it returns a object of plots wrapped by `patchwork::wrap_plots` if `combine = TRUE`;
#'  otherwise, it returns a list of ggplot objects.
#' @export
#' @inheritParams BoxViolinPlot
#' @examples
#' \donttest{
#' set.seed(8525)
#' data <- data.frame(
#'     x = rep(LETTERS[1:8], each = 40),
#'     y = c(rnorm(160), rnorm(160, mean = 1)),
#'     group1 = sample(c("g1", "g2"), 320, replace = TRUE),
#'     group2 = sample(c("h1", "h2", "h3", "h4"), 320, replace = TRUE)
#' )
#'
#' BoxPlot(data, x = "x", y = "y")
#' BoxPlot(data, x = "x", y = "y", add_beeswarm = TRUE, pt_color = "grey30")
#' BoxPlot(data,
#'     x = "x", y = "y",
#'     stack = TRUE, flip = TRUE, facet_by = "group1",
#'     add_bg = TRUE, bg_palette = "Paired"
#' )
#' BoxPlot(data,
#'     x = "x", y = "y",
#'     stack = TRUE, flip = TRUE, split_by = "group1",
#'     add_bg = TRUE, bg_palette = "Paired",
#'     palcolor = list(g1 = c("red", "blue"), g2 = c("blue", "red"))
#' )
#'
#' # wide form data
#' data_wide <- data.frame(
#'     A = rnorm(100),
#'     B = rnorm(100),
#'     C = rnorm(100)
#' )
#' BoxPlot(data_wide, x = c("A", "B", "C"), in_form = "wide")
#'
#' paired_data <- data.frame(
#'     subject = rep(paste0("s", 1:10), each = 2),
#'     visit = rep(c("pre", "post"), times = 10),
#'     value = rnorm(20)
#' )
#' # paired plot with connected lines and paired test
#' BoxPlot(
#'     paired_data,
#'     x = "visit", y = "value", comparisons = TRUE,
#'     paired_by = "subject", add_point = TRUE
#' )
#' paired_group_data <- data.frame(
#'     subject = rep(paste0("s", 1:6), each = 2),
#'     x = rep(c("A", "B"), each = 6),
#'     group = rep(c("before", "after"), times = 6),
#'     value = rnorm(12)
#' )
#' BoxPlot(
#'     paired_group_data,
#'     x = "x", y = "value",
#'     paired_by = "subject", group_by = "group",
#'     comparisons = TRUE, pt_size = 3, pt_color = "red"
#' )
#' }
BoxPlot <- function(
    data, x, x_sep = "_", y = NULL, in_form = c("long", "wide"),
    split_by = NULL, split_by_sep = "_", symnum_args = NULL,
    sort_x = c("none", "mean_asc", "mean_desc", "mean", "median_asc", "median_desc", "median"),
    flip = FALSE, keep_empty = FALSE, group_by = NULL, group_by_sep = "_", group_name = NULL,
    paired_by = NULL, x_text_angle = ifelse(isTRUE(flip) && isTRUE(stack), 90, 45), step_increase = 0.1,
    fill_mode = ifelse(!is.null(group_by), "dodge", "x"), fill_reverse = FALSE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = NULL, legend.position = "right", legend.direction = "vertical",
    add_point = FALSE, pt_color = if(isTRUE(add_beeswarm)) NULL else "grey30", pt_size = NULL, pt_alpha = 1,
    jitter_width = NULL, jitter_height = 0, stack = FALSE, y_max = NULL, y_min = NULL,
    add_beeswarm = FALSE, beeswarm_method = "swarm", beeswarm_cex = 1, beeswarm_priority = "ascending",
    beeswarm_dodge = 0.9, add_trend = FALSE, trend_color = NULL, trend_linewidth = 1, trend_ptsize = 2,
    add_stat = NULL, stat_name = NULL, stat_color = "black", stat_size = 1, stat_stroke = 1, stat_shape = 25,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    add_line = NULL, line_color = "red2", line_width = .6, line_type = 2,
    highlight = NULL, highlight_color = "red2", highlight_size = 1, highlight_alpha = 1,
    comparisons = NULL, ref_group = NULL, pairwise_method = "wilcox.test",
    multiplegroup_comparisons = FALSE, multiple_method = "kruskal.test",
    sig_label = "p.format", sig_labelsize = 3.5, hide_ns = FALSE,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    axes = NULL, axis_titles = axes, guides = NULL, ...) {
    stat_name <- stat_name %||% paste0(y, " (", deparse(substitute(add_stat)), ")")
    BoxViolinPlot(
        data = data, x = x, x_sep = x_sep, y = y, base = "box", in_form = in_form,
        split_by = split_by, split_by_sep = split_by_sep,
        sort_x = sort_x, flip = flip, keep_empty = keep_empty, group_by = group_by, group_by_sep = group_by_sep, group_name = group_name,
        paired_by = paired_by, x_text_angle = x_text_angle, fill_mode = fill_mode, fill_reverse = fill_reverse, step_increase = step_increase,
        theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor, alpha = alpha,
        aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction,
        add_point = add_point, pt_color = pt_color, pt_size = pt_size, pt_alpha = pt_alpha, symnum_args = symnum_args,
        jitter_width = jitter_width, jitter_height = jitter_height, stack = stack, y_max = y_max, y_min = y_min,
        add_beeswarm = add_beeswarm, beeswarm_method = beeswarm_method, beeswarm_cex = beeswarm_cex, beeswarm_priority = beeswarm_priority,
        beeswarm_dodge = beeswarm_dodge, add_trend = add_trend, trend_color = trend_color, trend_linewidth = trend_linewidth, trend_ptsize = trend_ptsize,
        add_stat = add_stat, stat_name = stat_name, stat_color = stat_color, stat_size = stat_size, stat_stroke = stat_stroke, stat_shape = stat_shape,
        add_bg = add_bg, bg_palette = bg_palette, bg_palcolor = bg_palcolor, bg_alpha = bg_alpha,
        add_line = add_line, line_color = line_color, line_width = line_width, line_type = line_type,
        highlight = highlight, highlight_color = highlight_color, highlight_size = highlight_size, highlight_alpha = highlight_alpha,
        comparisons = comparisons, ref_group = ref_group, pairwise_method = pairwise_method,
        multiplegroup_comparisons = multiplegroup_comparisons, multiple_method = multiple_method,
        sig_label = sig_label, sig_labelsize = sig_labelsize, hide_ns = hide_ns,
        facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
        title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, seed = seed, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, ...
    )
}

#' @rdname boxviolinplot
#' @export
#' @inheritParams BoxViolinPlot
#' @examples
#' \donttest{
#' ViolinPlot(data, x = "x", y = "y")
#' ViolinPlot(data, x = "x", y = "y", add_beeswarm = TRUE, pt_color = "grey30")
#' ViolinPlot(data, x = "x", y = "y", add_box = TRUE)
#' ViolinPlot(data, x = "x", y = "y", add_point = TRUE)
#' ViolinPlot(data, x = "x", y = "y", add_trend = TRUE)
#' ViolinPlot(data, x = "x", y = "y", add_stat = mean)
#' ViolinPlot(data, x = "x", y = "y", add_bg = TRUE)
#' ViolinPlot(data, x = "x", y = "y", add_line = 0)
#' ViolinPlot(data, x = "x", y = "y", group_by = "group1")
#' ViolinPlot(data,
#'     x = "x", y = "y", group_by = "group1",
#'     facet_by = "group2", add_box = TRUE
#' )
#' ViolinPlot(data, x = "x", y = "y", add_point = TRUE, highlight = 'group1 == "g1"',
#'     alpha = 0.8, highlight_size = 1.5, pt_size = 1, add_box = TRUE)
#' ViolinPlot(data,
#'     x = "x", y = "y", group_by = "group1",
#'     comparisons = TRUE, sig_label = "p = {p}"
#' )
#' ViolinPlot(data,
#'     x = "x", y = "y", sig_label = "p.format", hide_ns = TRUE,
#'     facet_by = "group2", comparisons = list(c("D", "E"))
#' )
#' ViolinPlot(data,
#'     x = "x", y = "y", fill_mode = "mean",
#'     facet_by = "group2", palette = "Blues", multiplegroup_comparisons = TRUE
#' )
#' ViolinPlot(data,
#'     x = "x", y = "y", fill_mode = "mean",
#'     split_by = "group1", palette = c(g1 = "Blues", g2 = "Reds")
#' )
#' ViolinPlot(data,
#'     x = "x", y = "y", stack = TRUE,
#'     facet_by = "group2", add_box = TRUE, add_bg = TRUE,
#'     bg_palette = "Paired"
#' )
#' }
ViolinPlot <- function(
    data, x, x_sep = "_", y = NULL, in_form = c("long", "wide"),
    split_by = NULL, split_by_sep = "_", symnum_args = NULL,
    sort_x = c("none", "mean_asc", "mean_desc", "mean", "median_asc", "median_desc", "median"),
    flip = FALSE, keep_empty = FALSE, group_by = NULL, group_by_sep = "_", group_name = NULL,
    paired_by = NULL, x_text_angle = ifelse(isTRUE(flip) && isTRUE(stack), 90, 45), step_increase = 0.1,
    fill_mode = ifelse(!is.null(group_by), "dodge", "x"), fill_reverse = FALSE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = NULL, legend.position = "right", legend.direction = "vertical",
    add_point = FALSE, pt_color = if(isTRUE(add_beeswarm)) NULL else "grey30", pt_size = NULL, pt_alpha = 1,
    jitter_width = NULL, jitter_height = 0, stack = FALSE, y_max = NULL, y_min = NULL,
    add_beeswarm = FALSE, beeswarm_method = "swarm", beeswarm_cex = 1, beeswarm_priority = "ascending",
    beeswarm_dodge = 0.9, add_box = FALSE, box_color = "black", box_width = 0.1, box_ptsize = 2.5,
    add_trend = FALSE, trend_color = NULL, trend_linewidth = 1, trend_ptsize = 2,
    add_stat = NULL, stat_name = NULL, stat_color = "black", stat_size = 1, stat_stroke = 1, stat_shape = 25,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    add_line = NULL, line_color = "red2", line_width = .6, line_type = 2,
    highlight = NULL, highlight_color = "red2", highlight_size = 1, highlight_alpha = 1,
    comparisons = NULL, ref_group = NULL, pairwise_method = "wilcox.test",
    multiplegroup_comparisons = FALSE, multiple_method = "kruskal.test",
    sig_label = "p.format", sig_labelsize = 3.5, hide_ns = FALSE,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    axes = NULL, axis_titles = axes, guides = NULL, ...) {
    stat_name <- stat_name %||% paste0(y, " (", deparse(substitute(add_stat)), ")")
    BoxViolinPlot(
        data = data, x = x, x_sep = x_sep, y = y, base = "violin", in_form = in_form,
        split_by = split_by, split_by_sep = split_by_sep,
        sort_x = sort_x, flip = flip, keep_empty = keep_empty, group_by = group_by, group_by_sep = group_by_sep, group_name = group_name,
        paired_by = paired_by, x_text_angle = x_text_angle, fill_mode = fill_mode, fill_reverse = fill_reverse, step_increase = step_increase,
        theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor, alpha = alpha,
        aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction,
        add_point = add_point, pt_color = pt_color, pt_size = pt_size, pt_alpha = pt_alpha, symnum_args = symnum_args,
        jitter_width = jitter_width, jitter_height = jitter_height, stack = stack, y_max = y_max, y_min = y_min,
        add_beeswarm = add_beeswarm, beeswarm_method = beeswarm_method, beeswarm_cex = beeswarm_cex, beeswarm_priority = beeswarm_priority,
        beeswarm_dodge = beeswarm_dodge, add_box = add_box, box_color = box_color, box_width = box_width, box_ptsize = box_ptsize,
        add_trend = add_trend, trend_color = trend_color, trend_linewidth = trend_linewidth, trend_ptsize = trend_ptsize,
        add_stat = add_stat, stat_name = stat_name, stat_color = stat_color, stat_size = stat_size, stat_stroke = stat_stroke, stat_shape = stat_shape,
        add_bg = add_bg, bg_palette = bg_palette, bg_palcolor = bg_palcolor, bg_alpha = bg_alpha,
        add_line = add_line, line_color = line_color, line_width = line_width, line_type = line_type,
        highlight = highlight, highlight_color = highlight_color, highlight_size = highlight_size, highlight_alpha = highlight_alpha,
        comparisons = comparisons, ref_group = ref_group, pairwise_method = pairwise_method,
        multiplegroup_comparisons = multiplegroup_comparisons, multiple_method = multiple_method,
        sig_label = sig_label, sig_labelsize = sig_labelsize, hide_ns = hide_ns,
        facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
        title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, seed = seed, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, ...
    )
}

#' @rdname boxviolinplot
#' @export
#' @inheritParams BoxViolinPlot
#' @param add_violin Logical, whether to add violin plot behind the beeswarm points.
#' Adding violin to a beeswarm plot is actually not supported. A message will be shown to
#' remind users to use `ViolinPlot(..., add_beeswarm = TRUE)` instead.
#' @examples
#' \donttest{
#' # Beeswarm plot examples
#' BeeswarmPlot(data, x = "x", y = "y")
#' BeeswarmPlot(data, x = "x", y = "y", pt_size = 1)
#' BeeswarmPlot(data, x = "x", y = "y", add_box = TRUE, pt_color = "grey30")
#' # Equivalent to:
#' # BoxPlot(data, x = "x", y = "y", add_beeswarm = TRUE, pt_color = "grey30")
#'
#' BeeswarmPlot(data, x = "x", y = "y", group_by = "group1")
#' # no dodging
#' BeeswarmPlot(data, x = "x", y = "y", group_by = "group1", beeswarm_dodge = NULL)
#'
#' BeeswarmPlot(data,
#'     x = "x", y = "y", beeswarm_method = "hex",
#'     beeswarm_cex = 2
#' )
#' }
BeeswarmPlot <- function(
    data, x, x_sep = "_", y = NULL, in_form = c("long", "wide"),
    split_by = NULL, split_by_sep = "_", symnum_args = NULL,
    sort_x = c("none", "mean_asc", "mean_desc", "mean", "median_asc", "median_desc", "median"),
    flip = FALSE, keep_empty = FALSE, group_by = NULL, group_by_sep = "_", group_name = NULL,
    paired_by = NULL, x_text_angle = ifelse(isTRUE(flip) && isTRUE(stack), 90, 45), step_increase = 0.1,
    fill_mode = ifelse(!is.null(group_by), "dodge", "x"), fill_reverse = FALSE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1,
    aspect.ratio = NULL, legend.position = "right", legend.direction = "vertical",
    pt_color = NULL, pt_size = NULL, pt_alpha = 1,
    jitter_width = NULL, jitter_height = 0, stack = FALSE, y_max = NULL, y_min = NULL, add_violin = FALSE,
    beeswarm_method = "swarm", beeswarm_cex = 1, beeswarm_priority = "ascending", beeswarm_dodge = 0.9,
    add_box = FALSE, box_color = "black", box_width = 0.1, box_ptsize = 2.5,
    add_trend = FALSE, trend_color = NULL, trend_linewidth = 1, trend_ptsize = 2,
    add_stat = NULL, stat_name = NULL, stat_color = "black", stat_size = 1, stat_stroke = 1, stat_shape = 25,
    add_bg = FALSE, bg_palette = "stripe", bg_palcolor = NULL, bg_alpha = 0.2,
    add_line = NULL, line_color = "red2", line_width = .6, line_type = 2,
    highlight = NULL, highlight_color = "red2", highlight_size = 1, highlight_alpha = 1,
    comparisons = NULL, ref_group = NULL, pairwise_method = "wilcox.test",
    multiplegroup_comparisons = FALSE, multiple_method = "kruskal.test",
    sig_label = "p.format", sig_labelsize = 3.5, hide_ns = FALSE,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    title = NULL, subtitle = NULL, xlab = NULL, ylab = NULL, seed = 8525,
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    axes = NULL, axis_titles = axes, guides = NULL, ...) {
    if (isTRUE(add_violin)) {
        stop("Adding violin to a beeswarm plot is not supported. Please use ViolinPlot(..., add_beeswarm = TRUE) instead.")
    }
    stat_name <- stat_name %||% paste0(y, " (", deparse(substitute(add_stat)), ")")
    BoxViolinPlot(
        data = data, x = x, x_sep = x_sep, y = y, base = "none", in_form = in_form,
        split_by = split_by, split_by_sep = split_by_sep,
        sort_x = sort_x, flip = flip, keep_empty = keep_empty, group_by = group_by, group_by_sep = group_by_sep, group_name = group_name,
        paired_by = paired_by, x_text_angle = x_text_angle, fill_mode = fill_mode, fill_reverse = fill_reverse, step_increase = step_increase,
        theme = theme, theme_args = theme_args, palette = palette, palcolor = palcolor, alpha = alpha,
        aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction,
        add_point = TRUE, pt_color = pt_color, pt_size = pt_size, pt_alpha = pt_alpha, symnum_args = symnum_args,
        jitter_width = jitter_width, jitter_height = jitter_height, stack = stack, y_max = y_max, y_min = y_min,
        add_beeswarm = TRUE, beeswarm_method = beeswarm_method, beeswarm_cex = beeswarm_cex, beeswarm_priority = beeswarm_priority,
        beeswarm_dodge = beeswarm_dodge, add_box = add_box, box_color = box_color, box_width = box_width, box_ptsize = box_ptsize,
        add_trend = add_trend, trend_color = trend_color, trend_linewidth = trend_linewidth, trend_ptsize = trend_ptsize,
        add_stat = add_stat, stat_name = stat_name, stat_color = stat_color, stat_size = stat_size, stat_stroke = stat_stroke, stat_shape = stat_shape,
        add_bg = add_bg, bg_palette = bg_palette, bg_palcolor = bg_palcolor, bg_alpha = bg_alpha,
        add_line = add_line, line_color = line_color, line_width = line_width, line_type = line_type,
        highlight = highlight, highlight_color = highlight_color, highlight_size = highlight_size, highlight_alpha = highlight_alpha,
        comparisons = comparisons, ref_group = ref_group, pairwise_method = pairwise_method,
        multiplegroup_comparisons = multiplegroup_comparisons, multiple_method = multiple_method,
        sig_label = sig_label, sig_labelsize = sig_labelsize, hide_ns = hide_ns,
        facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
        title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, seed = seed, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, ...
    )
}
