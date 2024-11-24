#' Atomic ROC curve
#'
#' @inheritParams common_args
#' @param data A data frame with the truth and score columns.
#' See also https://cran.r-project.org/web/packages/plotROC/vignettes/examples.html.
#' @param truth_by A character string of the column name that contains the true class labels.
#' (a.k.a. the binary outcome, 1/0 or TRUE/FALSE.)
#' @param score_by character strings of the column names that contains the predicted scores.
#' @param group_by A character vector of column names to group the ROC curve by.
#' When `score_by` contains multiple columns, `group_by` should be NULL.
#' @param group_by_sep A character string to separate the columns in `group_by`.
#' @param group_name A character string to name the legend of the ROC curve groups.
#' @param x_axis_reverse A logical to reverse the x-axis, that is from 1 to 0.
#' @param percent A logical to display the x and y axis as percentages.
#' @param n.cuts An integer to specify the number of cuts on the ROC curve.
#' Passed to [plotROC::geom_roc()].
#' @param ci A list of arguments to pass to [plotROC::geom_rocci()] to add confidence intervals.
#' When NULL, no confidence intervals are added.
#' @param show_auc A character string to specify the position of the AUC values.
#' * "auto" (default): Automatically determine the position based on the plot.
#'   When there is a single group or 'facet_by' is provided, the AUC is placed on the plot.
#'   Otherwise, the AUC is placed in the legend.
#' * "none": Do not display the AUC values.
#' * "legend": Display the AUC values in the legend.
#' * "plot": Display the AUC values on the plot (left/right bottom corner).
#' @param auc_accuracy A numeric to specify the accuracy of the AUC values.
#' @param auc_size A numeric to specify the size of the AUC values when they are displayed on the plot.
#' @return A ggplot object.
#' @keywords internal
#' @importFrom ggplot2 geom_abline scale_color_manual scale_x_reverse scale_x_continuous scale_y_continuous waiver
ROCCurveAtomic <- function(data, truth_by, score_by,
    group_by = NULL, group_by_sep = "_", group_name = NULL,
    x_axis_reverse = FALSE, percent = FALSE, n.cuts = 0, ci = NULL,
    show_auc = c("auto", "none", "legend", "plot"), auc_accuracy = 0.01, auc_size = 4,
    theme = "theme_this", theme_args = list(),  palette = "Spectral", palcolor = NULL, alpha = 1,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, legend.position = waiver(), legend.direction = "vertical", title = NULL, subtitle = NULL,
    xlab = ifelse(x_axis_reverse, "Specificity", "1 - Specificity"), ylab = "Sensitivity", ...) {
    show_auc <- match.arg(show_auc)
    truth_by <- check_columns(data, truth_by, allow_multi = FALSE)
    score_by <- check_columns(data, score_by, force_factor = FALSE, allow_multi = TRUE)
    group_by <- check_columns(data, group_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = group_by_sep)
    stopifnot("'group_by' should be NULL when mulitple 'score_by' columns are provided." = length(score_by) == 1 || is.null(group_by))

    if (length(score_by) > 1) {
        data <- pivot_longer(data, cols = score_by, names_to = ".group", values_to = ".score")
        group_by <- ".group"
        data[[group_by]] <- factor(data[[group_by]], levels = score_by)
        score_by <- ".score"
    }

    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    base_size <- theme_args$base_size %||% 12
    text_size_scale <- base_size / 12

    if (is.null(group_by)) {
        data$..group <- factor("")
        group_by <- "..group"
        legend.position <- ifelse(inherits(legend.position, "waiver"), "none", "right")
    } else {
        legend.position <- ifelse(inherits(legend.position, "waiver"), "right", legend.position)
    }

    # determine where to place the AUC
    if (identical(show_auc, "auto")) {
        if (identical(group_by, "..group") || !is.null(facet_by)) {
            show_auc <- "plot"
        } else {
            show_auc <- "legend"
        }
    }

    p <- ggplot(data, aes(d = !!sym(truth_by), m = !!sym(score_by), color = !!sym(group_by))) +
        plotROC::geom_roc(n.cuts = n.cuts, ...)

    if (!is.null(ci)) {
        p <- p + do.call(plotROC::geom_rocci, ci)
    }
    # facet the plot to calculate the correct AUCs for each facet
    attr(p, "height") <- attr(p, "width") <- 0
    auc <- plotROC::calc_auc(
        facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow))
    if (show_auc == "plot") {
        if (identical(group_by, "..group")) {
            auc_df <- auc
            auc_df$AUC <- paste0("AUC = ", scales::number(auc$AUC, accuracy = auc_accuracy))
        } else {
            auc_df <- if (is.null(facet_by)) auc else auc %>% dplyr::group_by(!!sym(facet_by))
            auc_df <- auc_df %>%
                summarise(AUC = paste0(
                    "AUC (", !!sym(group_by), ") = ", scales::number(!!sym("AUC"), accuracy = auc_accuracy),
                    collapse = "\n"), .groups = "drop")
        }
        p <- p + geom_text(
            data = auc_df, x = if (x_axis_reverse) -Inf else Inf, y = -Inf, size = auc_size * text_size_scale,
            hjust = ifelse(x_axis_reverse, -0.1, 1.1), vjust = -0.5,
            mapping = aes(label = !!sym("AUC")), inherit.aes = FALSE)
        labels <- levels(data[[group_by]])
    } else if (show_auc == "legend") {
        if (!is.null(facet_by)) {
            auc_df <- auc[order(auc[[facet_by]], auc[[group_by]]), , drop = FALSE]
            auc_df$AUC <- paste0("AUC (", auc_df[[facet_by]], ") = ", scales::number(auc$AUC, accuracy = auc_accuracy))
            labels <- auc_df %>% group_by(!!sym(group_by)) %>%
                summarise(AUC = paste(!!sym("AUC"), collapse = "\n"), .groups = "drop") %>%
                mutate(AUC = paste0(!!sym(group_by), "\n", !!sym("AUC"))) %>%
                pull("AUC")
        } else {
            labels <- paste0(auc[[group_by]], " (AUC = ", scales::number(auc$AUC, accuracy = auc_accuracy), ")")
        }
    } else {
        labels <- levels(data[[group_by]])
    }

    p <- p +
        # diagnal line
        geom_abline(intercept = 0, slope = ifelse(x_axis_reverse, -1, 1), linetype = "dashed", color = "grey50") +
        scale_color_manual(
            name = group_name %||% group_by,
            values = palette_this(levels(data[[group_by]]), palette = palette, palcolor = palcolor),
            labels = labels
        )

    if (isTRUE(percent)) {
        p <- p + scale_y_continuous(labels = scales::percent)
    }

    if (x_axis_reverse) {
        p <- p + scale_x_reverse(labels = if (isTRUE(percent)) scales::percent else waiver())
    } else if (isTRUE(percent)) {
        p <- p + scale_x_continuous(labels = scales::percent)
    }

    p <- p +
        labs(title = title, subtitle = subtitle, x = xlab, y = ylab) +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            panel.grid.major = element_line(colour = "grey80", linetype = 2)
        )

    height <- width <- 4.5
    if (!identical(legend.position, "none")) {
        if (legend.position %in% c("right", "left")) {
            width <- width + 1
        } else if (legend.direction == "horizontal") {
            height <- height + 1
        } else {
            width <- width + 2
        }
    }

    attr(p, "auc") <- auc
    attr(p, "height") <- height
    attr(p, "width") <- width

    facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow,
        legend.position = legend.position, legend.direction = legend.direction)
}


#' ROC curve
#'
#' A wrapped function around `plotROC` package to create ROC curves.
#'
#' @inheritParams common_args
#' @inheritParams ROCCurveAtomic
#' @return A `patch_work::wrap_plots` object or a list of them if `combine` is `FALSE`.
#' You can retrieve the AUC values using `attr(p, "auc")` if `combine` is `TRUE`.
#' If `combine` is `FALSE`, The AUC value of each plot can be retrieved using `attr(p[[i]], "auc")`.
#' @export
#' @examples
#' set.seed(8525)
#'
#' D.ex <- rbinom(200, size = 1, prob = .5)
#' M1 <- rnorm(200, mean = D.ex, sd = .65)
#' M2 <- rnorm(200, mean = D.ex, sd = 1.5)
#' gender <- c("Male", "Female")[rbinom(200, 1, .49) + 1]
#'
#' data <- data.frame(D = D.ex, D.str = c("Healthy", "Ill")[D.ex + 1],
#'   gender = gender, M1 = M1, M2 = M2)
#'
#' ROCCurve(data, truth_by = "D", score_by = "M1")
#' # Multiple ROC curves
#' ROCCurve(data, truth_by = "D", score_by = c("M1", "M2"), group_name = "Method")
#' ROCCurve(data, truth_by = "D", score_by = "M1", group_by = "gender", show_auc = "plot")
#' # Reverse the x-axis and display the axes as percentages
#' ROCCurve(data, truth_by = "D", score_by = "M1", x_axis_reverse = TRUE, percent = TRUE)
#' # Pass additional arguments to geom_roc and make the curve black
#' ROCCurve(data, truth_by = "D", score_by = "M1", n.cuts = 10, palcolor = "black")
#' # Add confidence intervals
#' ROCCurve(data, truth_by = "D", score_by = "M1", ci = list(sig.level = .01))
#' # Facet by a column
#' ROCCurve(data, truth_by = "D", score_by = "M1", facet_by = "gender")
#' # Split by a column
#' p <- ROCCurve(data, truth_by = "D", score_by = "M1", split_by = "gender")
#' p
#' # Retrieve the AUC values
#' attr(p, "auc")
ROCCurve <- function(data, truth_by, score_by, split_by = NULL, split_by_sep = "_",
    group_by = NULL, group_by_sep = "_", group_name = NULL,
    x_axis_reverse = FALSE, percent = FALSE, n.cuts = 0, ci = NULL,
    show_auc = c("auto", "none", "legend", "plot"), auc_accuracy = 0.01, auc_size = 4,
    theme = "theme_this", theme_args = list(),  palette = "Spectral", palcolor = NULL, alpha = 1,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, legend.position = waiver(), legend.direction = "vertical", title = NULL, subtitle = NULL,
    xlab = ifelse(x_axis_reverse, "Specificity", "1 - Specificity"), ylab = "Sensitivity",
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, seed = 8525, ...) {
    validate_common_args(seed, facet_by)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = split_by_sep)

    if (!is.null(split_by)) {
        datas <- split(data, data[[split_by]])
        # keep the order of levels
        datas <- datas[levels(data[[split_by]])]
    } else {
        datas <- list(data)
        names(datas) <- "..."
    }

    palette <- check_palette(palette, names(datas))
    palcolor <- check_palcolor(palcolor, names(datas))

    plots <- lapply(
        names(datas), function(nm) {
            default_title <- if (length(datas) == 1 && identical(nm, "...")) NULL else nm
            if (is.function(title)) {
                title <- title(default_title)
            } else {
                title <- title %||% default_title
            }
            if (!is.null(split_by)) {
                datas[[nm]][[split_by]] <- NULL
            }

            ROCCurveAtomic(datas[[nm]], truth_by, score_by,
                group_by = group_by, group_by_sep = group_by_sep, group_name = group_name,
                x_axis_reverse = x_axis_reverse, percent = percent, n.cuts = n.cuts, ci = ci,
                show_auc = show_auc, auc_accuracy = auc_accuracy, auc_size = auc_size,
                theme = theme, theme_args = theme_args, palette = palette[[nm]], palcolor = palcolor[[nm]], alpha = alpha,
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                aspect.ratio = aspect.ratio, legend.position = legend.position, legend.direction = legend.direction, title = title, subtitle = subtitle,
                xlab = xlab, ylab = ylab, ...)
        }
    )

    p <- combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow)
    if (!combine) {
        return(p)
    } else {
        attr(p, "auc") <- do.call(rbind, lapply(seq_along(plots), function(i) {
            df <- attr(plots[[i]], "auc")
            if (!is.null(split_by)) {
                df[[split_by]] <- names(datas)[i]
            }
            df
        }))
        return(p)
    }
}
