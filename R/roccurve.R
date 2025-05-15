#' Prepare the cutoff data for the ROC curve
#'
#' @param data A data frame with the truth and score columns.
#' @param truth_by A character string of the column name that contains the true class labels.
#' @param score_by A character string of the column name that contains the predicted scores.
#' @param cat_by A character string of the column name to categorize/group the data.
#' If specified, the cutoffs will be calculated for each category.
#' @param cutoffs_at Vector of user supplied cutoffs to plot as points. If non-NULL,
#' it will override the values of n_cuts and plot the observed cutoffs closest to the user-supplied ones.
#' @param cutoffs_labels vector of user-supplied labels for the cutoffs. Must be a character vector of the
#' same length as cutoffs_at.
#' @param n_cuts An integer to specify the number of cuts on the ROC curve.
#' @param increasing TRUE if the score is increasing with the truth (1), FALSE otherwise.
#' @return A data frame with the cutoffs and the corresponding x and y values.
#' @keywords internal
get_cutoffs_data <- function(
    data, truth_by, score_by, cat_by, cutoffs_at = NULL, cutoffs_labels = NULL,
    cutoffs_accuracy = 0.01, n_cuts = 0, increasing = TRUE
) {
    if (is.null(cutoffs_at) && n_cuts == 0) {
        return(NULL)
    }

    if (is.null(cutoffs_at) && n_cuts > 0) {
        cutoffs_at <- quantile(data[[score_by]], probs = seq(0, 1, length.out = n_cuts + 2))
        cutoffs_at <- unname(cutoffs_at[-c(1, length(cutoffs_at))])
    }

    stopifnot("cutoffs_labels should be NULL or a character vector of the same length as cutoffs_at." =
        is.null(cutoffs_labels) || length(cutoffs_labels) == length(cutoffs_at))

    cutoff_methods = c(
        "CB", "MCT", "MinValueSp", "MinValueSe", "ValueSe", "MinValueSpSe", "MaxSp", "MaxSe", "MaxSpSe",
        "MaxProdSpSe", "ROC01", "SpEqualSe", "Youden", "MaxEfficiency", "Minimax", "MaxDOR", "MaxKappa",
        "MinValueNPV", "MinValuePPV", "ValueNPV", "ValuePPV", "MinValueNPVPPV", "PROC01", "NPVEqualPPV",
        "MaxNPVPPV", "MaxSumNPVPPV", "MaxProdNPVPPV", "ValueDLR.Negative", "ValueDLR.Positive", "MinPvalue",
        "ObservedPrev", "MeanPrev", "PrevalenceMatching"
    )

    # Get sensitivity and specificity for given cutoff
    get_se_sp <- function(df, cutoff, cutoff_in = NULL) {
        cutoff_in <- cutoff_in %||% cutoff
        if (is.na(cutoff)) {
            stop("[ROCCurve] Invalid cutoffs_at '", cutoff_in, "', which numerized as NA.")
        }
        min_score <- min(df[[score_by]], na.rm = TRUE)
        max_score <- max(df[[score_by]], na.rm = TRUE)
        if (cutoff > max_score || cutoff < min_score) {
            stop("[ROCCurve] Invalid cutoffs_at '", cutoff_in, "', which is out of range of the score [", min_score, ", ", max_score, "].")
        }

        if (increasing) {
            tp <- sum(df[[truth_by]] == 1 & df[[score_by]] >= cutoff)
            fp <- sum(df[[truth_by]] == 0 & df[[score_by]] >= cutoff)
            tn <- sum(df[[truth_by]] == 0 & df[[score_by]] < cutoff)
            fn <- sum(df[[truth_by]] == 1 & df[[score_by]] < cutoff)
        } else {
            tp <- sum(df[[truth_by]] == 1 & df[[score_by]] <= cutoff)
            fp <- sum(df[[truth_by]] == 0 & df[[score_by]] <= cutoff)
            tn <- sum(df[[truth_by]] == 0 & df[[score_by]] > cutoff)
            fn <- sum(df[[truth_by]] == 1 & df[[score_by]] > cutoff)
        }

        se <- tp / (tp + fn)
        sp <- tn / (tn + fp)
        c(cutoff = cutoff, x = 1 - sp, y = se)
    }

    # get the cutoff value and the corresponding sensitivity and specificity
    # for a given method
    get_cutoff <- function(df, cutoff, label) {
        if (!cutoff %in% cutoff_methods) {
            cutoff_num <- as.numeric(cutoff)
            se_sp <- get_se_sp(df, cutoff_num, cutoff)
            label <- label %||% scales::number(cutoff_num, accuracy = cutoffs_accuracy)
        } else {
            cutoff_info <- OptimalCutpoints::optimal.cutpoints(
                X = score_by, status = truth_by, methods = cutoff, data = as.data.frame(df),
                direction = ifelse(increasing, ">", "<"), tag.healthy = 0
            )
            cutoff_num <- tryCatch({
                cutoff_info[[cutoff]][[1]]$optimal.cutoff$cutoff[1]
            }, error = function(e) {
                warning("No optimal cutoff found for ", cutoff, immediate. = TRUE)
                NA
            })
            se_sp <- get_se_sp(df, cutoff_num, cutoff)
            label <- label %||% paste0(scales::number(cutoff_num, cutoffs_accuracy), " (by ", cutoff, ")")
        }
        c(se_sp, label = label)
    }
    splits <- split(data, data[[cat_by]])
    do.call(rbind, lapply(names(splits), function(ns) {
        df <- do.call(rbind, lapply(seq_along(cutoffs_at), function(i) {
            get_cutoff(splits[[ifelse(identical(ns, ""), 1, ns)]], cutoffs_at[i], if (is.null(cutoffs_labels)) NULL else cutoffs_labels[i])
        }))
        df <- as.data.frame(df)
        df[[cat_by]] <- ns
        df$x <- as.numeric(df$x)
        df$y <- as.numeric(df$y)
        df
    }))
}


#' Atomic ROC curve
#'
#' @inheritParams common_args
#' @param data A data frame with the truth and score columns.
#' See also https://cran.r-project.org/web/packages/plotROC/vignettes/examples.html.
#' @param truth_by A character string of the column name that contains the true class labels.
#' (a.k.a. the binary outcome, 1/0 or TRUE/FALSE.)
#' @param score_by character strings of the column names that contains the predicted scores.
#' When multiple columns are provided, the ROC curve is plotted for each column.
#' @param pos_label A character string of the positive class label.
#' When NULL, the labels will be handled by the `plotROC` package.
#' @param group_by A character vector of column names to group the ROC curve by.
#' When `score_by` contains multiple columns, `group_by` should be NULL.
#' @param group_by_sep A character string to separate the columns in `group_by`.
#' @param group_name A character string to name the legend of the ROC curve groups.
#' @param x_axis_reverse A logical to reverse the x-axis, that is from 1 to 0.
#' @param percent A logical to display the x and y axis as percentages.
#' @param ci A list of arguments to pass to [plotROC::geom_rocci()] to add confidence intervals.
#' When NULL, no confidence intervals are added.
#' @param n_cuts An integer to specify the number of cutpoints on the ROC curve.
#' It will be the quantiles of the predicted scores.
#' @param cutoffs_at Vector of user supplied cutoffs to plot as points. If non-NULL,
#' it will override the values of n_cuts and plot the observed cutoffs closest to the user-supplied ones.
#' Both `cutoffs_at` and `cutoffs.labels` will be passed to [plotROC::geom_roc()].
#' Other than numeric values, the following special values are allowed. These values are the methods of
#' [OptimalCutpoints::optimal.cutpoints()], they are literally:
#' * "CB" (cost-benefit method);
#' * "MCT" (minimizes Misclassification Cost Term);
#' * "MinValueSp" (a minimum value set for Specificity);
#' * "MinValueSe" (a minimum value set for Sensitivity);
#' * "ValueSe" (a value set for Sensitivity);
#' * "MinValueSpSe" (a minimum value set for Specificity and Sensitivity);
#' * "MaxSp" (maximizes Specificity);
#' * "MaxSe" (maximizes Sensitivity);
#' * "MaxSpSe" (maximizes Sensitivity and Specificity simultaneously);
#' * "MaxProdSpSe" (maximizes the product of Sensitivity and Specificity or Accuracy Area);
#' * "ROC01" (minimizes distance between ROC plot and point (0,1));
#' * "SpEqualSe" (Sensitivity = Specificity);
#' * "Youden" (Youden Index);
#' * "MaxEfficiency" (maximizes Efficiency or Accuracy, similar to minimize Error Rate);
#' * "Minimax" (minimizes the most frequent error);
#' * "MaxDOR" (maximizes Diagnostic Odds Ratio);
#' * "MaxKappa" (maximizes Kappa Index);
#' * "MinValueNPV" (a minimum value set for Negative Predictive Value);
#' * "MinValuePPV" (a minimum value set for Positive Predictive Value);
#' * "ValueNPV" (a value set for Negative Predictive Value);
#' * "ValuePPV" (a value set for Positive Predictive Value);
#' * "MinValueNPVPPV" (a minimum value set for Predictive Values);
#' * "PROC01" (minimizes distance between PROC plot and point (0,1));
#' * "NPVEqualPPV" (Negative Predictive Value = Positive Predictive Value);
#' * "MaxNPVPPV" (maximizes Positive Predictive Value and Negative Predictive Value simultaneously);
#' * "MaxSumNPVPPV" (maximizes the sum of the Predictive Values);
#' * "MaxProdNPVPPV" (maximizes the product of Predictive Values);
#' * "ValueDLR.Negative" (a value set for Negative Diagnostic Likelihood Ratio);
#' * "ValueDLR.Positive" (a value set for Positive Diagnostic Likelihood Ratio);
#' * "MinPvalue" (minimizes p-value associated with the statistical Chi-squared test which measures the association between
#'   the marker and the binary result obtained on using the cutpoint);
#' * "ObservedPrev" (The closest value to observed prevalence);
#' * "MeanPrev" (The closest value to the mean of the diagnostic test values);
#' * "PrevalenceMatching" (The value for which predicted prevalence is practically equal to observed prevalence).
#' @param cutoffs_labels vector of user-supplied labels for the cutoffs. Must be a character vector of the
#' same length as cutoffs_at.
#' @param cutoffs_accuracy A numeric to specify the accuracy of the cutoff values to show.
#' @param cutoffs_pt_size A numeric to specify the size of the cutoff points.
#' @param cutoffs_pt_shape A numeric to specify the shape of the cutoff points.
#' @param cutoffs_pt_stroke A numeric to specify the stroke of the cutoff points.
#' @param cutoffs_labal_fg A character string to specify the color of the cutoff labels.
#' @param cutoffs_label_size A numeric to specify the size of the cutoff labels.
#' @param cutoffs_label_bg A character string to specify the background color of the cutoff labels.
#' @param cutoffs_label_bg_r A numeric to specify the radius of the background of the cutoff labels.
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
#' @importFrom dplyr mutate summarise pull
#' @importFrom tidyr unite pivot_longer separate
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggplot2 geom_abline scale_color_manual scale_x_reverse scale_x_continuous scale_y_continuous waiver
#' @importFrom ggplot2 geom_text labs
ROCCurveAtomic <- function(data, truth_by, score_by, pos_label = NULL,
    group_by = NULL, group_by_sep = "_", group_name = NULL, x_axis_reverse = FALSE, percent = FALSE, ci = NULL,
    n_cuts = 0, cutoffs_at = NULL, cutoffs_labels = NULL, cutoffs_accuracy = 0.01, cutoffs_pt_size = 5, cutoffs_pt_shape = 4,
    cutoffs_pt_stroke = 1, cutoffs_labal_fg = "black", cutoffs_label_size = 4, cutoffs_label_bg = "white", cutoffs_label_bg_r = 0.1,
    show_auc = c("auto", "none", "legend", "plot"), auc_accuracy = 0.01, auc_size = 4, increasing = TRUE,
    theme = "theme_this", theme_args = list(),  palette = "Spectral", palcolor = NULL, alpha = 1,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, legend.position = waiver(), legend.direction = "vertical", title = NULL, subtitle = NULL,
    xlab = ifelse(x_axis_reverse, "Specificity", "1 - Specificity"), ylab = "Sensitivity", ...) {
    show_auc <- match.arg(show_auc)
    truth_by <- check_columns(data, truth_by, allow_multi = FALSE)
    score_by <- check_columns(data, score_by, force_factor = FALSE, allow_multi = TRUE)
    group_by <- check_columns(data, group_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = group_by_sep)
    stopifnot("'group_by' should be NULL when mulitple 'score_by' columns are provided." = length(score_by) == 1 || is.null(group_by))

    if (!is.null(pos_label)) {
        data[[truth_by]] <- factor(data[[truth_by]], levels = c(setdiff(unique(data[[truth_by]]), pos_label), pos_label))
        data[[truth_by]] <- as.numeric(data[[truth_by]]) - 1
    } else if (is.factor(data[[truth_by]])) {
        warning(
            "'pos_label' is NULL, the last level of '",
            truth_by,
            "' (",
            levels(data[[truth_by]])[length(levels(data[[truth_by]]))],
            ") will be used as the positive label."
        )
        data[[truth_by]] <- as.numeric(data[[truth_by]]) - 1
    } else if (!is.numeric(data[[truth_by]])) {
        data[[truth_by]] <- factor(data[[truth_by]])
        warning(
            "'pos_label' is NULL, value '",
            levels(data[[truth_by]])[length(levels(data[[truth_by]]))],
            "' from '",
            truth_by,
            "' will be used as the positive label."
        )
        data[[truth_by]] <- as.numeric(data[[truth_by]]) - 1
    }

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
        # plotROC::geom_roc() doesn't support different cutoffs for different groups/facets
        plotROC::geom_roc(n.cuts = 0, cutoffs.at = NULL, cutoff.labels = NULL, increasing = increasing, ...)

    # facet the plot to calculate the correct AUCs for each facet
    attr(p, "height") <- attr(p, "width") <- 0
    auc <- plotROC::calc_auc(
        facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow))

    cutoffs_df <- get_cutoffs_data(
        data %>% unite(".cat", !!!syms(unique(c(group_by, facet_by))), sep = " // "),
        truth_by, score_by, ".cat", cutoffs_at, cutoffs_labels, cutoffs_accuracy, n_cuts, increasing)

    if (!is.null(cutoffs_df)) {
        cutoffs_df <- cutoffs_df %>%
            separate(".cat", into = unique(c(group_by, facet_by)), sep = " // ")

        p <- p + geom_point(
            data = cutoffs_df, aes(x = !!sym("x"), y = !!sym("y"), color = !!sym(group_by)), inherit.aes = FALSE, shape = cutoffs_pt_shape,
            size = cutoffs_pt_size, stroke = cutoffs_pt_stroke
        ) + geom_text_repel(
            data = cutoffs_df, aes(label = !!sym("label"), x = !!sym("x"), y = !!sym("y")), inherit.aes = FALSE,
            size = cutoffs_label_size * text_size_scale, color = cutoffs_labal_fg, nudge_x = 0.01,
            bg.color = cutoffs_label_bg, bg.r = cutoffs_label_bg_r, min.segment.length = 10
        )
    }

    if (!is.null(ci)) {
        p <- p + do.call(plotROC::geom_rocci, ci)
    }

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

        auc_pos_x <- ifelse(increasing, ifelse(x_axis_reverse, -1, 1), 0)
        auc_pos_y <- ifelse(increasing, 0, 1)
        auc_pos_hjust <- ifelse(increasing == x_axis_reverse, -0.05, 1.05)
        auc_pos_vjust <- ifelse(increasing, -0.5 / nlevels(data[[group_by]]), 1 + .5 / nlevels(data[[group_by]]))
        p <- p + geom_text(
            data = auc_df, x = auc_pos_x, y = auc_pos_y, size = auc_size * text_size_scale,
            hjust = auc_pos_hjust, vjust = auc_pos_vjust, aes(label = !!sym("AUC")), inherit.aes = FALSE)
        labels <- levels(data[[group_by]])
    } else if (show_auc == "legend") {
        if (!is.null(facet_by)) {
            auc_df <- auc[order(auc[[facet_by]], auc[[group_by]]), , drop = FALSE]
            auc_df$AUC <- paste0("AUC (", auc_df[[facet_by]], ") = ", scales::number(auc$AUC, accuracy = auc_accuracy))
            labels <- auc_df %>% dplyr::group_by(!!sym(group_by)) %>%
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
            values = palette_this(levels(data[[group_by]]), palette = palette, palcolor = palcolor, alpha = alpha),
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
#' # will warn about the positive label
#' ROCCurve(data, truth_by = "D.str", score_by = "M1")
#' ROCCurve(data, truth_by = "D", score_by = "M1", increasing = FALSE)
#' # Multiple ROC curves
#' ROCCurve(data, truth_by = "D", score_by = c("M1", "M2"), group_name = "Method")
#' ROCCurve(data, truth_by = "D", score_by = "M1", group_by = "gender", show_auc = "plot")
#' # Reverse the x-axis and display the axes as percentages
#' ROCCurve(data, truth_by = "D", score_by = "M1", x_axis_reverse = TRUE, percent = TRUE)
#' # Pass additional arguments to geom_roc and make the curve black
#' ROCCurve(data, truth_by = "D", score_by = "M1", n_cuts = 10, palcolor = "black")
#' # Add confidence intervals
#' ROCCurve(data, truth_by = "D", score_by = "M1", ci = list(sig.level = .01))
#' # Facet by a column
#' ROCCurve(data, truth_by = "D", score_by = "M1", facet_by = "gender")
#' # Show cutoffs
#' ROCCurve(data, truth_by = "D", score_by = "M1", cutoffs_at = c(0, "ROC01", "SpEqualSe"))
#' # Split by a column
#' p <- ROCCurve(data, truth_by = "D", score_by = "M1", split_by = "gender")
#' p
#' # Retrieve the AUC values
#' attr(p, "auc")
ROCCurve <- function(data, truth_by, score_by, pos_label = NULL, split_by = NULL, split_by_sep = "_",
    group_by = NULL, group_by_sep = "_", group_name = NULL,
    x_axis_reverse = FALSE, percent = FALSE, ci = NULL, n_cuts = 0,
    cutoffs_at = NULL, cutoffs_labels = NULL, cutoffs_accuracy = 0.01, cutoffs_pt_size = 5, cutoffs_pt_shape = 4,
    cutoffs_pt_stroke = 1, cutoffs_labal_fg = "black", cutoffs_label_size = 4, cutoffs_label_bg = "white", cutoffs_label_bg_r = 0.1,
    show_auc = c("auto", "none", "legend", "plot"), auc_accuracy = 0.01, auc_size = 4,
    theme = "theme_this", theme_args = list(),  palette = "Spectral", palcolor = NULL, alpha = 1,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    aspect.ratio = 1, legend.position = waiver(), legend.direction = "vertical", title = NULL, subtitle = NULL,
    xlab = ifelse(x_axis_reverse, "Specificity", "1 - Specificity"), ylab = "Sensitivity",
    combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE, seed = 8525,
    axes = NULL, axis_titles = axes, guides = NULL, design = NULL, ...) {
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
    legend.direction <- check_legend(legend.direction, names(datas), "legend.direction")
    legend.position <- check_legend(legend.position, names(datas), "legend.position")

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

            ROCCurveAtomic(datas[[nm]], truth_by, score_by, pos_label = pos_label,
                group_by = group_by, group_by_sep = group_by_sep, group_name = group_name,
                x_axis_reverse = x_axis_reverse, percent = percent, ci = ci, n_cuts = n_cuts,
                cutoffs_at = cutoffs_at, cutoffs_labels = cutoffs_labels, cutoffs_accuracy = cutoffs_accuracy,
                cutoffs_pt_size = cutoffs_pt_size, cutoffs_pt_shape = cutoffs_pt_shape, cutoffs_pt_stroke = cutoffs_pt_stroke,
                cutoffs_labal_fg = cutoffs_labal_fg, cutoffs_label_size = cutoffs_label_size, cutoffs_label_bg = cutoffs_label_bg,
                cutoffs_label_bg_r = cutoffs_label_bg_r, show_auc = show_auc, auc_accuracy = auc_accuracy, auc_size = auc_size,
                theme = theme, theme_args = theme_args, palette = palette[[nm]], palcolor = palcolor[[nm]], alpha = alpha,
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                aspect.ratio = aspect.ratio, legend.position = legend.position[[nm]], legend.direction = legend.direction[[nm]],
                title = title, subtitle = subtitle, xlab = xlab, ylab = ylab, ...)
        }
    )

    p <- combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, design = design)

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
