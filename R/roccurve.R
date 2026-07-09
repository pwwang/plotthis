#' Prepare cutoff data for ROC curve annotation
#'
#' Internal helper that computes the sensitivity and specificity values for a
#' set of user-supplied cutoff points (or computed via OptimalCutpoints methods)
#' across categories defined by group_by and/or facet_by columns. Used by
#' \code{ROCCurveAtomic()} to annotate the ROC curve with cutoff markers and labels.
#'
#' Cutoffs can be specified either as numeric values (raw score thresholds) or as
#' method names from the \code{OptimalCutpoints} package for automatic optimal
#' cutoff identification. When \code{n_cuts > 0}, \code{n_cuts} evenly-spaced
#' quantile values of the score distribution are used as cutoffs.
#'
#' @param data A data frame with the truth and score columns.
#' @param truth_by A character string of the column name that contains the true
#'   class labels (binary, 0/1 or TRUE/FALSE).
#' @param score_by A character string of the column name that contains the predicted scores.
#' @param cat_by A character string of the column name to categorise/group the data.
#'   When specified, cutoffs are calculated separately for each category level,
#'   enabling per-group or per-facet cutoff annotation.
#' @param cutoffs_at A vector of user-supplied cutoff values to plot as points.
#'   When non-NULL, overrides \code{n_cuts}. Supports both raw numeric values
#'   and method names from \code{\link[OptimalCutpoints]{optimal.cutpoints}}.
#' @param cutoffs_labels A character vector of user-supplied labels for the
#'   cutoffs. Must be the same length as \code{cutoffs_at}. When NULL, labels
#'   are generated automatically.
#' @param cutoffs_accuracy A numeric value specifying the rounding precision
#'   for automatically generated cutoff labels. Default: \code{0.001}.
#' @param n_cuts An integer specifying the number of evenly-spaced quantile-based
#'   cutoff points. Ignored when \code{cutoffs_at} is non-NULL. Default: \code{0}
#'   (no quantile cutoffs).
#' @param increasing A logical value. If TRUE (default), higher scores indicate
#'   the positive class; if FALSE, lower scores indicate the positive class.
#' @return A data frame with columns \code{cutoff}, \code{x} (1 - specificity),
#'   \code{y} (sensitivity), \code{label}, and \code{cat_by} (the category name).
#' @keywords internal
get_cutoffs_data <- function(
    data,
    truth_by,
    score_by,
    cat_by,
    cutoffs_at = NULL,
    cutoffs_labels = NULL,
    cutoffs_accuracy = 0.001,
    n_cuts = 0,
    increasing = TRUE
) {
    if (is.null(cutoffs_at) && n_cuts == 0) {
        return(NULL)
    }

    if (is.null(cutoffs_at) && n_cuts > 0) {
        cutoffs_at <- quantile(
            data[[score_by]],
            probs = seq(0, 1, length.out = n_cuts + 2)
        )
        cutoffs_at <- unname(cutoffs_at[-c(1, length(cutoffs_at))])
    }

    stopifnot(
        "cutoffs_labels should be NULL or a character vector of the same length as cutoffs_at." = is.null(
            cutoffs_labels
        ) ||
            length(cutoffs_labels) == length(cutoffs_at)
    )

    cutoff_methods <- c(
        "CB",
        "MCT",
        "MinValueSp",
        "MinValueSe",
        "ValueSe",
        "MinValueSpSe",
        "MaxSp",
        "MaxSe",
        "MaxSpSe",
        "MaxProdSpSe",
        "ROC01",
        "SpEqualSe",
        "Youden",
        "MaxEfficiency",
        "Minimax",
        "MaxDOR",
        "MaxKappa",
        "MinValueNPV",
        "MinValuePPV",
        "ValueNPV",
        "ValuePPV",
        "MinValueNPVPPV",
        "PROC01",
        "NPVEqualPPV",
        "MaxNPVPPV",
        "MaxSumNPVPPV",
        "MaxProdNPVPPV",
        "ValueDLR.Negative",
        "ValueDLR.Positive",
        "MinPvalue",
        "ObservedPrev",
        "MeanPrev",
        "PrevalenceMatching"
    )

    # Get sensitivity and specificity for given cutoff
    get_se_sp <- function(df, cutoff, cutoff_in = NULL) {
        cutoff_in <- cutoff_in %||% cutoff
        if (is.na(cutoff)) {
            stop(
                "[ROCCurve] Invalid cutoffs_at '",
                cutoff_in,
                "', which numerized as NA."
            )
        }
        min_score <- min(df[[score_by]], na.rm = TRUE)
        max_score <- max(df[[score_by]], na.rm = TRUE)
        if (cutoff > max_score || cutoff < min_score) {
            stop(
                "[ROCCurve] Invalid cutoffs_at '",
                cutoff_in,
                "', which is out of range of the score [",
                min_score,
                ", ",
                max_score,
                "]."
            )
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
            label <- label %||%
                scales::number(cutoff_num, accuracy = cutoffs_accuracy)
            return(list(c(se_sp, label = label)))
        } else {
            cutoff_info <- OptimalCutpoints::optimal.cutpoints(
                X = score_by,
                status = truth_by,
                methods = cutoff,
                data = as.data.frame(df),
                direction = ifelse(increasing, ">", "<"),
                tag.healthy = 0
            )
            n_cutoff_num <- tryCatch(
                {
                    length(cutoff_info[[cutoff]][[1]]$optimal.cutoff$cutoff)
                },
                error = function(e) {
                    warning(
                        "No optimal cutoff found for ",
                        cutoff,
                        immediate. = TRUE
                    )
                    0
                }
            )
            if (n_cutoff_num == 0) {
                se_sp <- get_se_sp(df, NA, cutoff)
                label <- label %||%
                    paste0("No optimal cutoff found for ", cutoff)
                return(list(c(se_sp, label = label)))
            } else {
                out <- lapply(seq_len(n_cutoff_num), function(i) {
                    cutoff_num <- cutoff_info[[cutoff]][[
                        1
                    ]]$optimal.cutoff$cutoff[i]
                    se_sp <- get_se_sp(df, cutoff_num, cutoff)
                    label <- cutoffs_labels[i] %||%
                        paste0(
                            scales::number(cutoff_num, cutoffs_accuracy),
                            " (by ",
                            cutoff,
                            ")"
                        )
                    c(se_sp, label = label)
                })
                return(out)
            }
        }
    }
    splits <- split(data, data[[cat_by]])
    do_call(
        rbind,
        lapply(names(splits), function(ns) {
            df <- do_call(
                rbind,
                lapply(seq_along(cutoffs_at), function(i) {
                    co <- get_cutoff(
                        splits[[ifelse(identical(ns, ""), 1, ns)]],
                        cutoffs_at[i],
                        if (is.null(cutoffs_labels)) NULL else cutoffs_labels[i]
                    )
                    co <- t(as.data.frame(co))
                    rownames(co) <- NULL
                    co
                })
            )
            df <- as.data.frame(df)
            df[[cat_by]] <- ns
            df$x <- as.numeric(df$x)
            df$y <- as.numeric(df$y)
            df
        })
    )
}


#' Atomic ROC curve (internal)
#'
#' Core implementation for drawing a single Receiver Operating Characteristic
#' (ROC) curve. This is the internal workhorse behind the exported
#' \code{\link{ROCCurve}} function. It takes a **single** data frame (no
#' \code{split_by} support) and returns a \code{ggplot} object.
#'
#' The function produces an ROC curve using \code{plotROC::geom_roc()}, with
#' the following capabilities:
#' \itemize{
#'   \item \strong{Multiple classifiers} — \code{score_by} accepts multiple
#'   column names, automatically pivoting them into a grouped format so
#'   several prediction scores can be compared on a single plot.
#'   \item \strong{AUC calculation} — area under the curve is computed via
#'   \code{plotROC::calc_auc()} and displayed either on the plot or in the
#'   legend, controlled by \code{show_auc}.
#'   \item \strong{Cutoff annotation} — user-specified cutoffs (numeric score
#'   thresholds or named optimal-cutoff methods from the
#'   \code{OptimalCutpoints} package) are rendered as markers with labels,
#'   using \code{ggrepel::geom_text_repel()} for label placement.
#'   \item \strong{Confidence intervals} — optional ROC confidence bands via
#'   \code{plotROC::geom_rocci()}.
#'   \item \strong{Axis flexibility} — supports reversed x-axis (displaying
#'   specificity) and percent-scaled axes.
#' }
#'
#' @section Architecture:
#' \enumerate{
#'   \item \strong{show_auc resolution} — \code{match.arg()} resolves
#'   \code{show_auc} to one of \code{"auto"}, \code{"none"}, \code{"legend"},
#'   or \code{"plot"}.
#'   \item \strong{Column validation} — \code{\link{check_columns}()} validates
#'   \code{truth_by} (single column), \code{score_by} (multiple allowed), and
#'   \code{group_by} (factor, multi-column concatenated). An error is raised if
#'   \code{group_by} is provided alongside multiple \code{score_by} columns.
#'   \item \strong{Positive label encoding} — Converts \code{truth_by} to binary
#'   numeric (0/1) with three paths:
#'   \itemize{
#'     \item \code{pos_label} provided: re-factor with \code{pos_label} as the
#'     last level, then convert.
#'     \item \code{truth_by} is a factor: warn that the last level is treated
#'     as positive, then convert.
#'     \item Non-numeric, non-factor: coerce to factor, warn, then convert.
#'   }
#'   \item \strong{Multi-score_by expansion} — When \code{score_by} contains
#'   multiple columns, \code{tidyr::pivot_longer()} reshapes into a single
#'   \code{.score} column with a \code{.group} identifier, which becomes the
#'   \code{group_by} variable.
#'   \item \strong{ggplot dispatch} — Selects \code{gglogger::ggplot} or
#'   \code{ggplot2::ggplot} based on
#'   \code{getOption("plotthis.gglogger.enabled")}.
#'   \item \strong{Dummy group insertion} — When \code{group_by = NULL}, creates
#'   a synthetic \code{..group} column (constant \code{""}) so the curve still
#'   renders. The legend is suppressed (\code{"none"}).
#'   \item \strong{Auto AUC placement} — When \code{show_auc = "auto"},
#'   single-group or faceted plots place AUC on the plot; multi-group plots
#'   place it in the legend.
#'   \item \strong{Base ROC geometry} — \code{plotROC::geom_roc()} with
#'   \code{aes(d = truth, m = score, color = group)}, controlling direction
#'   via the \code{increasing} parameter.
#'   \item \strong{AUC calculation} — Temporarily facets via
#'   \code{\link{facet_plot}()}, then computes per-group (and per-facet) AUC
#'   values via \code{plotROC::calc_auc()}.
#'   \item \strong{Cutoff computation} — \code{get_cutoffs_data()} combines
#'   \code{group_by} and \code{facet_by} columns into a \code{.cat} identifier
#'   and computes per-category cutoff data, supporting both numeric thresholds
#'   and named \code{OptimalCutpoints} methods.
#'   \item \strong{Cutoff rendering} — When cutoff data is non-NULL, splits the
#'   \code{.cat} column back into group/facet columns and adds
#'   \code{geom_point()} (markers) and \code{geom_text_repel()} (labels) with
#'   configurable size, shape, stroke, colour, and background styling.
#'   \item \strong{Confidence intervals} — When \code{ci} is non-NULL,
#'   \code{plotROC::geom_rocci()} is added with the provided arguments.
#'   \item \strong{AUC display} — Three modes:
#'   \itemize{
#'     \item \code{"plot"}: \code{geom_text()} places AUC labels at a corner
#'     position determined by \code{increasing} and \code{x_axis_reverse}.
#'     \item \code{"legend"}: AUC values are appended to the
#'     \code{scale_color_manual()} labels (with per-facet prefixes when
#'     faceting is active).
#'     \item \code{"none"}: group level names are used as-is.
#'   }
#'   \item \strong{Diagonal reference} — \code{geom_abline()} draws the
#'   no-discrimination line (y = x, or y = -x when x-axis is reversed) as a
#'   dashed grey line.
#'   \item \strong{Color scale} — \code{scale_color_manual()} assigns
#'   palette-derived colours via \code{\link{palette_this}()}, with
#'   AUC-augmented labels when \code{show_auc = "legend"}.
#'   \item \strong{Axis formatting} — Percent labels on y-axis (and x-axis)
#'   when \code{percent = TRUE}. X-axis reversed (1 to 0) when
#'   \code{x_axis_reverse = TRUE}, changing the axis label to
#'   \code{"Specificity"}.
#'   \item \strong{Labels and theme} — \code{labs()} sets title, subtitle, x,
#'   and y labels. The theme, aspect ratio, legend position/direction, and
#'   dashed grid lines are applied.
#'   \item \strong{Dimension calculation} — \code{\link{calculate_plot_dimensions}()}
#'   computes \code{height} and \code{width} attributes from
#'   \code{base_height = 4.5}, aspect ratio, and legend metrics.
#'   \item \strong{Attribute storage} — \code{auc} and \code{cutoffs} data
#'   frames are stored as \code{attr(p, "auc")} and
#'   \code{attr(p, "cutoffs")} for retrieval by the exported wrapper.
#'   \item \strong{Faceting} — \code{\link{facet_plot}()} wraps the plot with
#'   \code{facet_wrap} / \code{facet_grid} if \code{facet_by} is provided.
#' }
#'
#' @inheritParams common_args
#' @param data A data frame with the truth and score columns. See
#'   \url{https://CRAN.R-project.org/package=plotROC}
#'   for the expected format.
#' @param truth_by A character string naming the column that contains the true
#'   class labels (binary outcome, 0/1 or TRUE/FALSE).
#' @param score_by A character vector of column names containing the predicted
#'   scores (classifier output values). When multiple columns are provided, each
#'   column becomes a separate ROC curve grouped by a \code{.group} identifier.
#'   When multiple columns are used, \code{group_by} must be NULL.
#' @param pos_label A character string specifying the positive class label in
#'   \code{truth_by}. When NULL (default), the labels are handled by the
#'   \code{plotROC} package: if \code{truth_by} is a factor, the last level is
#'   used; otherwise it is coerced to a factor with a warning.
#' @param group_by A character vector of column names to group the ROC curve by.
#'   Each unique combination of group values renders a separate ROC curve.
#'   When \code{score_by} contains multiple columns, \code{group_by} must be
#'   NULL because the score columns themselves define the groups. Multiple
#'   \code{group_by} columns are concatenated with \code{group_by_sep}.
#' @param group_by_sep A character string used to separate concatenated
#'   \code{group_by} columns. Default: \code{"_"}.
#' @param group_name A character string to use as the legend title for the
#'   ROC curve groups. When NULL (default), the \code{group_by} column name
#'   is used.
#' @param x_axis_reverse A logical value. If TRUE, the x-axis is reversed (from
#'   1 to 0), displaying specificity instead of 1 - specificity. The x-axis
#'   label automatically changes to \code{"Specificity"}. Default: FALSE.
#' @param percent A logical value. If TRUE, the x and y axes are displayed as
#'   percentages (0 to 100). Default: FALSE.
#' @param ci A list of arguments passed to \code{plotROC::geom_rocci()} to add
#'   confidence intervals to the ROC curve. When NULL (default), no confidence
#'   intervals are drawn. Example: \code{ci = list(sig.level = 0.05)}.
#' @param n_cuts An integer specifying the number of evenly-spaced quantile-based
#'   cutoff points to annotate on the ROC curve. Quantiles are computed from the
#'   \code{score_by} distribution. Default: \code{0} (no quantile cutoffs).
#'   Ignored when \code{cutoffs_at} is non-NULL.
#' @param cutoffs_at A vector of user-supplied cutoff values to annotate as
#'   points on the ROC curve. When non-NULL, overrides \code{n_cuts}. Accepts
#'   raw numeric score thresholds and/or named method strings from the
#'   \code{\link[OptimalCutpoints]{optimal.cutpoints}} package for automatic
#'   optimal cutoff identification. Both \code{cutoffs_at} and
#'   \code{cutoffs.labels} are passed to \code{plotROC::geom_roc()}.
#'   Supported method values are:
#' \itemize{
#'   \item \code{"CB"} (cost-benefit method);
#'   \item \code{"MCT"} (minimises Misclassification Cost Term);
#'   \item \code{"MinValueSp"} (a minimum value set for Specificity);
#'   \item \code{"MinValueSe"} (a minimum value set for Sensitivity);
#'   \item \code{"ValueSe"} (a value set for Sensitivity);
#'   \item \code{"MinValueSpSe"} (a minimum value set for Specificity and Sensitivity);
#'   \item \code{"MaxSp"} (maximises Specificity);
#'   \item \code{"MaxSe"} (maximises Sensitivity);
#'   \item \code{"MaxSpSe"} (maximises Sensitivity and Specificity simultaneously);
#'   \item \code{"MaxProdSpSe"} (maximises the product of Sensitivity and Specificity);
#'   \item \code{"ROC01"} (minimises distance between ROC plot and point (0,1));
#'   \item \code{"SpEqualSe"} (Sensitivity = Specificity);
#'   \item \code{"Youden"} (Youden Index);
#'   \item \code{"MaxEfficiency"} (maximises Efficiency/Accuracy);
#'   \item \code{"Minimax"} (minimises the most frequent error);
#'   \item \code{"MaxDOR"} (maximises Diagnostic Odds Ratio);
#'   \item \code{"MaxKappa"} (maximises Kappa Index);
#'   \item \code{"MinValueNPV"} (a minimum value set for Negative Predictive Value);
#'   \item \code{"MinValuePPV"} (a minimum value set for Positive Predictive Value);
#'   \item \code{"ValueNPV"} (a value set for Negative Predictive Value);
#'   \item \code{"ValuePPV"} (a value set for Positive Predictive Value);
#'   \item \code{"MinValueNPVPPV"} (a minimum value set for Predictive Values);
#'   \item \code{"PROC01"} (minimises distance between PROC plot and point (0,1));
#'   \item \code{"NPVEqualPPV"} (Negative Predictive Value = Positive Predictive Value);
#'   \item \code{"MaxNPVPPV"} (maximises Positive and Negative Predictive Values simultaneously);
#'   \item \code{"MaxSumNPVPPV"} (maximises the sum of the Predictive Values);
#'   \item \code{"MaxProdNPVPPV"} (maximises the product of Predictive Values);
#'   \item \code{"ValueDLR.Negative"} (a value set for Negative Diagnostic Likelihood Ratio);
#'   \item \code{"ValueDLR.Positive"} (a value set for Positive Diagnostic Likelihood Ratio);
#'   \item \code{"MinPvalue"} (minimises p-value of the Chi-squared test);
#'   \item \code{"ObservedPrev"} (closest value to observed prevalence);
#'   \item \code{"MeanPrev"} (closest value to the mean of the test values);
#'   \item \code{"PrevalenceMatching"} (predicted prevalence equals observed prevalence).
#' }
#' @param cutoffs_labels A character vector of user-supplied labels for the
#'   cutoff points. Must be the same length as \code{cutoffs_at}. When NULL,
#'   labels are generated automatically (score value or method name).
#' @param cutoffs_accuracy A numeric value controlling the rounding precision of
#'   automatically generated cutoff labels. Default: \code{0.01}.
#' @param cutoffs_pt_size A numeric value specifying the size of the cutoff
#'   point markers. Default: \code{5}.
#' @param cutoffs_pt_shape A numeric value specifying the shape of the cutoff
#'   point markers. Default: \code{4} (cross).
#' @param cutoffs_pt_stroke A numeric value specifying the stroke width of the
#'   cutoff point markers. Default: \code{1}.
#' @param cutoffs_labal_fg A character string specifying the text colour of
#'   the cutoff labels. Default: \code{"black"}.
#' @param cutoffs_label_size A numeric value specifying the font size of the
#'   cutoff labels. Default: \code{4}.
#' @param cutoffs_label_bg A character string specifying the background colour
#'   of the cutoff labels. Default: \code{"white"}.
#' @param cutoffs_label_bg_r A numeric value specifying the background radius
#'   of the cutoff labels (passed to \code{ggrepel::geom_text_repel()}).
#'   Default: \code{0.1}.
#' @param show_auc A character string specifying the display mode for AUC values:
#' \itemize{
#'   \item \code{"auto"} (default): Automatically determine the position. When
#'   there is a single group or \code{facet_by} is provided, AUC is placed on
#'   the plot; otherwise AUC is placed in the legend.
#'   \item \code{"none"}: Do not display AUC values.
#'   \item \code{"legend"}: Display AUC values in the legend labels.
#'   \item \code{"plot"}: Display AUC values as text on the plot.
#' }
#' @param auc_accuracy A numeric value controlling the rounding precision of
#'   AUC values in labels. Default: \code{0.01}.
#' @param auc_size A numeric value specifying the font size of AUC labels when
#'   displayed on the plot. Default: \code{4}.
#' @param increasing A logical value. If TRUE (default), higher scores indicate
#'   the positive class; if FALSE, lower scores indicate the positive class.
#'   Controls the direction of comparison in the ROC analysis.
#' @return A \code{ggplot} object with \code{height} and \code{width}
#'   attributes (in inches) attached, plus \code{attr(p, "auc")} and
#'   \code{attr(p, "cutoffs")} data frames.
#' @keywords internal
#' @importFrom dplyr mutate summarise pull
#' @importFrom tidyr unite pivot_longer separate
#' @importFrom ggrepel geom_text_repel
#' @importFrom ggplot2 geom_abline scale_color_manual scale_x_reverse scale_x_continuous scale_y_continuous waiver
#' @importFrom ggplot2 geom_text labs
ROCCurveAtomic <- function(
    data,
    truth_by,
    score_by,
    pos_label = NULL,
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    x_axis_reverse = FALSE,
    percent = FALSE,
    ci = NULL,
    n_cuts = 0,
    cutoffs_at = NULL,
    cutoffs_labels = NULL,
    cutoffs_accuracy = 0.01,
    cutoffs_pt_size = 5,
    cutoffs_pt_shape = 4,
    cutoffs_pt_stroke = 1,
    cutoffs_labal_fg = "black",
    cutoffs_label_size = 4,
    cutoffs_label_bg = "white",
    cutoffs_label_bg_r = 0.1,
    show_auc = c("auto", "none", "legend", "plot"),
    auc_accuracy = 0.01,
    auc_size = 4,
    increasing = TRUE,
    theme = "theme_this",
    theme_args = list(),
    palette = "Spectral",
    palcolor = NULL,
    palreverse = FALSE,
    alpha = 1,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    aspect.ratio = 1,
    legend.position = waiver(),
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = ifelse(x_axis_reverse, "Specificity", "1 - Specificity"),
    ylab = "Sensitivity",
    ...
) {
    show_auc <- match.arg(show_auc)
    truth_by <- check_columns(data, truth_by, allow_multi = FALSE)
    score_by <- check_columns(
        data,
        score_by,
        force_factor = FALSE,
        allow_multi = TRUE
    )
    group_by <- check_columns(
        data,
        group_by,
        force_factor = TRUE,
        allow_multi = TRUE,
        concat_multi = TRUE,
        concat_sep = group_by_sep
    )
    stopifnot(
        "'group_by' should be NULL when mulitple 'score_by' columns are provided." = length(
            score_by
        ) ==
            1 ||
            is.null(group_by)
    )

    if (!is.null(pos_label)) {
        data[[truth_by]] <- factor(
            data[[truth_by]],
            levels = c(setdiff(unique(data[[truth_by]]), pos_label), pos_label)
        )
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
        data <- pivot_longer(
            data,
            cols = score_by,
            names_to = ".group",
            values_to = ".score"
        )
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
        legend.position <- ifelse(
            inherits(legend.position, "waiver"),
            "none",
            "right"
        )
    } else {
        legend.position <- ifelse(
            inherits(legend.position, "waiver"),
            "right",
            legend.position
        )
    }

    # determine where to place the AUC
    if (identical(show_auc, "auto")) {
        if (identical(group_by, "..group") || !is.null(facet_by)) {
            show_auc <- "plot"
        } else {
            show_auc <- "legend"
        }
    }

    p <- ggplot(
        data,
        aes(d = !!sym(truth_by), m = !!sym(score_by), color = !!sym(group_by))
    ) +
        # plotROC::geom_roc() doesn't support different cutoffs for different groups/facets
        plotROC::geom_roc(
            n.cuts = 0,
            cutoffs.at = NULL,
            cutoff.labels = NULL,
            increasing = increasing,
            ...
        )

    # facet the plot to calculate the correct AUCs for each facet
    attr(p, "height") <- attr(p, "width") <- 0
    auc <- plotROC::calc_auc(
        facet_plot(
            p,
            facet_by,
            facet_scales,
            facet_nrow,
            facet_ncol,
            facet_byrow
        )
    )

    cutoffs_df <- get_cutoffs_data(
        data %>%
            unite(".cat", !!!syms(unique(c(group_by, facet_by))), sep = " // "),
        truth_by,
        score_by,
        ".cat",
        cutoffs_at,
        cutoffs_labels,
        cutoffs_accuracy,
        n_cuts,
        increasing
    )

    if (!is.null(cutoffs_df)) {
        cutoffs_df <- cutoffs_df %>%
            separate(".cat", into = unique(c(group_by, facet_by)), sep = " // ")

        p <- p +
            geom_point(
                data = cutoffs_df,
                aes(x = !!sym("x"), y = !!sym("y"), color = !!sym(group_by)),
                inherit.aes = FALSE,
                shape = cutoffs_pt_shape,
                size = cutoffs_pt_size,
                stroke = cutoffs_pt_stroke
            ) +
            geom_text_repel(
                data = cutoffs_df,
                aes(label = !!sym("label"), x = !!sym("x"), y = !!sym("y")),
                inherit.aes = FALSE,
                size = cutoffs_label_size * text_size_scale,
                color = cutoffs_labal_fg,
                nudge_x = 0.01,
                bg.color = cutoffs_label_bg,
                bg.r = cutoffs_label_bg_r,
                min.segment.length = 10
            )
    }
    cutoffs_df$specificity <- 1 - cutoffs_df$x
    cutoffs_df$sensitivity <- cutoffs_df$y

    if (!is.null(ci)) {
        p <- p + do_call(plotROC::geom_rocci, ci)
    }

    if (show_auc == "plot") {
        if (identical(group_by, "..group")) {
            auc_df <- auc
            auc_df$AUC <- paste0(
                "AUC = ",
                scales::number(auc$AUC, accuracy = auc_accuracy)
            )
        } else {
            auc_df <- if (is.null(facet_by)) {
                auc
            } else {
                auc %>% dplyr::group_by(!!sym(facet_by))
            }
            auc_df <- auc_df %>%
                summarise(
                    AUC = paste0(
                        "AUC (",
                        !!sym(group_by),
                        ") = ",
                        scales::number(!!sym("AUC"), accuracy = auc_accuracy),
                        collapse = "\n"
                    ),
                    .groups = "drop"
                )
        }

        auc_pos_x <- ifelse(increasing, ifelse(x_axis_reverse, -1, 1), 0)
        auc_pos_y <- ifelse(increasing, 0, 1)
        auc_pos_hjust <- ifelse(increasing == x_axis_reverse, -0.05, 1.05)
        auc_pos_vjust <- ifelse(
            increasing,
            -0.5 / nlevels(data[[group_by]]),
            1 + .5 / nlevels(data[[group_by]])
        )
        p <- p +
            geom_text(
                data = auc_df,
                x = auc_pos_x,
                y = auc_pos_y,
                size = auc_size * text_size_scale,
                hjust = auc_pos_hjust,
                vjust = auc_pos_vjust,
                aes(label = !!sym("AUC")),
                inherit.aes = FALSE
            )
        labels <- levels(data[[group_by]])
    } else if (show_auc == "legend") {
        if (!is.null(facet_by)) {
            auc_df <- auc[
                order(auc[[facet_by]], auc[[group_by]]),
                ,
                drop = FALSE
            ]
            auc_df$AUC <- paste0(
                "AUC (",
                auc_df[[facet_by]],
                ") = ",
                scales::number(auc$AUC, accuracy = auc_accuracy)
            )
            labels <- auc_df %>%
                dplyr::group_by(!!sym(group_by)) %>%
                summarise(
                    AUC = paste(!!sym("AUC"), collapse = "\n"),
                    .groups = "drop"
                ) %>%
                mutate(AUC = paste0(!!sym(group_by), "\n", !!sym("AUC"))) %>%
                pull("AUC")
        } else {
            labels <- paste0(
                auc[[group_by]],
                " (AUC = ",
                scales::number(auc$AUC, accuracy = auc_accuracy),
                ")"
            )
        }
    } else {
        labels <- levels(data[[group_by]])
    }

    p <- p +
        # diagnal line
        geom_abline(
            intercept = 0,
            slope = ifelse(x_axis_reverse, -1, 1),
            linetype = "dashed",
            color = "grey50"
        ) +
        scale_color_manual(
            name = group_name %||% group_by,
            values = palette_this(
                levels(data[[group_by]]),
                palette = palette,
                palcolor = palcolor,
                alpha = alpha,
                reverse = palreverse
            ),
            labels = labels
        )

    if (isTRUE(percent)) {
        p <- p + scale_y_continuous(labels = scales::percent)
    }

    if (x_axis_reverse) {
        p <- p +
            scale_x_reverse(
                labels = if (isTRUE(percent)) scales::percent else waiver()
            )
    } else if (isTRUE(percent)) {
        p <- p + scale_x_continuous(labels = scales::percent)
    }

    p <- p +
        labs(title = title, subtitle = subtitle, x = xlab, y = ylab) +
        do_call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction,
            panel.grid.major = element_line(colour = "grey80", linetype = 2)
        )

    dims <- calculate_plot_dimensions(
        base_height = 4.5,
        aspect.ratio = aspect.ratio,
        legend.position = legend.position,
        legend.direction = legend.direction,
        legend_n = length(labels),
        legend_nchar = max(nchar(labels))
    )

    attr(p, "auc") <- auc
    attr(p, "cutoffs") <- cutoffs_df
    attr(p, "height") <- dims$height
    attr(p, "width") <- dims$width

    facet_plot(
        p,
        facet_by,
        facet_scales,
        facet_nrow,
        facet_ncol,
        facet_byrow,
        legend.position = legend.position,
        legend.direction = legend.direction
    )
}


#' ROC curve
#'
#' Draws one or more Receiver Operating Characteristic (ROC) curves for
#' evaluating binary classifier performance. The function wraps
#' \code{\link{ROCCurveAtomic}} with \code{split_by} handling, providing the
#' ability to generate separate ROC curves per split level and combine them
#' via \code{\link[patchwork]{wrap_plots}}.
#'
#' Key features:
#' \itemize{
#'   \item \strong{Multiple classifiers} — compare several prediction scores
#'   side-by-side by providing multiple \code{score_by} columns.
#'   \item \strong{AUC display} — area under the curve values shown on the
#'   plot or in the legend, with configurable precision.
#'   \item \strong{Optimal cutoffs} — identify and annotate optimal cutoff
#'   points using any of the 30+ methods from the \code{OptimalCutpoints}
#'   package, or supply custom numeric thresholds.
#'   \item \strong{Confidence intervals} — add ROC confidence bands via
#'   \code{plotROC::geom_rocci()}.
#'   \item \strong{Axis orientation} — reverse x-axis to show specificity or
#'   display axes as percentages.
#'   \item \strong{Splitting and faceting} — split data into sub-plots via
#'   \code{split_by} or facet within a single plot via \code{facet_by}.
#' }
#'
#' @section split_by Workflow:
#' When \code{split_by} is provided, the following pipeline executes:
#' \enumerate{
#'   \item \strong{Validation} — \code{\link{validate_common_args}()} checks
#'   the random seed and \code{facet_by} configuration.
#'   \item \strong{Column resolution} — \code{\link{check_columns}()} resolves
#'   \code{split_by} (force_factor, allow_multi, concat_multi).
#'   \item \strong{Data splitting} — Unused factor levels in \code{split_by}
#'   are dropped via \code{droplevels()}, and the data is split by
#'   \code{split_by} levels (preserving factor level order). If
#'   \code{split_by} is NULL, the data is wrapped in a single-element list
#'   named \code{"..."}.
#'   \item \strong{Per-split resolution} — \code{\link{check_palette}()},
#'   \code{\link{check_palcolor}()}, and \code{\link{check_legend}()} resolve
#'   per-split palette, colour, legend.position, and legend.direction
#'   overrides.
#'   \item \strong{Per-split dispatch} — For each split:
#'   \itemize{
#'     \item Title resolution: if \code{title} is a function, it receives
#'     the split level name; otherwise \code{title \%||\% split_level} is
#'     used.
#'     \item The \code{split_by} column is removed from the per-split data
#'     frame to avoid conflicts with the ROC analysis.
#'     \item \code{\link{ROCCurveAtomic}()} is called with the per-split
#'     palette, palcolor, legend.position, and legend.direction.
#'   }
#'   \item \strong{Combination} — \code{\link{combine_plots}()} assembles the
#'   list of plots via \code{patchwork::wrap_plots}, honouring
#'   \code{nrow}/\code{ncol}/\code{byrow}/\code{design}.
#'   \item \strong{AUC / cutoff collection} — When \code{combine = TRUE}, the
#'   per-split \code{auc} and \code{cutoffs} attributes are collected into
#'   combined data frames with a \code{split_by} column identifying the source
#'   split, and stored as \code{attr(p, "auc")} and
#'   \code{attr(p, "cutoffs")}.
#' }
#'
#' @inheritParams common_args
#' @inheritParams ROCCurveAtomic
#' @param split_by The column(s) to split the data by and produce separate
#'   ROC curve plots for each level. The \code{split_by} column is removed from
#'   the per-split data to avoid interfering with ROC analysis. Multiple columns
#'   are concatenated with \code{split_by_sep}.
#' @param split_by_sep A character string used to separate concatenated
#'   \code{split_by} columns. Default: \code{"_"}.
#' @param seed A numeric seed for reproducibility. Default: \code{8525}.
#'   Passed to \code{\link{validate_common_args}()}.
#' @param combine A logical value. When TRUE (default), the list of per-split
#'   plots is combined into a single \code{patchwork} object with
#'   \code{attr(p, "auc")} and \code{attr(p, "cutoffs")} containing the
#'   aggregated results. When FALSE, returns a named list of individual
#'   \code{ggplot} objects.
#' @param nrow,ncol Integer values specifying the number of rows and columns
#'   in the combined plot layout. Passed to \code{\link[patchwork]{wrap_plots}}.
#' @param byrow A logical value. If TRUE (default), the combined layout is
#'   filled row-wise. Passed to \code{\link[patchwork]{wrap_plots}}.
#' @param axes A character string specifying how axes are treated across the
#'   combined layout. Passed to \code{\link{combine_plots}()}. Options:
#'   \code{"keep"}, \code{"collect"}, \code{"collect_x"}, \code{"collect_y"}.
#' @param axis_titles A character string specifying how axis titles are treated
#'   across the combined layout. Defaults to \code{axes}. Passed to
#'   \code{\link{combine_plots}()}.
#' @param guides A character string specifying how legends are collected across
#'   panels in the combined layout. Passed to \code{\link{combine_plots}()}.
#' @param design A custom layout specification for the combined plot. Passed
#'   to \code{\link{combine_plots}()}. When specified, \code{nrow},
#'   \code{ncol}, and \code{byrow} are ignored.
#' @return A \code{patchwork} object (when \code{combine = TRUE}) with
#'   \code{attr(p, "auc")} and \code{attr(p, "cutoffs")} data frames
#'   containing aggregated AUC values and cutoff information across all
#'   splits. When \code{combine = FALSE}, returns a named list of
#'   \code{ggplot} objects, each with their own \code{attr(p[[i]], "auc")}
#'   and \code{attr(p[[i]], "cutoffs")}.
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
#' # --- Basic ROC curve ---
#' ROCCurve(data, truth_by = "D", score_by = "M1")
#'
#' # --- Will warn about the positive label ---
#' ROCCurve(data, truth_by = "D.str", score_by = "M1")
#'
#' # --- Decreasing direction ---
#' ROCCurve(data, truth_by = "D", score_by = "M1", increasing = FALSE)
#'
#' # --- Multiple ROC curves (multiple classifiers) ---
#' ROCCurve(data, truth_by = "D", score_by = c("M1", "M2"), group_name = "Method")
#'
#' # --- Grouping by a column ---
#' ROCCurve(data, truth_by = "D", score_by = "M1", group_by = "gender", show_auc = "plot")
#'
#' # --- Reverse x-axis and display as percentages ---
#' ROCCurve(data, truth_by = "D", score_by = "M1", x_axis_reverse = TRUE, percent = TRUE)
#'
#' # --- Custom n_cuts and single colour ---
#' ROCCurve(data, truth_by = "D", score_by = "M1", n_cuts = 10, palcolor = "black")
#'
#' # --- Add confidence intervals ---
#' ROCCurve(data, truth_by = "D", score_by = "M1", ci = list(sig.level = .01))
#'
#' # --- Facet by a column ---
#' ROCCurve(data, truth_by = "D", score_by = "M1", facet_by = "gender")
#'
#' # --- Show cutoffs ---
#' ROCCurve(data, truth_by = "D", score_by = "M1", cutoffs_at = c(0, "ROC01", "SpEqualSe"))
#'
#' # --- Split by a column ---
#' p <- ROCCurve(data, truth_by = "D", score_by = "M1", split_by = "gender",
#'    cutoffs_at = c(0.2, "MaxSpSe"))
#' p
#' # Retrieve the AUC values
#' attr(p, "auc")
#' # Retrieve the cutoffs
#' attr(p, "cutoffs")
ROCCurve <- function(
    data,
    truth_by,
    score_by,
    pos_label = NULL,
    split_by = NULL,
    split_by_sep = "_",
    group_by = NULL,
    group_by_sep = "_",
    group_name = NULL,
    x_axis_reverse = FALSE,
    percent = FALSE,
    ci = NULL,
    n_cuts = 0,
    cutoffs_at = NULL,
    cutoffs_labels = NULL,
    cutoffs_accuracy = 0.001,
    cutoffs_pt_size = 5,
    cutoffs_pt_shape = 4,
    cutoffs_pt_stroke = 1,
    cutoffs_labal_fg = "black",
    cutoffs_label_size = 4,
    cutoffs_label_bg = "white",
    cutoffs_label_bg_r = 0.1,
    show_auc = c("auto", "none", "legend", "plot"),
    auc_accuracy = 0.01,
    auc_size = 4,
    theme = "theme_this",
    theme_args = list(),
    palette = "Spectral",
    palcolor = NULL,
    palreverse = FALSE,
    alpha = 1,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    aspect.ratio = 1,
    legend.position = waiver(),
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    xlab = ifelse(x_axis_reverse, "Specificity", "1 - Specificity"),
    ylab = "Sensitivity",
    combine = TRUE,
    nrow = NULL,
    ncol = NULL,
    byrow = TRUE,
    seed = 8525,
    axes = NULL,
    axis_titles = axes,
    guides = NULL,
    design = NULL,
    ...
) {
    validate_common_args(seed, facet_by)
    split_by <- check_columns(
        data,
        split_by,
        force_factor = TRUE,
        allow_multi = TRUE,
        concat_multi = TRUE,
        concat_sep = split_by_sep
    )

    if (!is.null(split_by)) {
        data[[split_by]] <- droplevels(data[[split_by]])
        datas <- split(data, data[[split_by]])
        # keep the order of levels
        datas <- datas[levels(data[[split_by]])]
    } else {
        datas <- list(data)
        split_by <- names(datas) <- "..."
    }

    palette <- check_palette(palette, names(datas))
    palcolor <- check_palcolor(palcolor, names(datas))
    legend.direction <- check_legend(
        legend.direction,
        names(datas),
        "legend.direction"
    )
    legend.position <- check_legend(
        legend.position,
        names(datas),
        "legend.position"
    )

    plots <- lapply(
        names(datas),
        function(nm) {
            default_title <- if (length(datas) == 1 && identical(nm, "...")) {
                NULL
            } else {
                nm
            }
            if (is.function(title)) {
                title <- title(default_title)
            } else {
                title <- title %||% default_title
            }
            if (!is.null(split_by)) {
                datas[[nm]][[split_by]] <- NULL
            }

            ROCCurveAtomic(
                datas[[nm]],
                truth_by,
                score_by,
                pos_label = pos_label,
                group_by = group_by,
                group_by_sep = group_by_sep,
                group_name = group_name,
                x_axis_reverse = x_axis_reverse,
                percent = percent,
                ci = ci,
                n_cuts = n_cuts,
                cutoffs_at = cutoffs_at,
                cutoffs_labels = cutoffs_labels,
                cutoffs_accuracy = cutoffs_accuracy,
                cutoffs_pt_size = cutoffs_pt_size,
                cutoffs_pt_shape = cutoffs_pt_shape,
                cutoffs_pt_stroke = cutoffs_pt_stroke,
                cutoffs_labal_fg = cutoffs_labal_fg,
                cutoffs_label_size = cutoffs_label_size,
                cutoffs_label_bg = cutoffs_label_bg,
                cutoffs_label_bg_r = cutoffs_label_bg_r,
                show_auc = show_auc,
                auc_accuracy = auc_accuracy,
                auc_size = auc_size,
                theme = theme,
                theme_args = theme_args,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                palreverse = palreverse,
                alpha = alpha,
                facet_by = facet_by,
                facet_scales = facet_scales,
                facet_ncol = facet_ncol,
                facet_nrow = facet_nrow,
                facet_byrow = facet_byrow,
                aspect.ratio = aspect.ratio,
                legend.position = legend.position[[nm]],
                legend.direction = legend.direction[[nm]],
                title = title,
                subtitle = subtitle,
                xlab = xlab,
                ylab = ylab,
                ...
            )
        }
    )

    names(plots) <- names(datas)

    p <- combine_plots(
        plots,
        combine = combine,
        split_by = split_by,
        nrow = nrow,
        ncol = ncol,
        byrow = byrow,
        axes = axes,
        axis_titles = axis_titles,
        guides = guides,
        design = design
    )

    if (!combine) {
        return(p)
    } else {
        attr(p, "auc") <- do_call(
            rbind,
            lapply(seq_along(plots), function(i) {
                df <- attr(plots[[i]], "auc")
                if (!is.null(split_by)) {
                    df[[split_by]] <- names(datas)[i]
                }
                df
            })
        )
        attr(p, "cutoffs") <- do_call(
            rbind,
            lapply(seq_along(plots), function(i) {
                df <- attr(plots[[i]], "cutoffs")
                if (!is.null(split_by)) {
                    df[[split_by]] <- names(datas)[i]
                }
                df
            })
        )
        return(p)
    }
}
