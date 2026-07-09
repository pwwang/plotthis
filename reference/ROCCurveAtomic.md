# Atomic ROC curve (internal)

Core implementation for drawing a single Receiver Operating
Characteristic (ROC) curve. This is the internal workhorse behind the
exported
[`ROCCurve`](https://pwwang.github.io/plotthis/reference/ROCCurve.md)
function. It takes a **single** data frame (no `split_by` support) and
returns a `ggplot` object.

## Usage

``` r
ROCCurveAtomic(
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
)
```

## Arguments

- data:

  A data frame with the truth and score columns. See
  <https://CRAN.R-project.org/package=plotROC> for the expected format.

- truth_by:

  A character string naming the column that contains the true class
  labels (binary outcome, 0/1 or TRUE/FALSE).

- score_by:

  A character vector of column names containing the predicted scores
  (classifier output values). When multiple columns are provided, each
  column becomes a separate ROC curve grouped by a `.group` identifier.
  When multiple columns are used, `group_by` must be NULL.

- pos_label:

  A character string specifying the positive class label in `truth_by`.
  When NULL (default), the labels are handled by the `plotROC` package:
  if `truth_by` is a factor, the last level is used; otherwise it is
  coerced to a factor with a warning.

- group_by:

  A character vector of column names to group the ROC curve by. Each
  unique combination of group values renders a separate ROC curve. When
  `score_by` contains multiple columns, `group_by` must be NULL because
  the score columns themselves define the groups. Multiple `group_by`
  columns are concatenated with `group_by_sep`.

- group_by_sep:

  A character string used to separate concatenated `group_by` columns.
  Default: `"_"`.

- group_name:

  A character string to use as the legend title for the ROC curve
  groups. When NULL (default), the `group_by` column name is used.

- x_axis_reverse:

  A logical value. If TRUE, the x-axis is reversed (from 1 to 0),
  displaying specificity instead of 1 - specificity. The x-axis label
  automatically changes to `"Specificity"`. Default: FALSE.

- percent:

  A logical value. If TRUE, the x and y axes are displayed as
  percentages (0 to 100). Default: FALSE.

- ci:

  A list of arguments passed to
  [`plotROC::geom_rocci()`](https://sachsmc.github.io/plotROC/reference/geom_rocci.html)
  to add confidence intervals to the ROC curve. When NULL (default), no
  confidence intervals are drawn. Example:
  `ci = list(sig.level = 0.05)`.

- n_cuts:

  An integer specifying the number of evenly-spaced quantile-based
  cutoff points to annotate on the ROC curve. Quantiles are computed
  from the `score_by` distribution. Default: `0` (no quantile cutoffs).
  Ignored when `cutoffs_at` is non-NULL.

- cutoffs_at:

  A vector of user-supplied cutoff values to annotate as points on the
  ROC curve. When non-NULL, overrides `n_cuts`. Accepts raw numeric
  score thresholds and/or named method strings from the
  [`optimal.cutpoints`](https://rdrr.io/pkg/OptimalCutpoints/man/optimal.cutpoints.html)
  package for automatic optimal cutoff identification. Both `cutoffs_at`
  and `cutoffs.labels` are passed to
  [`plotROC::geom_roc()`](https://sachsmc.github.io/plotROC/reference/geom_roc.html).
  Supported method values are:

  - `"CB"` (cost-benefit method);

  - `"MCT"` (minimises Misclassification Cost Term);

  - `"MinValueSp"` (a minimum value set for Specificity);

  - `"MinValueSe"` (a minimum value set for Sensitivity);

  - `"ValueSe"` (a value set for Sensitivity);

  - `"MinValueSpSe"` (a minimum value set for Specificity and
    Sensitivity);

  - `"MaxSp"` (maximises Specificity);

  - `"MaxSe"` (maximises Sensitivity);

  - `"MaxSpSe"` (maximises Sensitivity and Specificity simultaneously);

  - `"MaxProdSpSe"` (maximises the product of Sensitivity and
    Specificity);

  - `"ROC01"` (minimises distance between ROC plot and point (0,1));

  - `"SpEqualSe"` (Sensitivity = Specificity);

  - `"Youden"` (Youden Index);

  - `"MaxEfficiency"` (maximises Efficiency/Accuracy);

  - `"Minimax"` (minimises the most frequent error);

  - `"MaxDOR"` (maximises Diagnostic Odds Ratio);

  - `"MaxKappa"` (maximises Kappa Index);

  - `"MinValueNPV"` (a minimum value set for Negative Predictive Value);

  - `"MinValuePPV"` (a minimum value set for Positive Predictive Value);

  - `"ValueNPV"` (a value set for Negative Predictive Value);

  - `"ValuePPV"` (a value set for Positive Predictive Value);

  - `"MinValueNPVPPV"` (a minimum value set for Predictive Values);

  - `"PROC01"` (minimises distance between PROC plot and point (0,1));

  - `"NPVEqualPPV"` (Negative Predictive Value = Positive Predictive
    Value);

  - `"MaxNPVPPV"` (maximises Positive and Negative Predictive Values
    simultaneously);

  - `"MaxSumNPVPPV"` (maximises the sum of the Predictive Values);

  - `"MaxProdNPVPPV"` (maximises the product of Predictive Values);

  - `"ValueDLR.Negative"` (a value set for Negative Diagnostic
    Likelihood Ratio);

  - `"ValueDLR.Positive"` (a value set for Positive Diagnostic
    Likelihood Ratio);

  - `"MinPvalue"` (minimises p-value of the Chi-squared test);

  - `"ObservedPrev"` (closest value to observed prevalence);

  - `"MeanPrev"` (closest value to the mean of the test values);

  - `"PrevalenceMatching"` (predicted prevalence equals observed
    prevalence).

- cutoffs_labels:

  A character vector of user-supplied labels for the cutoff points. Must
  be the same length as `cutoffs_at`. When NULL, labels are generated
  automatically (score value or method name).

- cutoffs_accuracy:

  A numeric value controlling the rounding precision of automatically
  generated cutoff labels. Default: `0.01`.

- cutoffs_pt_size:

  A numeric value specifying the size of the cutoff point markers.
  Default: `5`.

- cutoffs_pt_shape:

  A numeric value specifying the shape of the cutoff point markers.
  Default: `4` (cross).

- cutoffs_pt_stroke:

  A numeric value specifying the stroke width of the cutoff point
  markers. Default: `1`.

- cutoffs_labal_fg:

  A character string specifying the text colour of the cutoff labels.
  Default: `"black"`.

- cutoffs_label_size:

  A numeric value specifying the font size of the cutoff labels.
  Default: `4`.

- cutoffs_label_bg:

  A character string specifying the background colour of the cutoff
  labels. Default: `"white"`.

- cutoffs_label_bg_r:

  A numeric value specifying the background radius of the cutoff labels
  (passed to
  [`ggrepel::geom_text_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)).
  Default: `0.1`.

- show_auc:

  A character string specifying the display mode for AUC values:

  - `"auto"` (default): Automatically determine the position. When there
    is a single group or `facet_by` is provided, AUC is placed on the
    plot; otherwise AUC is placed in the legend.

  - `"none"`: Do not display AUC values.

  - `"legend"`: Display AUC values in the legend labels.

  - `"plot"`: Display AUC values as text on the plot.

- auc_accuracy:

  A numeric value controlling the rounding precision of AUC values in
  labels. Default: `0.01`.

- auc_size:

  A numeric value specifying the font size of AUC labels when displayed
  on the plot. Default: `4`.

- increasing:

  A logical value. If TRUE (default), higher scores indicate the
  positive class; if FALSE, lower scores indicate the positive class.
  Controls the direction of comparison in the ROC analysis.

- theme:

  A character string or a theme class (i.e. ggplot2::theme_classic)
  specifying the theme to use. Default is "theme_this".

- theme_args:

  A list of arguments to pass to the theme function.

- palette:

  A character string specifying the palette to use. A named list or
  vector can be used to specify the palettes for different `split_by`
  values.

- palcolor:

  A character string specifying the color to use in the palette. A named
  list can be used to specify the colors for different `split_by`
  values. If some values are missing, the values from the palette will
  be used (palcolor will be NULL for those values).

- palreverse:

  A logical value indicating whether to reverse the palette. Default is
  FALSE.

- alpha:

  A numeric value specifying the transparency of the plot.

- facet_by:

  A character string specifying the column name of the data frame to
  facet the plot. Otherwise, the data will be split by `split_by` and
  generate multiple plots and combine them into one using
  [`patchwork::wrap_plots`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)

- facet_scales:

  Whether to scale the axes of facets. Default is "fixed" Other options
  are "free", "free_x", "free_y". See
  [`ggplot2::facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)

- facet_ncol:

  A numeric value specifying the number of columns in the facet. When
  facet_by is a single column and facet_wrap is used.

- facet_nrow:

  A numeric value specifying the number of rows in the facet. When
  facet_by is a single column and facet_wrap is used.

- facet_byrow:

  A logical value indicating whether to fill the plots by row. Default
  is TRUE.

- aspect.ratio:

  A numeric value specifying the aspect ratio of the plot.

- legend.position:

  A character string specifying the position of the legend. if
  `waiver()`, for single groups, the legend will be "none", otherwise
  "right".

- legend.direction:

  A character string specifying the direction of the legend.

- title:

  A character string specifying the title of the plot. A function can be
  used to generate the title based on the default title. This is useful
  when split_by is used and the title needs to be dynamic.

- subtitle:

  A character string specifying the subtitle of the plot.

- xlab:

  A character string specifying the x-axis label.

- ylab:

  A character string specifying the y-axis label.

- ...:

  Additional arguments.

## Value

A `ggplot` object with `height` and `width` attributes (in inches)
attached, plus `attr(p, "auc")` and `attr(p, "cutoffs")` data frames.

## Details

The function produces an ROC curve using
[`plotROC::geom_roc()`](https://sachsmc.github.io/plotROC/reference/geom_roc.html),
with the following capabilities:

- **Multiple classifiers** — `score_by` accepts multiple column names,
  automatically pivoting them into a grouped format so several
  prediction scores can be compared on a single plot.

- **AUC calculation** — area under the curve is computed via
  [`plotROC::calc_auc()`](https://sachsmc.github.io/plotROC/reference/calc_auc.html)
  and displayed either on the plot or in the legend, controlled by
  `show_auc`.

- **Cutoff annotation** — user-specified cutoffs (numeric score
  thresholds or named optimal-cutoff methods from the `OptimalCutpoints`
  package) are rendered as markers with labels, using
  [`ggrepel::geom_text_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)
  for label placement.

- **Confidence intervals** — optional ROC confidence bands via
  [`plotROC::geom_rocci()`](https://sachsmc.github.io/plotROC/reference/geom_rocci.html).

- **Axis flexibility** — supports reversed x-axis (displaying
  specificity) and percent-scaled axes.

## Architecture

1.  **show_auc resolution** —
    [`match.arg()`](https://rdrr.io/r/base/match.arg.html) resolves
    `show_auc` to one of `"auto"`, `"none"`, `"legend"`, or `"plot"`.

2.  **Column validation** —
    [`check_columns()`](https://pwwang.github.io/plotthis/reference/check_columns.md)
    validates `truth_by` (single column), `score_by` (multiple allowed),
    and `group_by` (factor, multi-column concatenated). An error is
    raised if `group_by` is provided alongside multiple `score_by`
    columns.

3.  **Positive label encoding** — Converts `truth_by` to binary numeric
    (0/1) with three paths:

    - `pos_label` provided: re-factor with `pos_label` as the last
      level, then convert.

    - `truth_by` is a factor: warn that the last level is treated as
      positive, then convert.

    - Non-numeric, non-factor: coerce to factor, warn, then convert.

4.  **Multi-score_by expansion** — When `score_by` contains multiple
    columns,
    [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
    reshapes into a single `.score` column with a `.group` identifier,
    which becomes the `group_by` variable.

5.  **ggplot dispatch** — Selects `gglogger::ggplot` or
    [`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
    based on `getOption("plotthis.gglogger.enabled")`.

6.  **Dummy group insertion** — When `group_by = NULL`, creates a
    synthetic `..group` column (constant `""`) so the curve still
    renders. The legend is suppressed (`"none"`).

7.  **Auto AUC placement** — When `show_auc = "auto"`, single-group or
    faceted plots place AUC on the plot; multi-group plots place it in
    the legend.

8.  **Base ROC geometry** —
    [`plotROC::geom_roc()`](https://sachsmc.github.io/plotROC/reference/geom_roc.html)
    with `aes(d = truth, m = score, color = group)`, controlling
    direction via the `increasing` parameter.

9.  **AUC calculation** — Temporarily facets via
    [`facet_plot()`](https://pwwang.github.io/plotthis/reference/facet_plot.md),
    then computes per-group (and per-facet) AUC values via
    [`plotROC::calc_auc()`](https://sachsmc.github.io/plotROC/reference/calc_auc.html).

10. **Cutoff computation** —
    [`get_cutoffs_data()`](https://pwwang.github.io/plotthis/reference/get_cutoffs_data.md)
    combines `group_by` and `facet_by` columns into a `.cat` identifier
    and computes per-category cutoff data, supporting both numeric
    thresholds and named `OptimalCutpoints` methods.

11. **Cutoff rendering** — When cutoff data is non-NULL, splits the
    `.cat` column back into group/facet columns and adds `geom_point()`
    (markers) and `geom_text_repel()` (labels) with configurable size,
    shape, stroke, colour, and background styling.

12. **Confidence intervals** — When `ci` is non-NULL,
    [`plotROC::geom_rocci()`](https://sachsmc.github.io/plotROC/reference/geom_rocci.html)
    is added with the provided arguments.

13. **AUC display** — Three modes:

    - `"plot"`: `geom_text()` places AUC labels at a corner position
      determined by `increasing` and `x_axis_reverse`.

    - `"legend"`: AUC values are appended to the `scale_color_manual()`
      labels (with per-facet prefixes when faceting is active).

    - `"none"`: group level names are used as-is.

14. **Diagonal reference** — `geom_abline()` draws the no-discrimination
    line (y = x, or y = -x when x-axis is reversed) as a dashed grey
    line.

15. **Color scale** — `scale_color_manual()` assigns palette-derived
    colours via
    [`palette_this()`](https://pwwang.github.io/plotthis/reference/palette_this.md),
    with AUC-augmented labels when `show_auc = "legend"`.

16. **Axis formatting** — Percent labels on y-axis (and x-axis) when
    `percent = TRUE`. X-axis reversed (1 to 0) when
    `x_axis_reverse = TRUE`, changing the axis label to `"Specificity"`.

17. **Labels and theme** — `labs()` sets title, subtitle, x, and y
    labels. The theme, aspect ratio, legend position/direction, and
    dashed grid lines are applied.

18. **Dimension calculation** —
    [`calculate_plot_dimensions()`](https://pwwang.github.io/plotthis/reference/calculate_plot_dimensions.md)
    computes `height` and `width` attributes from `base_height = 4.5`,
    aspect ratio, and legend metrics.

19. **Attribute storage** — `auc` and `cutoffs` data frames are stored
    as `attr(p, "auc")` and `attr(p, "cutoffs")` for retrieval by the
    exported wrapper.

20. **Faceting** —
    [`facet_plot()`](https://pwwang.github.io/plotthis/reference/facet_plot.md)
    wraps the plot with `facet_wrap` / `facet_grid` if `facet_by` is
    provided.
