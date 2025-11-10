# Atomic ROC curve

Atomic ROC curve

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

  A data frame with the truth and score columns. See also
  https://cran.r-project.org/web/packages/plotROC/vignettes/examples.html.

- truth_by:

  A character string of the column name that contains the true class
  labels. (a.k.a. the binary outcome, 1/0 or TRUE/FALSE.)

- score_by:

  character strings of the column names that contains the predicted
  scores. When multiple columns are provided, the ROC curve is plotted
  for each column.

- pos_label:

  A character string of the positive class label. When NULL, the labels
  will be handled by the `plotROC` package.

- group_by:

  A character vector of column names to group the ROC curve by. When
  `score_by` contains multiple columns, `group_by` should be NULL.

- group_by_sep:

  A character string to separate the columns in `group_by`.

- group_name:

  A character string to name the legend of the ROC curve groups.

- x_axis_reverse:

  A logical to reverse the x-axis, that is from 1 to 0.

- percent:

  A logical to display the x and y axis as percentages.

- ci:

  A list of arguments to pass to
  [`plotROC::geom_rocci()`](https://sachsmc.github.io/plotROC/reference/geom_rocci.html)
  to add confidence intervals. When NULL, no confidence intervals are
  added.

- n_cuts:

  An integer to specify the number of cutpoints on the ROC curve. It
  will be the quantiles of the predicted scores.

- cutoffs_at:

  Vector of user supplied cutoffs to plot as points. If non-NULL, it
  will override the values of n_cuts and plot the observed cutoffs
  closest to the user-supplied ones. Both `cutoffs_at` and
  `cutoffs.labels` will be passed to
  [`plotROC::geom_roc()`](https://sachsmc.github.io/plotROC/reference/geom_roc.html).
  Other than numeric values, the following special values are allowed.
  These values are the methods of
  [`OptimalCutpoints::optimal.cutpoints()`](https://rdrr.io/pkg/OptimalCutpoints/man/optimal.cutpoints.html),
  they are literally:

  - "CB" (cost-benefit method);

  - "MCT" (minimizes Misclassification Cost Term);

  - "MinValueSp" (a minimum value set for Specificity);

  - "MinValueSe" (a minimum value set for Sensitivity);

  - "ValueSe" (a value set for Sensitivity);

  - "MinValueSpSe" (a minimum value set for Specificity and
    Sensitivity);

  - "MaxSp" (maximizes Specificity);

  - "MaxSe" (maximizes Sensitivity);

  - "MaxSpSe" (maximizes Sensitivity and Specificity simultaneously);

  - "MaxProdSpSe" (maximizes the product of Sensitivity and Specificity
    or Accuracy Area);

  - "ROC01" (minimizes distance between ROC plot and point (0,1));

  - "SpEqualSe" (Sensitivity = Specificity);

  - "Youden" (Youden Index);

  - "MaxEfficiency" (maximizes Efficiency or Accuracy, similar to
    minimize Error Rate);

  - "Minimax" (minimizes the most frequent error);

  - "MaxDOR" (maximizes Diagnostic Odds Ratio);

  - "MaxKappa" (maximizes Kappa Index);

  - "MinValueNPV" (a minimum value set for Negative Predictive Value);

  - "MinValuePPV" (a minimum value set for Positive Predictive Value);

  - "ValueNPV" (a value set for Negative Predictive Value);

  - "ValuePPV" (a value set for Positive Predictive Value);

  - "MinValueNPVPPV" (a minimum value set for Predictive Values);

  - "PROC01" (minimizes distance between PROC plot and point (0,1));

  - "NPVEqualPPV" (Negative Predictive Value = Positive Predictive
    Value);

  - "MaxNPVPPV" (maximizes Positive Predictive Value and Negative
    Predictive Value simultaneously);

  - "MaxSumNPVPPV" (maximizes the sum of the Predictive Values);

  - "MaxProdNPVPPV" (maximizes the product of Predictive Values);

  - "ValueDLR.Negative" (a value set for Negative Diagnostic Likelihood
    Ratio);

  - "ValueDLR.Positive" (a value set for Positive Diagnostic Likelihood
    Ratio);

  - "MinPvalue" (minimizes p-value associated with the statistical
    Chi-squared test which measures the association between the marker
    and the binary result obtained on using the cutpoint);

  - "ObservedPrev" (The closest value to observed prevalence);

  - "MeanPrev" (The closest value to the mean of the diagnostic test
    values);

  - "PrevalenceMatching" (The value for which predicted prevalence is
    practically equal to observed prevalence).

- cutoffs_labels:

  vector of user-supplied labels for the cutoffs. Must be a character
  vector of the same length as cutoffs_at.

- cutoffs_accuracy:

  A numeric to specify the accuracy of the cutoff values to show.

- cutoffs_pt_size:

  A numeric to specify the size of the cutoff points.

- cutoffs_pt_shape:

  A numeric to specify the shape of the cutoff points.

- cutoffs_pt_stroke:

  A numeric to specify the stroke of the cutoff points.

- cutoffs_labal_fg:

  A character string to specify the color of the cutoff labels.

- cutoffs_label_size:

  A numeric to specify the size of the cutoff labels.

- cutoffs_label_bg:

  A character string to specify the background color of the cutoff
  labels.

- cutoffs_label_bg_r:

  A numeric to specify the radius of the background of the cutoff
  labels.

- show_auc:

  A character string to specify the position of the AUC values.

  - "auto" (default): Automatically determine the position based on the
    plot. When there is a single group or 'facet_by' is provided, the
    AUC is placed on the plot. Otherwise, the AUC is placed in the
    legend.

  - "none": Do not display the AUC values.

  - "legend": Display the AUC values in the legend.

  - "plot": Display the AUC values on the plot (left/right bottom
    corner).

- auc_accuracy:

  A numeric to specify the accuracy of the AUC values.

- auc_size:

  A numeric to specify the size of the AUC values when they are
  displayed on the plot.

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

A ggplot object.
