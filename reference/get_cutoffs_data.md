# Prepare cutoff data for ROC curve annotation

Internal helper that computes the sensitivity and specificity values for
a set of user-supplied cutoff points (or computed via OptimalCutpoints
methods) across categories defined by group_by and/or facet_by columns.
Used by
[`ROCCurveAtomic()`](https://pwwang.github.io/plotthis/reference/ROCCurveAtomic.md)
to annotate the ROC curve with cutoff markers and labels.

## Usage

``` r
get_cutoffs_data(
  data,
  truth_by,
  score_by,
  cat_by,
  cutoffs_at = NULL,
  cutoffs_labels = NULL,
  cutoffs_accuracy = 0.001,
  n_cuts = 0,
  increasing = TRUE
)
```

## Arguments

- data:

  A data frame with the truth and score columns.

- truth_by:

  A character string of the column name that contains the true class
  labels (binary, 0/1 or TRUE/FALSE).

- score_by:

  A character string of the column name that contains the predicted
  scores.

- cat_by:

  A character string of the column name to categorise/group the data.
  When specified, cutoffs are calculated separately for each category
  level, enabling per-group or per-facet cutoff annotation.

- cutoffs_at:

  A vector of user-supplied cutoff values to plot as points. When
  non-NULL, overrides `n_cuts`. Supports both raw numeric values and
  method names from
  [`optimal.cutpoints`](https://rdrr.io/pkg/OptimalCutpoints/man/optimal.cutpoints.html).

- cutoffs_labels:

  A character vector of user-supplied labels for the cutoffs. Must be
  the same length as `cutoffs_at`. When NULL, labels are generated
  automatically.

- cutoffs_accuracy:

  A numeric value specifying the rounding precision for automatically
  generated cutoff labels. Default: `0.001`.

- n_cuts:

  An integer specifying the number of evenly-spaced quantile-based
  cutoff points. Ignored when `cutoffs_at` is non-NULL. Default: `0` (no
  quantile cutoffs).

- increasing:

  A logical value. If TRUE (default), higher scores indicate the
  positive class; if FALSE, lower scores indicate the positive class.

## Value

A data frame with columns `cutoff`, `x` (1 - specificity), `y`
(sensitivity), `label`, and `cat_by` (the category name).

## Details

Cutoffs can be specified either as numeric values (raw score thresholds)
or as method names from the `OptimalCutpoints` package for automatic
optimal cutoff identification. When `n_cuts > 0`, `n_cuts` evenly-spaced
quantile values of the score distribution are used as cutoffs.
