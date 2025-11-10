# Prepare the cutoff data for the ROC curve

Prepare the cutoff data for the ROC curve

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
  labels.

- score_by:

  A character string of the column name that contains the predicted
  scores.

- cat_by:

  A character string of the column name to categorize/group the data. If
  specified, the cutoffs will be calculated for each category.

- cutoffs_at:

  Vector of user supplied cutoffs to plot as points. If non-NULL, it
  will override the values of n_cuts and plot the observed cutoffs
  closest to the user-supplied ones.

- cutoffs_labels:

  vector of user-supplied labels for the cutoffs. Must be a character
  vector of the same length as cutoffs_at.

- n_cuts:

  An integer to specify the number of cuts on the ROC curve.

- increasing:

  TRUE if the score is increasing with the truth (1), FALSE otherwise.

## Value

A data frame with the cutoffs and the corresponding x and y values.
