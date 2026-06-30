# Compute velocity on a regular grid from sparse cell embeddings

Computes velocity vectors on a regular grid by averaging cell-level
velocities within a Gaussian-weighted neighborhood of each grid point.
This function is adapted from the scvelo Python implementation at
<https://github.com/theislab/scvelo/blob/master/scvelo/plotting/velocity_embedding_grid.py>.

## Usage

``` r
.compute_velocity_on_grid(
  embedding,
  v_embedding,
  density = NULL,
  smooth = NULL,
  n_neighbors = NULL,
  min_mass = NULL,
  scale = 1,
  adjust_for_stream = FALSE,
  cutoff_perc = NULL
)
```

## Arguments

- embedding:

  A numeric matrix of dimension n_obs x n_dim containing the
  low-dimensional embedding coordinates of each cell.

- v_embedding:

  A numeric matrix of dimension n_obs x n_dim containing the velocity
  vectors for each cell.

- density:

  A numeric value specifying the density of the grid points along each
  dimension. Higher values produce a finer grid. Default is 1.

- smooth:

  A numeric value specifying the standard deviation multiplier for the
  Gaussian kernel used to weight neighboring cells when averaging
  velocities onto grid points. Default is 0.5.

- n_neighbors:

  An integer value specifying the number of nearest neighbors to
  consider for each grid point when computing the weighted average
  velocity. Default is `ceiling(n_obs / 50)`.

- min_mass:

  A numeric value specifying the minimum mass threshold. Grid points
  with total weight below this threshold are filtered out. When
  `adjust_for_stream = TRUE`, this is interpreted on a logarithmic scale
  (`10^(min_mass - 6)`). Default is 1.

- scale:

  A numeric value specifying the scaling factor to apply to the
  resulting grid velocity vectors. Only used when
  `adjust_for_stream = FALSE`. Default is 1.

- adjust_for_stream:

  A logical value indicating whether to adjust the output for streamline
  rendering. When `TRUE`, returns a 2D array format with different
  filtering logic suitable for
  [`metR::geom_streamline`](https://eliocamp.github.io/metR/reference/geom_streamline.html).
  Default is `FALSE`.

- cutoff_perc:

  A numeric value specifying the percentile cutoff for removing
  low-density grid points. Only used when `adjust_for_stream = TRUE`.
  Default is 5.

## Value

A list with components `x_grid` (grid point coordinates) and `v_grid`
(velocity vectors at each grid point). When `adjust_for_stream = TRUE`,
`x_grid` is a 2-row matrix of unique coordinates and `v_grid` is a 3D
array.
