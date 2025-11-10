# Compute velocity on grid

The original python code is on
https://github.com/theislab/scvelo/blob/master/scvelo/plotting/velocity_embedding_grid.py

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

  A matrix of dimension n_obs x n_dim specifying the embedding
  coordinates of the cells.

- v_embedding:

  A matrix of dimension n_obs x n_dim specifying the velocity vectors of
  the cells.

- density:

  An optional numeric value specifying the density of the grid points
  along each dimension. Default is 1.

- smooth:

  An optional numeric value specifying the smoothing factor for the
  velocity vectors. Default is 0.5.

- n_neighbors:

  An optional numeric value specifying the number of nearest neighbors
  for each grid point. Default is ceiling(n_obs / 50).

- min_mass:

  An optional numeric value specifying the minimum mass required for a
  grid point to be considered. Default is 1.

- scale:

  An optional numeric value specifying the scaling factor for the
  velocity vectors. Default is 1.

- adjust_for_stream:

  A logical value indicating whether to adjust the velocity vectors for
  streamlines. Default is FALSE.

- cutoff_perc:

  An optional numeric value specifying the percentile cutoff for
  removing low-density grid points. Default is 5.
