## code to prepare `DATASET` dataset goes here

pd_demo <- tibble(
  id     = rep(1, 18),
  stroke = c(
    1,
    2, 2,
    3, 3, 3, 3,
    4, 4, 4, 4, 4, 4, 4, 4,
    5,
    6, 6
  ),
  x = c(
     1,
     0, 2,
     1, 1, 3, 3,
     0, 2, 2, 1, 1, 2, 2, 0,
    10,
    10, 12
  ),
  y = c(
     1,
     2, 0,
     1, 5, 5, 1,
     2, 2, 3, 3, 4, 4, 5, 5,
    10,
    12, 10
  )
)

use_data(pd_demo)