## code to prepare `DATASET` dataset goes here
require(dplyr)
require(devtools)
require(jsonlite)

pd_dd_geom2 <- jsonlite::fromJSON("data-raw/two_geoms.json")
pd_dd_geom3 <- jsonlite::fromJSON("data-raw/four_geoms.json")

# pd_dd_geom1 <- list(
#   "Amy" = list(
#     "a" = list(
#       "coor" = data.frame(
#         x = c(1),
#         y = c(1)
#       ),
#     "info" = list(
#       "name" = "Amy Ding",
#       "age" = 64
#     )
#     ),
#     "b" = list(
#       "coor" = data.frame(
#         x = c(0,2),
#         y = c(2,0)
#       )
#     ),
#     "c" = list(
#       "coor" = data.frame(
#         x = c(1,1.5,3,3),
#         y = c(1,5,5,1)
#       )
#     ),
#     "d" = list(
#       "coor" = data.frame(
#         x = c(0,2,2,1,1,2,2,0),
#         y = c(2,2,3,3,4,4,5,5)
#       )
#     ),
#     "e" = list(
#       "coor" = data.frame(
#         x = c(10),
#         y = c(10)
#       )
#     ),
#     "f" = list(
#       "coor" = data.frame(
#         x = c(10,12),
#         y = c(12,10)
#       )
#     )
#   ),
#   "Bob" = list(
#     "a" = list(
#       "coor" = data.frame(
#         x = c(15),
#         y = c(12)
#       )
#     ),
#     "b" = list(
#       "coor" = data.frame(
#         x = c(20,22),
#         y = c(22,20)
#       )
#     ),
#     "c" = list(
#       "coor" = data.frame(
#         x = c(21,21.5,23,23),
#         y = c(21,25,25,21)
#       )
#     )
#   )
# )

pd_dd_geom1 <- list(pd_dd_geom2, pd_dd_geom3)

use_data(pd_dd_geom3, overwrite=TRUE)
use_data(pd_dd_geom2, overwrite=TRUE)
use_data(pd_dd_geom1, overwrite=TRUE)