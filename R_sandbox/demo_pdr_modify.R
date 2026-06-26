library(paindrawings)
library(tidyverse)
library(here)

###########################################################
################   ANATOMY TEMPLASTES   ###################
###########################################################

###########################################################
##### Create a full body outline from anatomy regions #####
###########################################################

# Note that pdr_implode is a summarizing function, it returns
# a valid pdr list-col, but NOT with the same number of 
# elements as the input. Therefor not suited for use as a
# list-col in a tibble
pdr_all_anatomy <- pdr_example_anatomy |> pdr_implode()

# pdr_example_anatomy consists of 46 pdr elements, each with
# just one stroke, conversely, pdr_all_anatomy now consists
# of a single element with 46 strokes.
tibble(pdr_data = pdr_all_anatomy) |>
  pdr_plot_drawing()

# We now need to merge/union all 46 strokes pairwise
tibble(pdr_data = pdr_all_anatomy) |>
  mutate(pdr_merged = pdr_modify(pdr_data, "merge_edges")) |>
  pdr_plot_drawing(pdr_merged)

################################################################
##### Create a body outline of posterior low back and legs #####
################################################################

# Again note that we rely on pdr_implode which means breaking 
# out of the first tibble (using pull)
pdr_anatomy_post_low <- 
  tibble(pdr_data = pdr_example_anatomy) |>
  mutate(anatomy_region = pdr_get_info(pdr_data, ".id")) |>
  filter(str_detect(anatomy_region, "(Mid_back_bottom)|(Back.+(lowerback|leg|calf|foot|thigh|buttock))")) |>
  pull(pdr_data) |>
  pdr_implode()
tibble(pdr_data = pdr_anatomy_post_low) |>
  mutate(pdr_data = pdr_modify(pdr_data, "merge_edges")) |> 
  pdr_plot_drawing(type = "polygon", background_image = here("inst", "extdata", "mird_body_background.png"))


###########################################################
#################   POLYGON MANIPULATION  #################
###########################################################

###########################################################
####### Managing polygon pain drawing with 'bugs'    ######
###########################################################

# Lets use some of the example geometry data. This contains some
# constructed principal examples of 'bugs' ... or rather 'issues'
geometry <- tibble(pdr_data = pdr_example_geometry) |>
  mutate(id = pdr_get_info(pdr_data, ".id"))
pdr_plot_drawing(geometry, include_id = TRUE)

# The pdr_modify() function takes a parameter 'ops', which
# must contain e.g. 'buffer_noarea', 'sanitize', etc. If multiple
# ops are specified, they are performed in a fixed order

# The operation 'drop_noarea' will drop any any strokes which are
# singular points or two point lines. 
geometry |>
  mutate(pdr_data = pdr_modify(pdr_data, ops="drop_noarea")) |>
  pdr_plot_drawing()

# The operation 'buffer_noarea' will buffer any any strokes which 
# are singular points or two point lines. 
geometry |>
  mutate(pdr_data = pdr_modify(pdr_data, ops="buffer_noarea", delta=10)) |>
  pdr_plot_drawing()

# The operation 'sanitize' will remove self-intersections, single
# points and lines, and un-closed polygons .. note than some of 
# the polygons are deleted (becasue they enclose no area)
geometry |>
  mutate(pdr_data = pdr_modify(pdr_data, ops="sanitize")) |>
  pdr_plot_drawing()

# The operation 'reduce_to_chull' will replace all strokes with
# their convex hull polygon.
geometry |>
  mutate(pdr_data = pdr_modify(pdr_data, ops="reduce_to_chull")) |>
  pdr_plot_drawing()

# The operation 'merge_overlaps' will merge (by union) any strokes
# which overlap. If any holes are produced by this union in the
# resulting polygon, they are deleted.
geometry |>
  mutate(pdr_data = pdr_modify(pdr_data, ops="merge_overlaps")) |>
  pdr_plot_drawing()

# The operation 'merge_edges' will merge (by union) any strokes
# which share an edge. If any holes are produced by this union in the
# resulting polygon, they are deleted.
geometry |>
  mutate(pdr_data = pdr_modify(pdr_data, ops="merge_edges")) |>
  pdr_plot_drawing()


# We can combine several of these ops, but for each function
# call they are performed in the above order -- if, for some reason,
# users want to break with this order, they must do so by individual
# function calls in the desired order.
geometry |>
  mutate(pdr_data = pdr_modify(pdr_data, 
    ops=c("buffer_noarea", "merge_overlaps"))) |>
  mutate(pdr_data = pdr_modify(pdr_data, 
    ops=c("reduce_to_chull"))) |>
  pdr_plot_drawing()

###########################################################
#################   REAL WORLD DATA    ####################
###########################################################

###########################################################
####### 9 examples of real world LBP pain drawings   ######
###########################################################

tibble(pdr_data = pdr_example_data) |>
  mutate(id = pdr_get_info(pdr_data, ".id")) |>
  filter(row_number()<10) |>
  pdr_plot_drawing(type="path", background_image = here("inst", "extdata", "mird_body_background.png"))

# Notice that the y coordinates are 'upside-down' so 'flipy'
tibble(pdr_data = pdr_example_data) |>
  mutate(id = pdr_get_info(pdr_data, ".id")) |>
  filter(row_number()<10) |>
  mutate(pdr_data = pdr_modify(pdr_data, "flipy")) |>
  pdr_plot_drawing(type="path", background_image = here("inst", "extdata", "mird_body_background.png"))

# Merge overlaps
tibble(pdr_data = pdr_example_data) |>
  mutate(id = pdr_get_info(pdr_data, ".id")) |>
  filter(row_number()<10) |>
  mutate(pdr_data = pdr_modify(pdr_data, c("flipy", "sanitize"))) |>
  mutate(pdr_data = pdr_modify(pdr_data, "merge_overlaps")) |>
  pdr_plot_drawing(type="path", background_image = here("inst", "extdata", "mird_body_background.png"))

# Calculate some geomtric areas
tibble(pdr_data = pdr_example_data) |>
  mutate(id = pdr_get_info(pdr_data, ".id")) |>
  filter(row_number()<10) |>
  mutate(pdr_data = pdr_modify(pdr_data, c("flipy", "sanitize"))) |>
  mutate(area_with_overlaps = pdr_poly_areas(pdr_data)) |>
  mutate(pdr_data = pdr_modify(pdr_data, "merge_overlaps")) |>
  mutate(area_without_overlaps = pdr_poly_areas(pdr_data)) 

