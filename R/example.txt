# Firstly 'load_all()' in console, then try these lines...
# Load some data
pd <- pd_json2df(c("data-raw/two_geoms.json", "data-raw/four_geoms.json"))

# Look at it as plot
pd |> pd_geom_plot()

# Anon2 seems to have invisible strokes (nr 3 and 4) ... inspecting data will show 3 is a point and 4 a two-point line
# Try to clean that up with buffering (default) and delte set to 25
pd |> pd_geom_cleanup(delta=25) |> pd_geom_plot()

# Let's look at the areas of the two pain drawings
# First look at the structure of the pd data (a list of three tibbles)
pd 

# Notice that there are no data in $drawings or $strokes about areas
# Now let's calculate some areas:
pd |> pd_geom_areas() 

# Notice the area of strokes 3 and 4 in id Anon2 and let's try to clean that up again
# also note the combined area of the two drawings in $drawing$drawing_area
pd |> pd_geom_cleanup(delta=25) |> pd_geom_areas()

# Notice that the areas of Anon2 strokes 3 and 4 have changed
# Also note in the plot, that there is some overlap in both drawings
pd |> pd_geom_cleanup(delta=25) |> pd_geom_plot()

# Lets manage that overlap by 'merge'
pd |> pd_geom_cleanup(delta=25) |> pd_geom_manage_overlaps() |> pd_geom_plot()
pd |> pd_geom_cleanup(delta=25) |> pd_geom_manage_overlaps() |> pd_geom_areas()

# Notice that in $strokes there are now som NA values because strokes Anon|2 and Anon2|3 and Anon2|4 no longer exist