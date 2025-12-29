pd_check_df <- function(pd) {
  # Check that pd is a valid pain drawing data structure
  # A list of
  #   - tibble 'drawings' -- must have col 'id'
  #   - tibble 'strokes' -- must have col 'id' + 'i'
  #   - tibble 'points' -- must have col 'id' + 'i' + 'x' + 'y'
}