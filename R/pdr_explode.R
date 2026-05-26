#' Split pain drawings into one row per stroke
#'
#' Expands each pain drawing into multiple rows such that each stroke
#' (as defined by the `s` and `p` list-columns) becomes its own
#' independent pain drawing.
#'
#' All top-level metadata columns are duplicated across the resulting rows.
#'
#' @details
#' For each input row (pain drawing):
#'
#' * Each stroke in the `s` column becomes a new row
#' * The corresponding coordinates in the `p` column are matched via `i`
#' * The stroke index `i` is reset to `1` in both `s` and `p`
#'
#' The output preserves the structure of a valid pain drawing data structure
#' (see [pdr_check_data()]), but with:
#'
#' * A new `id` column: sequential integers (`1:n`)
#' * An `old_id` column: original pain drawing identifier
#'
#' **Handling of missing or empty data**
#'
#' * If `s` or `p` is `NA` or empty, the corresponding output rows will
#'   contain `NA` in those columns
#'
#' @param pd A valid pain drawing data structure (see [pdr_check_data()]).
#'
#' @returns
#' A tibble representing a valid pain drawing data structure where each row
#' corresponds to a single stroke from the input.
#'
#' @section Structure:
#' The returned object:
#'
#' * retains all original top-level columns
#' * replaces `s` and `p` with single-stroke list elements
#' * includes:
#'   * `id`: new unique identifier per stroke
#'   * `old_id`: original drawing identifier
#'
#' @export
#'
#' @examples
#' # Explode strokes into separate rows
#' pdr_exploded <- pdr_explode(pdr_demo_data)
#'
#' # Inspect how many strokes per original drawing
#' pdr_exploded |>
#'   dplyr::count(old_id)
#'
#' # Each row now contains exactly one stroke
#' pdr_exploded$p[[1]]



pdr_explode <- function(pd) {
  if(!pdr_check_data(pd, verbose=FALSE)) {
    pdr_check_data(pd, verbose=TRUE) # for the output..
    stop("`pd`is not valid pain drawing data.")
  }

  # This function takes pain drawing data as input.
  # It explodes/expands each pain drawing (row) into
  # several new pain drawings/row -- one for each stroke
  # It duplictates all other columns for each new row added

  pd |> 
    # Grouped by id should be 1 row at the time
    dplyr::group_by(id) |>
    dplyr::group_modify(\(pdr_row, r_indx) {
      # Retain all discrete info pertaining to paindrawing 
      toplevel_info <- pdr_row |> dplyr::select(-s, -p)

      # Explode the 's' column into list of single row tibbles
      if (identical(NA, pdr_row$s[[1]]) || length(pdr_row$s[[1]])==0) {
        col_s <- NA
      } else {
        col_s <- pdr_row$s[[1]] |> 
          dplyr::mutate(old_i = i, i = as.integer(1)) |>
          dplyr::group_by(old_i) |>
          tidyr::nest(.key="s") |> 
          dplyr::ungroup() |> 
          dplyr::select(-old_i) |>
          dplyr::select(s)
      }
      
      # Explode the 'p' column into list of single row tibbles
      if (identical(NA, pdr_row$p[[1]]) || length(pdr_row$p[[1]])==0) {  
        col_p <- NA
      } else {
        col_p <- pdr_row$p[[1]] |> 
          dplyr::mutate(old_i = i, i = as.integer(1)) |> 
          dplyr::group_by(old_i) |> 
          tidyr::nest(.key="p") |>
          dplyr::ungroup() |>
          dplyr::select(-old_i) |>
          dplyr::select(p) 
      }

      tibble::tibble(
        toplevel_info,
        col_s,
        col_p
      ) 
      
    }) |>
      dplyr::ungroup() |>
      mutate(old_id = id , id = dplyr::row_number()) |> 
      mutate(id = as.character(id))
  
}
