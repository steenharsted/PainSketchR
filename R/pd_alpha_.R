#' Calculate mean alpha intensity of drawn pixels in a pain drawing PNG
#'
#' Computes the mean alpha value across all pixels with alpha greater than
#' `min_alpha`. This captures how intensely a patient drew -- i.e. how dark
#' or opaque the strokes are -- independently of how much area was covered.
#' Use in combination with [pd_alpha_area()] to distinguish between a small
#' intense drawing and a large light drawing.
#'
#' @param png A numeric array of dimensions `[height, width, 4]` representing
#'   an RGBA image. The fourth slice (`png[,, 4]`) must contain alpha values
#'   in the range \[0, 1\]. Typically the `.png` list-column produced by
#'   [pd_add_png()].
#' @param min_alpha A single numeric value in \[0, 1\) used as a lower
#'   (exclusive) threshold for filtering pixels. Only pixels with alpha
#'   strictly greater than `min_alpha` are included in the mean. Defaults to
#'   `0`, i.e. all drawn pixels. Increase (e.g. `0.05`) to exclude very faint
#'   noise pixels.
#'
#' @returns A single numeric value in (0, 1\] representing the mean alpha of
#'   drawn pixels, or `NA_real_` if no pixels exceed `min_alpha`.
#'
#' @seealso [pd_alpha_area()] for the complementary area-based metric.
#'
#' @export
#' @examples
#' pd <- pd_json2pd("data-raw/two_geoms.json") |> pd_add_png()
#'
#' # Mean intensity across all drawn pixels
#' pd_alpha_intensity(pd$.png[[1]])
#'
#' # Exclude very faint pixels (alpha <= 0.05)
#' pd_alpha_intensity(pd$.png[[1]], min_alpha = 0.05)
#'
#' # Use inside mutate() with map_dbl()
#' pd |> dplyr::mutate(intensity = purrr::map_dbl(.png, pd_alpha_intensity))
#'
pd_alpha_intensity <- function(png, min_alpha = 0) {
  # --- input checks ---
  if (!is.array(png) || length(dim(png)) != 3 || dim(png)[3] < 4) {
    stop("`png` must be a numeric array with dimensions [height, width, 4].")
  }
  if (
    !is.numeric(min_alpha) ||
      length(min_alpha) != 1 ||
      is.na(min_alpha) ||
      min_alpha < 0 ||
      min_alpha >= 1
  ) {
    stop("`min_alpha` must be a single numeric value in [0, 1).")
  }

  a <- as.double(png[,, 4])
  drawn <- a[a > min_alpha]

  if (length(drawn) == 0) {
    return(NA_real_)
  }

  mean(drawn)
}


#' Calculate the proportion of canvas covered in a pain drawing PNG
#'
#' Computes the proportion of total pixels whose alpha value falls strictly
#' within `alpha_range`. This captures how much of the canvas was drawn on,
#' independently of stroke intensity. Use in combination with
#' [pd_alpha_intensity()] to distinguish between a small intense drawing and
#' a large light drawing.
#'
#' @param png A numeric array of dimensions `[height, width, 4]` representing
#'   an RGBA image. The fourth slice (`png[,, 4]`) must contain alpha values
#'   in the range \[0, 1\]. Typically the `.png` list-column produced by
#'   [pd_add_png()].
#' @param alpha_range A numeric vector of length 2 specifying the lower and
#'   upper bounds of the alpha range (lower is exclusive, upper is inclusive). Defaults to `c(0, 1)`,
#'   i.e. all drawn pixels regardless of intensity. Use e.g. `c(0.5, 1)` to
#'   restrict to heavily drawn pixels only.
#'
#' @returns A single numeric value in \[0, 1\] representing the proportion of
#'   canvas pixels with alpha in `(alpha_range[1], alpha_range[2]]` --
#'   i.e. strictly greater than the lower bound and less than or equal to
#'   the upper bound.
#'
#' @seealso [pd_alpha_intensity()] for the complementary intensity-based metric.
#'
#' @export
#' @examples
#' pd <- pd_json2pd("data-raw/two_geoms.json") |> pd_add_png()
#'
#' # Proportion of all drawn pixels (any alpha > 0)
#' pd_alpha_area(pd$.png[[1]])
#'
#' # Proportion of heavily drawn pixels only (alpha > 0.5)
#' pd_alpha_area(pd$.png[[1]], alpha_range = c(0.5, 1))
#'
#' # Use inside mutate() with map_dbl()
#' pd |> dplyr::mutate(
#'   area         = purrr::map_dbl(.png, pd_alpha_area),
#'   area_over_50 = purrr::map_dbl(.png, \(x) pd_alpha_area(x, alpha_range = c(0.5, 1)))
#' )
pd_alpha_area <- function(png, alpha_range = c(0, 1)) {
  # --- input checks ---
  if (!is.array(png) || length(dim(png)) != 3 || dim(png)[3] < 4) {
    stop("`png` must be a numeric array with dimensions [height, width, 4].")
  }
  if (
    !is.numeric(alpha_range) ||
      length(alpha_range) != 2 ||
      any(is.na(alpha_range)) ||
      alpha_range[1] < 0 ||
      alpha_range[2] > 1 ||
      alpha_range[1] >= alpha_range[2]
  ) {
    stop(
      "`alpha_range` must be a numeric vector of length 2 with 0 <= alpha_range[1] < alpha_range[2] <= 1."
    )
  }

  a <- as.double(png[,, 4])
  n_in_range <- sum(a > alpha_range[1] & a <= alpha_range[2])
  n_in_range / length(a)
}
