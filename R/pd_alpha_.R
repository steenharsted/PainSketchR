#' Calculate mean alpha intensity of drawn pixels in a pain drawing PNG
#'
#' Computes the mean alpha value across all pixels whose alpha falls strictly
#' within `alpha_range`. This captures how intensely a patient drew -- i.e.
#' how dark or opaque the strokes are -- independently of how much area was
#' covered. Use in combination with [pd_alpha_area()] to distinguish between a
#' small intense drawing and a large light drawing.
#'
#' @param pngs A list of numeric arrays, each of dimensions
#'   `[height, width, 4]` representing an RGBA image. The fourth slice
#'   (`rgba[,, 4]`) must contain alpha values in the range \[0, 1\].
#'   Typically the `rgba` list-column produced by [pd_add_rgba()].
#' @param alpha_range A numeric vector of length 2 specifying the lower and
#'   upper bounds of the alpha range (lower is exclusive, upper is inclusive).
#'   Defaults to `c(0, 1)`, i.e. all drawn pixels regardless of intensity.
#'   Use e.g. `c(0.5, 1)` to restrict to heavily drawn pixels only.
#'
#' @returns A numeric vector of length `length(rgbas)`. Each element is the
#'   mean alpha of pixels with alpha in `(alpha_range[1], alpha_range[2]]`
#'   for the corresponding array, or `NA_real_` if no pixels fall in that
#'   range.
#'
#' @seealso [pd_alpha_area()] for the complementary area-based metric.
#'
#' @export
#' @examples
#' pd <- pd_json2pd("data-raw/two_geoms.json") |>
#'   dplyr::mutate(rgba = pd_add_rgba(pick(everything())))
#'
#' # Mean intensity across all drawn pixels
#' pd_alpha_intensity(pd$rgba)
#'
#' # Exclude very faint pixels (alpha <= 0.05)
#' pd_alpha_intensity(pd$rgba, alpha_range = c(0.05, 1))
#'
#' # Use inside mutate()
#' pd |> dplyr::mutate(intensity = pd_alpha_intensity(rgba))
#'
pd_alpha_intensity <- function(rgbas, alpha_range = c(0, 1)) {
  # --- input checks ---
  if (!is.list(rgbas)) {
    stop(
      "`rgbas` must be a list of numeric arrays with dimensions [height, width, 4]."
    )
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

  purrr::map_dbl(rgbas, function(rgba) {
    if (!is.array(rgba) || length(dim(rgba)) != 3 || dim(rgba)[3] < 4) {
      stop(
        "`pngs` must be a list of numeric arrays with dimensions [height, width, 4]."
      )
    }
    a <- as.double(rgba[,, 4])
    drawn <- a[a > alpha_range[1] & a <= alpha_range[2]]
    if (length(drawn) == 0) NA_real_ else mean(drawn)
  })
}


#' Calculate the proportion of canvas covered in a pain drawing PNG
#'
#' Computes the proportion of total pixels whose alpha value falls strictly
#' within `alpha_range`. This captures how much of the canvas was drawn on,
#' independently of stroke intensity. Use in combination with
#' [pd_alpha_intensity()] to distinguish between a small intense drawing and
#' a large light drawing.
#'
#' @param rgbas A list of numeric arrays, each of dimensions
#'   `[height, width, 4]` representing an RGBA image. The fourth slice
#'   (`rgba[,, 4]`) must contain alpha values in the range \[0, 1\].
#'   Typically the `rgba` list-column produced by [pd_add_rgba()].
#' @param alpha_range A numeric vector of length 2 specifying the lower and
#'   upper bounds of the alpha range (lower is exclusive, upper is inclusive).
#'   Defaults to `c(0, 1)`, i.e. all drawn pixels regardless of intensity.
#'   Use e.g. `c(0.5, 1)` to restrict to heavily drawn pixels only.
#'
#' @returns A numeric vector of length `length(rgbas)`. Each element is the
#'   proportion of canvas pixels with alpha in
#'   `(alpha_range[1], alpha_range[2]]` -- i.e. strictly greater than the
#'   lower bound and less than or equal to the upper bound.
#'
#' @seealso [pd_alpha_intensity()] for the complementary intensity-based metric.
#'
#' @export
#' @examples
#' pd <- pd_json2pd("data-raw/two_geoms.json") |>
#'   dplyr::mutate(rgba = pd_add_rgba(pick(everything())))
#'
#' # Proportion of all drawn pixels (any alpha > 0)
#' pd_alpha_area(pd$rgba)
#'
#' # Proportion of heavily drawn pixels only (alpha > 0.5)
#' pd_alpha_area(pd$rgba, alpha_range = c(0.5, 1))
#'
#' # Use inside mutate()
#' pd |> dplyr::mutate(
#'   area         = pd_alpha_area(rgba),
#'   area_over_50 = pd_alpha_area(rgba, alpha_range = c(0.5, 1))
#' )
pd_alpha_area <- function(rgbas, alpha_range = c(0, 1)) {
  # --- input checks ---
  if (!is.list(rgbas)) {
    stop(
      "`rgbas` must be a list of numeric arrays with dimensions [height, width, 4]."
    )
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

  purrr::map_dbl(rgbas, function(rgba) {
    if (!is.array(rgba) || length(dim(rgba)) != 3 || dim(rgba)[3] < 4) {
      stop(
        "`rgbas` must be a list of numeric arrays with dimensions [height, width, 4]."
      )
    }
    a <- as.double(rgba[,, 4])
    sum(a > alpha_range[1] & a <= alpha_range[2]) / length(a)
  })
}
