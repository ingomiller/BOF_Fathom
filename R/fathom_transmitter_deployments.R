#' Build a Fathom Transmitter Deployments table from Animals output
#'
#' @description
#' Converts a Fathom **Animals** tibble (as returned by `fathom_animals()`)
#' into a **Transmitter Deployments** tibble.
#'
#' @param animals_tbl A tibble produced by `fathom_animals()`.
#'
#' @return A tibble with columns:
#' `Station`, `Deployment Start`, `Deployment End`, `Latitude`, `Longitude`, `Device`, `Device Depth`.
#'
#' @examples
#' tx <- fathom_transmitter_deployments(animals_tbl)
#' @export
fathom_transmitter_deployments <- function(animals_tbl) {
  animals_tbl |>
    dplyr::transmute(
      Station           = NA_character_,
      `Deployment Start`= `Tagging Time`,
      `Deployment End`  = NA_character_,
      Latitude          = `Tagging Latitude`,
      Longitude         = `Tagging Longitude`,
      # keep only ID after the second "-" for each token; preserve space-separated multiples
      Device = `Tag ID`,
      `Device Depth`    = NA_character_
    )
}
