# Declare global variables to silence R CMD check NOTE
utils::globalVariables(c(
  "arr_delay", "dep_delay", "delay", "origin", "faa", "name",
  "lat", "lon", "n", "mean_delay"
))

#' Visualize mean flight delays by airport (nycflights13)
#'
#' This function computes and visualizes the mean flight delay
#' per airport using data from the **nycflights13** package.
#'
#' @return A ggplot object showing mean delays by airport.
#' @import dplyr
#' @importFrom ggplot2 ggplot aes geom_point scale_size_continuous scale_alpha_continuous labs theme_bw
#' @importFrom utils globalVariables
#' @export
#'
#' @examples
#' \dontrun{
#' visualize_airport_delays()
#' }
visualize_airport_delays <- function() {
  # Load data
  flights <- nycflights13::flights
  airports <- nycflights13::airports

  # Compute mean delay
  mean_delay_by_origin <- flights |>
    dplyr::mutate(delay = dplyr::coalesce(arr_delay, dep_delay)) |>
    dplyr::filter(!is.na(delay)) |>
    dplyr::group_by(origin) |>
    dplyr::summarise(mean_delay = mean(delay), n = dplyr::n(), .groups = "drop")

  # Join coordinates
  ap <- airports |>
    dplyr::select(faa, name, lat, lon)

  df <- mean_delay_by_origin |>
    dplyr::left_join(ap, by = c("origin" = "faa")) |>
    dplyr::filter(!is.na(lat), !is.na(lon))

  # Visualization
  ggplot2::ggplot(df, ggplot2::aes(lon, lat, size = n)) +
    ggplot2::geom_point(ggplot2::aes(alpha = mean_delay)) +
    ggplot2::scale_size_continuous(name = "Flights") +
    ggplot2::scale_alpha_continuous(name = "Mean delay (min)") +
    ggplot2::labs(
      title = "Mean flight delays by airport (nycflights13)",
      x = "Longitude", y = "Latitude",
      caption = "Delay = coalesce(arr_delay, dep_delay)"
    ) +
    ggplot2::theme_bw()
}
