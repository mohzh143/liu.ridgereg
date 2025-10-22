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
#' @importFrom rlang .data
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

  # Compute mean delay without global variables
  mean_delay_by_origin <- flights |>
    dplyr::mutate(delay = dplyr::coalesce(.data$arr_delay, .data$dep_delay)) |>
    dplyr::filter(!is.na(.data$delay)) |>
    dplyr::group_by(.data$origin) |>
    dplyr::summarise(mean_delay = mean(.data$delay),
                     n = dplyr::n(),
                     .groups = "drop")

  # Join coordinates
  ap <- airports |>
    dplyr::select(.data$faa, .data$name, .data$lat, .data$lon)

  df <- mean_delay_by_origin |>
    dplyr::left_join(ap, by = c("origin" = "faa")) |>
    dplyr::filter(!is.na(.data$lat), !is.na(.data$lon))

  # Visualization
  ggplot2::ggplot(df, ggplot2::aes(.data$lon, .data$lat, size = .data$n)) +
    ggplot2::geom_point(ggplot2::aes(alpha = .data$mean_delay)) +
    ggplot2::scale_size_continuous(name = "Flights") +
    ggplot2::scale_alpha_continuous(name = "Mean delay (min)") +
    ggplot2::labs(
      title = "Mean flight delays by airport (nycflights13)",
      x = "Longitude", y = "Latitude",
      caption = "Delay = coalesce(arr_delay, dep_delay)"
    ) +
    ggplot2::theme_bw()
}
