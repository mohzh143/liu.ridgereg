#' Visualize mean flight delays by airport (nycflights13)
#'
#' This function computes and visualizes the mean flight delay
#' per airport using data from the **nycflights13** package.
#'
#' @return A ggplot object showing mean delays by airport.
#' @import dplyr
#' @importFrom ggplot2 ggplot aes geom_point scale_size_continuous 
#' @importFrom ggplot2 scale_alpha_continuous labs theme_bw theme
#' @importFrom ggplot2 element_text
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' \dontrun{
#' visualize_airport_delays()
#' }
visualize_airport_delays <- function() {
  # Check if required packages are installed
  if (!requireNamespace("nycflights13", quietly = TRUE)) {
    stop("Please install the 'nycflights13' package to use this function")
  }
  
  # Load data
  flights <- nycflights13::flights
  airports <- nycflights13::airports
  
  # Input validation
  if (nrow(flights) == 0) {
    stop("Flights dataset is empty")
  }
  
  # Compute mean delay with better delay definition
  mean_delay_by_origin <- flights |>
    dplyr::mutate(
      # Use 0 if both delays are NA, otherwise use available delay
      delay = dplyr::coalesce(.data$arr_delay, .data$dep_delay, 0)
    ) |>
    dplyr::filter(!is.na(.data$delay)) |>
    dplyr::group_by(.data$origin) |>
    dplyr::summarise(
      mean_delay = mean(.data$delay, na.rm = TRUE),
      median_delay = stats::median(.data$delay, na.rm = TRUE),
      n_flights = dplyr::n(),
      .groups = "drop"
    ) |>
    # Filter out airports with very few flights for more reliable estimates
    dplyr::filter(.data$n_flights > 10)
  
  # Prepare airport data with better column selection
  ap <- airports |>
    dplyr::select(
      faa = .data$faa, 
      name = .data$name,
      lat = .data$lat, 
      lon = .data$lon
    ) |>
    dplyr::filter(!is.na(.data$lat), !is.na(.data$lon))
  
  # Join datasets with error handling
  df <- mean_delay_by_origin |>
    dplyr::left_join(ap, by = c("origin" = "faa")) |>
    dplyr::filter(!is.na(.data$lat), !is.na(.data$lon))
  
  # Check if we have data to plot
  if (nrow(df) == 0) {
    warning("No valid airport data found after joining and filtering")
    return(ggplot2::ggplot())  # Return empty plot
  }
  
  # Create visualization with improved styling
  p <- ggplot2::ggplot(
    df, 
    ggplot2::aes(
      x = .data$lon, 
      y = .data$lat, 
      size = .data$n_flights,
      alpha = .data$mean_delay
    )
  ) +
    ggplot2::geom_point(color = "#2E86AB", fill = "#2E86AB", shape = 21, stroke = 0.5) +
    ggplot2::scale_size_continuous(
      name = "Number of flights",
      range = c(3, 12)
    ) +
    ggplot2::scale_alpha_continuous(
      name = "Mean delay (minutes)",
      range = c(0.4, 1)
    ) +
    ggplot2::labs(
      title = "Mean Flight Delays by Airport",
      subtitle = "Data from nycflights13 package (2013)",
      x = "Longitude", 
      y = "Latitude",
      caption = "Delay = coalesce(arr_delay, dep_delay, 0)\nPoints sized by number of flights"
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray50"),
      legend.position = "bottom",
      legend.box = "horizontal",
      plot.caption = ggplot2::element_text(color = "gray40", size = 9)
    )
  
  return(p)
}
