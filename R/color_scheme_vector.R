#' Generate color scheme as named vector
#'
#' This function creates a color scheme formatted as a named vector
#' @param color_list A list of colors from unique_colors().
#' @param color_by Attribute to color by. A column from long_count_data, inputted as a string.
#' @param long_count_data Long form data, e.g. an output from get_nmds_scores().
#' @keywords color scheme named vector
#' @export
#' @examples
#' color_scheme_vector

color_scheme_vector <- function(color_list, color_by, long_count_data) {
  color_name <- left_join(long_count_data, color_list)
  color_name <- color_name %>%
    ungroup() %>%
    distinct(.[[color_by]], hex.color)

  color_list_filtered <- as.character(color_name$hex.color)
  names(color_list_filtered) <- color_name$`.[[color_by]]`
  color_list_filtered
} # Generate a ggplot-amenable color scheme as a named vector (from a dataframe). Color hexcodes must be in a column named "hex.color"
