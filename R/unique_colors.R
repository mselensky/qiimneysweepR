#' Generate list of unique colors
#'
#' This function generates a color palette. Uses RColorBrewer.
#' @param df An object of class dataframe.
#' @param color_by Attribute to color by.
#' @param brewer.pal_name RColorBrewer palette to use.
#' @keywords color scheme unique
#' @export
#' @examples
#' unique_colors()


unique_colors <- function(df, color_by, brewer.pal_name) {
  color_list <- levels(factor(df[[color_by]])) %>%
    as.data.frame() %>%
    rownames_to_column(var = "row_id")
  colnames(color_list)[2] <- color_by
  color_count = length(unique(df[[color_by]]))
  get_palette <- colorRampPalette(brewer.pal(n = 11, name = brewer.pal_name))
  palette <- get_palette(color_count) %>% # list of colors of length `color_count`
    as.data.frame() %>%
    rename("hex.color" = ".") %>%
    rownames_to_column(var = "row_id")
  palette
  color_by_merged <- left_join(color_list, palette, by = "row_id") %>% # list of phyla with unique colors
    select(-row_id) # don't confuse with this row in nodes object
  color_by_merged
} # generates a dataframe of unique colors matched to `color_by` attribute
