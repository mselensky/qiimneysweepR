#' Create a matrix from long form data
#'
#' This function creates an OTU or ASV matrix from long form data (e.g., output from calc_taxa_mean). Output is usable with matrix-based distance calculations.
#' @param long_data Long-form data to be converted into matrix.
#' @param count_column_name Name of column that contains count data. May be relative or absolute count data.
#' @param grouping_name Name of column to be used as rownames. Should be consistent with grouping_variable used in calc_taxa_mean.
#' @keywords matrix
#' @export
#' @examples
#' make_matrix()

make_matrix <- function(long_data, count_column_name, grouping_name) {
  long_data_filtered <- long_data %>%
    select(all_of(grouping_name), taxon, all_of(count_column_name))
  asv_matrix <- long_data_filtered %>%
    pivot_wider(names_from = "taxon", values_from = all_of(count_column_name)) %>%
    column_to_rownames(var = grouping_name) %>%
    as.matrix()
  asv_matrix
} # Prepares ASV table for diversity analyses
