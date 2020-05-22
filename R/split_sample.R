#' Split ASV table
#'
#' This function filters an OTU or ASV table based on a defined condition and returns a smaller table.
#' @param asv_table An OTU or ASV table.
#' @param metadata Metadata file.
#' @param metadata_column Column from metadata from which to filter.
#' @param variable Variable from metadata_column for which to filter.
#' @keywords filter split
#' @export
#' @examples
#' split_table()

split_table <- function(asv_table, metadata, metadata_column = ".", variable = ".") {
  asv_table_split <- asv_table %>%
    as.data.frame() %>%
    rownames_to_column(var = "sample-id") %>%
    pivot_longer(cols = 2:ncol(.), names_to = "taxon", values_to = "count") %>%
    left_join(., metadata, by = "sample-id") %>%
    filter(.[[metadata_column]] %in% variable) %>%
    select(-c(4:ncol(.))) %>%
    pivot_wider(names_from = "taxon", values_from = "count") %>%
    column_to_rownames(var = "sample-id")
  asv_table_split
} # Filters ASV table based on condition and produces a new ASV table
