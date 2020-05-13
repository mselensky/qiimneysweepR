#' Get NMDS scores
#'
#' This function extracts NMDS scores and merges associated metadata file for ease of ggplot2-based plotting.
#' @param metaMDS_output A metaMDS object.
#' @param metadata Metadata file.
#' @param joining_variable A defined column by which scores and metadata will merge. Should be consistent with grouping_variable argument from calc_taxa_mean().
#' @keywords color scheme unique
#' @export
#' @examples
#' get_nmds_scores()

get_nmds_scores <- function(metaMDS_output, metadata, joining_variable) {
  nmds_scores <- scores(metaMDS_output) %>%
    as.data.frame() %>%
    rownames_to_column(var = joining_variable) %>% as.tbl()
  nmds_scores
}
