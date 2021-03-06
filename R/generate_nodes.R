#' Generate nodes for network analysis
#'
#' This function generates network nodes for visNetwork from an OTU or ASV table.
#' @param asv_table An OTU or ASV table.
#' @keywords network nodes
#' @export
#' @examples
#' generate_nodes()

generate_nodes <- function(asv_table) {
  asv_data <- asv_table %>%
    rownames_to_column(var = "sample-id") %>%
    pivot_longer(cols = 2:ncol(.), names_to = "taxon", values_to = "count")

   nodes <- unique(asv_data$taxon) %>%
    as_tibble() %>%
    rename("label" = "value") %>%
    rowid_to_column("id")
}
