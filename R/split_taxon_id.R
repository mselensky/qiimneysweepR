#' Split taxonomic ID
#'
#' This function takes an ASV table and returns a longform dataframe of counts with separate ID columns for each level of taxonomic classification.
#' @param asv_table An OTU or ASV abundance table.
#' @keywords split taxonID
#' @export
#' @examples
#' split_taxonID

split_taxonID <- function(asv_table) {
  asv_long <- asv_table %>%
    mutate(taxonomy = str_remove_all(taxon, "D_0__|D_1__|D_2__|D_3__|D_4__|D_5__|D_6__")) %>%
    separate(taxonomy, sep=';',
             c("domain", "phylum", "class", "order", "family", "genus", "species")) %>%
    mutate(phylum = if_else(phylum == "Proteobacteria", class, phylum))
  asv_long
}
