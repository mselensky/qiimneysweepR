#' Return rare taxa
#'
#' This function filters an ASV or OTU table by a defined relative abundance cutoff such that only counts less than the cutoff are returned.
#' @param asv_table An OTU or ASV abundance table.
#' @param abun.cutoff A defined relative relative abundance cutoff. Only taxa with a relative abundance less than this value (a value from 0 to 1) are returned.
#' @keywords cutoff relative abundance
#' @export
#' @examples
#' rare_taxa

rare_taxa <- function(asv_table, abun.cutoff){
  asv_table_cutoff <- asv_table %>%
    rownames_to_column("sample-id") %>%
    pivot_longer(cols = 2:ncol(.), names_to = "taxon", values_to = "count") %>%
    group_by(`sample-id`, taxon) %>%
    mutate(sum_pct_abun = as.numeric(sum(count))) %>% # sum for each taxa in one cave
    filter(!sum_pct_abun >= abun.cutoff) %>%
    filter(!sum_pct_abun == 0) %>%
    select(-sum_pct_abun)
  asv_table_cutoff
}
