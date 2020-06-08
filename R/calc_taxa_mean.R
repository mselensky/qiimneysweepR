#' Calculate mean taxon abundance
#'
#' This function calculates mean OTU or ASV count data based on a defined grouping_variable.
#' @param asv_table An OTU or ASV abundance table.
#' @param metadata Qiime2-formatted metadata.
#' @param grouping_variable A grouping variable that calculates mean abundance for each taxon in the defined group.
#' @keywords mean count
#' @export
#' @examples
#' calc_taxa_mean

calc_taxa_mean <- function(asv_table, metadata, grouping_variable) {
  asv_table <- asv_table %>% # I know this is a bit messy, I will make this more streamlined later
    as.data.frame() %>%
    rownames_to_column(var = "sample-id") %>%
    pivot_longer(cols = 2:ncol(.), names_to = "taxon", values_to = "count") %>%
    left_join(., metadata, by = "sample-id") %>%
    group_by(.[[grouping_variable]], taxon) %>%
    mutate(mean_count = mean(count),
           sd_count = sd(count)) %>% ungroup() %>%
    distinct(`.[[grouping_variable]]`, taxon, .keep_all = TRUE) %>%
    ungroup() %>% group_by(`.[[grouping_variable]]`) %>%
    mutate(mean_count_pct = 100*(mean_count/sum(mean_count)),
           mean_count_pct_sd = sd(mean_count_pct)) %>%
    ungroup() %>%
    select(`sample-id`, all_of(grouping_variable), taxon, mean_count, sd_count, mean_count_pct, mean_count_pct_sd)
  asv_table
} # Calculate mean count, sd count, and mean pct count based on defined grouping variable from metadata
