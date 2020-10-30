#' Calculate mean taxon abundance
#'
#' This function calculates mean OTU or ASV count data based on a defined grouping_variable.
#' @param asv_table A rarefied OTU or ASV abundance table.
#' @param metadata Qiime2-formatted metadata.
#' @param grouping_variable A metadata grouping variable from which mean abundance for each taxon is calculated.
#' @param keep.vars A boolean. If TRUE, keep all metadata variables in output. If FALSE, only keep grouping_variable in output.
#' @keywords mean count
#' @export
#' @examples
#' calc_taxa_mean

calc_taxa_mean <- function(asv_table, metadata, grouping_variable, keep.vars) {

  # long-form taxon abundance data with joined metadata
  asv_long <- asv_table %>%
    rownames_to_column(var = "sample-id") %>%
    pivot_longer(cols = 2:ncol(.), names_to = "taxon", values_to = "count") %>%
    left_join(., metadata, by = "sample-id")

  # calculate mean taxon abundance + sd based on grouping_variable (absolute counts)
  asv_long_mean <- asv_long %>%
    group_by(.[[grouping_variable]], taxon) %>%
    mutate(mean_count = mean(count, na.rm = TRUE),
           sd_count = sd(count, na.rm = TRUE)) %>%
    distinct(`.[[grouping_variable]]`, taxon, .keep_all = TRUE) %>%
    ungroup()

  # convert mean_count and sd_count to relative abundance and calculate coefficient of variation (rsd)
  asv_long_mean_rel_abun <- asv_long_mean %>%
    group_by(.[[grouping_variable]]) %>%
    mutate(rel_abun_mean = mean_count/sum(mean_count),
           rel_abun_sd = sd_count/sum(mean_count),
           rsd = rel_abun_sd/rel_abun_mean)

  # keep or remove metadata variables based on keep.vars input
  if(keep.vars == FALSE) {
    asv_long_mean_rel_abun %>%
      ungroup() %>%
      select(all_of(grouping_variable), taxon, mean_count, sd_count, rel_abun_mean, rel_abun_sd, rsd)
  } else if(keep.vars == TRUE) {
    asv_long_mean_rel_abun %>%
      ungroup() %>%
      select(all_of(grouping_variable), taxon, mean_count, sd_count, rel_abun_mean, rel_abun_sd, rsd, everything(), -`.[[grouping_variable]]`)
  } else {
    asv_long_mean_rel_abun
  }

}
