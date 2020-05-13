#' Generate edges for network analysis
#'
#' This function creates a list of network edges from an OTU or ASV abundance table. See nodes()
#' @param abundance_matrix An OTU or ASV abundance table.
#' @param nodes A nodes() object.
#' @param dissimilarity A defined dissimilarity metric; c("spearman", "kendall", "pearson") accepted.
#' @param cutoff A defined asymptotic p-value cutoff for each edge. Rows with values higher than this cutoff are discarded.
#' @keywords network edges
#' @export
#' @examples
#' generate_edges()

generate_edges <- function(abundance_matrix, nodes, dissimilarity, cutoff) {
  # rel_matrix <- asv_data %>%
  #   pivot_wider(names_from = "taxon", values_from = "rel_abun") %>% # convert to table
  #   column_to_rownames(var = "sample") %>%
  #   as.matrix() # make relative abundance table

  dissim_matrix <- rcorr(abundance_matrix, type = dissimilarity) # pairwise dissim. and p-values

  dissim_asv <- dissim_matrix[["r"]] %>% # class data.frame w/ pairwise dissimilarity values
    as.data.frame() %>%
    rownames_to_column(var = "taxon1") %>%
    pivot_longer(cols = 2:ncol(.), names_to = "taxon2", values_to = dissimilarity) %>%
    mutate(test = (taxon1 == taxon2)) %>%
    filter(!test == "TRUE") %>% # remove if ID of taxon1 = taxon2
    select(-test)

  pval_asv <- dissim_matrix[["P"]] %>% # class data.frame w/ pairwise p-values
    as.data.frame() %>%
    rownames_to_column(var = "taxon1") %>%
    pivot_longer(cols = 2:ncol(.), names_to = "taxon2", values_to = "p_value") %>%
    mutate(is_na = is.na(.$p_value)) %>%
    filter(!is_na == "TRUE") %>% # remove if pairwise comparison of same taxon
    select(-is_na)

  dissim_pval <- left_join(dissim_asv, pval_asv, by = c("taxon1", "taxon2")) # join pairwise p and dissim vals

  edges <- dissim_pval %>%
    left_join(., nodes, by = c("taxon1" = "label")) %>%
    rename(from = id) %>%
    left_join(., nodes, by = c("taxon2" = "label")) %>%
    rename(to = id) %>%
    select(from, to, all_of(dissimilarity), p_value)

  if(dissimilarity %in% c("spearman", "kendall")) {
    edges <- filter(edges, abs(edges[dissimilarity]) >= cutoff) %>%
      mutate(is_positive = dissimilarity > 0)
  } else if(dissimilarity == "pearson") {
    edges <- filter(edges, edges[dissimilarity] >= cutoff)
  } else {
    edges
  }
  edges
}