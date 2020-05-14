#' Extract environmental vectors for NMDS plotting
#'
#' This function takes a vegan::envfit() output and extracts the top "n" significant vectors as a dataframe.
#' @param env_output Output object from envfit(),
#' @param top Top n significant vectors to display.
#' @param pvalue Defined p-value cutoff.
#' @keywords nmds vectors
#' @export
#' @examples
#' get_nmds_vectors

get_nmds_vectors <- function(env_output, top, pvalue) {
  vectors <- env_output %>% scores(display = "vectors") %>%
    as.data.frame() %>%
    rownames_to_column(var = "variable")
  pvals <- env_output$vectors$pvals %>%
    as.data.frame() %>%
    rename(., pval = `.`) %>%
    rownames_to_column(var = "variable")
  vectors <- left_join(vectors, pvals, by = "variable") %>%
    rename(NMDS1_vec = "NMDS1", NMDS2_vec = "NMDS2") %>%
    filter(pval < pvalue)
  vectors <- top_n(vectors, -(top))
  vectors
} # extracts NMDS loadings
