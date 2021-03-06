#' Import qiime2 artifact as ASV table dataframe
#'
#' This function uses qiime2R to create an OTU or ASV table as a dataframe.
#' @param file_path Path to a qiime2 .qza file.
#' @keywords asv table import
#' @export
#' @examples
#' import_asv_table()

import_asv_table <- function(file_path) {
  asv_table <- qiime2R::read_qza(file_path)
  asv <- asv_table$data %>%
    t() %>%
    as.data.frame()
  asv
}
