#' Information Memo for Director
#'
#' @import xml2
#' @export
information_memo <- function(fig_caption = TRUE, md_extensions = NULL, pandoc_args = NULL,
                             draft = TRUE, ...) {

  ref_docx <- sprintf('%s/rmarkdown/templates/information2director/resources/information2director_template.docx',
                      find.package('rocuments'))

  ret_val <- rocx(reference_docx = ref_docx, ...)
}


