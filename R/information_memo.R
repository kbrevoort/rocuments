#' Information Memo for the Director
#'
#' Generate an information memo for the Director.
#' @importFrom rocx rocx
#' @export
information_memo <- function(...) {
  reference_docx <- sprintf('%s/rmarkdown/templates/information2director/resources/information2director_template.docx',
                            find.package(package = 'rocuments'))
  rocx::rocx(reference_docx = reference_docx, ...)
}

#' CCP Analysis Memo
#'
#' Generate a memo detailing the results of a CCP analysis.  If the results are to
#' be circulated to the Directory, then use information_memo instead.
#' @importFrom rocx rocx
#' @export
ccp_memo <- function(...) {
  reference_docx <- sprintf('%s/rmarkdown/templates/ccp_memo/resources/ccp_memo_template.docx',
                            find.package(package = 'rocuments'))
  rocx::rocx(reference_docx = reference_docx, ...)
}

