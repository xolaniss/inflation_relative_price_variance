#' Highlight text for tracked revisions
#'
#' Wraps text in a highlight. In PDF (LaTeX) output this uses the soul package,
#' and in other outputs such as Word it uses a Pandoc `mark` span, which Pandoc
#' renders as a yellow highlight. Highlighting is controlled globally by the
#' option `hl.show`, so all highlights can be switched off in one place, for
#' example `options(hl.show = FALSE)` in the setup chunk.
#'
#' @param text Character string to highlight. It should not contain square brackets.
#' @param show Logical. Defaults to the global option `hl.show` (TRUE if unset).
#' @param color LaTeX colour specification passed to `\sethlcolor` (PDF output only).
#'
#' @return The highlighted string, or `text` unchanged when `show` is FALSE.
hl <- function(text, show = getOption("hl.show", TRUE), color = "red!20") {
  if (!isTRUE(show)) {
    return(text)
  }
  if (knitr::is_latex_output()) {
    paste0("\\sethlcolor{", color, "}\\hl{", text, "}")
  } else {
    paste0("[", text, "]{.mark}")
  }
}
