#' @title render_presentation
#' @description Copies presentation to an output directory and opens the HTML presentation in web browser
#' @param outdir Directory to download the presentation to on your local computer
#' @param name Name of the presentation or workshop to render
#' @param open_html Open the revealjs in the default web browser, Default: TRUE
#' @param save_as_pdf Save the revealjs as a pdf document, Default: FALSE
#' @param ... Additional parameters passed to quarto::quarto_render
#' @return A rendered Quarto revealjs presentation in the output directory
#'     that is opened on the user's default Internet browser
#' @details Copies presentation to an output directory and opens the HTML presentation in web browser
#' @seealso
#'  \code{\link[quarto]{quarto_render}}
#'  \code{\link[utils]{browseURL}}
#' @rdname render_presentation
#' @export
#' @importFrom quarto quarto_render
#' @importFrom utils browseURL
#' @importFrom pagedown chrome_print

render_presentation <- function(
  outdir,
  name,
  open_html = TRUE,
  save_as_pdf = FALSE,
  ...
) {
  if (!dir.exists(outdir)) {
    message("Output directory does not exist. Creating '", outdir, "' now.")
    dir.create(outdir)
  }

  if (!"qmd" %in% list.files(file.path(outdir))) {
    dir.create(file.path(outdir, "qmd"))
  }

  qmd_folders <- list.files(
    system.file("qmd", package = "RLAB"),
    full.names = TRUE
  )

  invisible(file.copy(
    from = qmd_folders[grepl(name, qmd_folders)],
    to = file.path(outdir, "qmd"),
    recursive = TRUE
  ))

  new_qmd_folder <- file.path(
    outdir,
    "qmd",
    basename(qmd_folders[grepl(name, qmd_folders)])
  )

  style_reveal <- system.file("styles/styleReveal.scss", package = "RLAB")
  if (!"styles" %in% list.files(outdir)) {
    dir.create(file.path(outdir, "styles"))
    invisible(file.copy(from = style_reveal, to = file.path(outdir, "styles")))
  }

  qmd_f <- list.files(new_qmd_folder, pattern = ".qmd$", full.names = TRUE)
  quarto::quarto_render(input = qmd_f, ...)
  html_f <- gsub(".qmd$", ".html", qmd_f)

  if (open_html) utils::browseURL(html_f)

  if (save_as_pdf)
    pagedown::chrome_print(
      input = html_f,
      output = gsub(".html$", ".pdf", html_f)
    )
}
