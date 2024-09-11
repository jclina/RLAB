#' @title meansd
#' @description Outputs mean and standard deviation of a vector rounded to
#' the digits argument specified by the user
#' @param x Numeric vector
#' @param digits Number of digits in which to round the mean and standard deviation, Default: 2
#' @return Returns a formatted mean and standard deviation value rounded to digits
#' @details Outputs mean and standard deviation of a vector rounded to
#' the digits argument specified by the user
#' @examples
#' \dontrun{
#' if(interactive()){
#'  mysample <- seq(1, 100, 1)
#'  meansd(x = mysample, digits = 2)
#'  }
#' }
#' @rdname meansd
#' @importFrom stats sd
#' @export

meansd <- function(x, digits = 2){
  fmt <- paste0("%.", digits, "f")
  output <- sprintf(
    paste("Mean and SD:", fmt, "\u00B1", fmt),
    mean(x), stats::sd(x)
    )
  return(output)
}


