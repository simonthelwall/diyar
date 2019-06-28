#' @title fmt - helper function
#'
#' @description
#'
#'
#' @param g Double. Number to format
#'
#' @return
#'
#' @examples
#'
#'
#' library(dplyr)

fmt <- function(g) formatC(g, format="d", big.mark=",")

