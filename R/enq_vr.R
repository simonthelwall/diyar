#' @title Enq_vr - helper function
#'
#' @description
#'
#'
#' @param x Dataframe.
#' @param vr
#'
#' @return
#'
#' @examples
#'
#'
#' library(dplyr)

  enq_vr <- function(x, vr){
    x <- names(dplyr::select(x, !!vr))

    if(length(x)==0){
      x <- NULL
    }else{
      x
    }
    return(x)
  }

