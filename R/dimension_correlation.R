#' dimension correlation
#'
#' Calculate the correlation of the dimensions of a graph. The output is a correlation matrix of the layers.
#' @param graph A graph of class tbl_graph
#' @param pvalue Locial value if pvalue should be computed. If it is then the output is a list with 2 matrices.
#' @import tidygraph
#' @import rlang
#' @import tidyverse
#' @import igraph
#' @export

dimension_correlation <- function(matlist, pvalue = FALSE){

 prep <- function(x) {
   x[lower.tri(x, diag = TRUE)] <- NA
   x  %>%
   as.vector()%>%
   na.omit()
}


  cor.mat <- c()
  pv.mat <- c()
  for(i in 1:length(matlist)){
    for(j in 1:length(matlist)){

       cor <- cor.test(prep(matlist[[i]]), prep(matlist[[j]]))
      cor.mat <- c(cor.mat, cor[[4]])
      pv.mat <- c(pv.mat, cor[[3]])
    }
  }

if(missing(pvalue) | pvalue == FALSE){
  dimension.correlation <- matrix(cor.mat, nrow = length(matlist), ncol = length(matlist))
  colnames(dimension.correlation) <- names(matlist)
  rownames(dimension.correlation) <- names(matlist)


  return(dimension.correlation)

}

  else{
    dimension.correlation <- matrix(cor.mat, nrow = length(matlist), ncol = length(matlist))
    colnames(dimension.correlation) <- names(matlist)
    rownames(dimension.correlation) <- names(matlist)

    dimension.pv <- matrix(pv.mat, nrow = length(matlist), ncol = length(matlist))
    colnames(dimension.pv) <- names(matlist)
    rownames(dimension.pv) <- names(matlist)

    return(list(dimension.correlation, dimension.pv))
  }
}

