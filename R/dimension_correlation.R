#' dimension correlation
#'
#' Calculate the correlation of the dimensions of a graph. The output is a correlation matrix of the layers.
#' @param graph A graph of class tbl_graph
dimension_correlation <- function(matlist){

  cor.mat <- c()
  for(i in 1:length(matlist)){
    for(j in 1:length(matlist)){
      cor <- cor(matlist[[i]], matlist[[j]])
      cor.mat <- c(cor.mat, cor)
    }
  }
dimension.correlation <- matrix(cor.mat, nrow = length(matlist), ncol = length(matlist))
colnames(dimension.correlation) <- names(matlist)
rownames(dimension.correlation) <- names(matlist)


return(dimension.correlation)
}
