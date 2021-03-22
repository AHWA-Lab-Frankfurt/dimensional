#' dimension correlation
#'
#' Calculate the correlation of the dimensions of a graph. The output is a correlation matrix of the layers.
#' @param graph A graph of class tbl_graph
#' @param weighted Logical value if weight of the edges of the dimensions is considered or not. Defaults to unweighted network.

dimension_correlation <- function(graph, weighted = FALSE){
  if(weighted == TRUE){

    edges.name <- graph %>%
      get.data.frame("edges") %>%
      select(name) %>%
      deframe() %>%
      unique()

    matlist <- list()

    for(j in 1:length(edges.name)) {
      mat <- decor %>%
        activate(edges) %>%
        filter(name == edges.name[j]) %>%
        as_adjacency_matrix(attr = "weight") %>%
        as.matrix() %>%
        as.vector()

      matlist[[edges.name[j]]] <- mat
      cor.mat <- c()
    }

    for(i in 1:length(matlist)){
      for(j in 1:length(matlist)){
        cor <- cor(matlist[[i]], matlist[[j]])
        cor.mat <- c(cor.mat, cor)
      }
    }

    dimension.correlation <- matrix(cor.mat, nrow = length(matlist), ncol = length(matlist))
    colnames(dimension.correlation) <- edges.name
    rownames(dimension.correlation) <- edges.name
  }
  else{
    edges.name <- graph %>%
      get.data.frame("edges") %>%
      select(name) %>%
      deframe() %>%
      unique()

    matlist <- list()

    for(j in 1:length(edges.name)) {
      mat <- graph %>%
        activate(edges) %>%
        filter(name == edges.name[j]) %>%
        as_adjacency_matrix() %>%
        as.matrix() %>%
        as.vector()

      matlist[[edges.name[j]]] <- mat
      cor.mat <- c()
    }

    for(i in 1:length(matlist)){
      for(j in 1:length(matlist)){
        cor <- cor(matlist[[i]], matlist[[j]])
        cor.mat <- c(cor.mat, cor)
      }
    }

    dimension.correlation <- matrix(cor.mat, nrow = length(matlist), ncol = length(matlist))
    colnames(dimension.correlation) <- edges.name
    rownames(dimension.correlation) <- edges.name

  }
  return(dimension.correlation)
}
