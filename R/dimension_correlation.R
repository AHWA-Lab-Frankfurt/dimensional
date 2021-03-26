#' dimension correlation
#'
#' Calculate the correlation of the dimensions of a graph. The output is a correlation matrix of the layers.
#' @param graph A graph of class tbl_graph
#' @param weighted Logical value if weight of the edges of the dimensions is considered or not. Defaults to unweighted network.

dimension_correlation <- function(graph, weighted = FALSE){

  edges.name <- graph %>%
    get.data.frame("edges") %>%
    select(name) %>%
    deframe() %>%
    unique()

  matlist <- list()
  cor.mat <- c()

  if(weighted == TRUE){

       for(j in 1:length(edges.name)) {
      mat <- decor.graph %>%
        activate(edges) %>%
        filter(name == edges.name[j]) %>%
        as_adjacency_matrix(attr = "weight")

      mat[lower.tri(mat, diag = TRUE)] <- NA
      mat <- mat %>%
        as.vector()%>%
        na.omit()

      matlist[[edges.name[j]]] <- mat

    }
    }


  else{

    for(j in 1:length(edges.name)) {
      mat <- decor.graph %>%
        activate(edges) %>%
        filter(name == edges.name[j]) %>%
        as_adjacency_matrix()

      mat[lower.tri(mat, diag = TRUE)] <- NA
      mat <- mat %>%
        as.vector()%>%
        na.omit()

      matlist[[edges.name[j]]] <- mat

    }
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


  return(dimension.correlation)
}
