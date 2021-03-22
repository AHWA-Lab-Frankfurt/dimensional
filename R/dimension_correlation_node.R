#' dimension correlation
#'
#' Calculate the correlation of the dimensions of a graph based on the neighborhood centrality of the nodes in each layer. The output is a correlation matrix of the layers.
#' @param graph A graph of class tbl_graph
#' @param weighted Logical value if weight of the edges of the dimensions is considered or not. Defaults to unweighted network.

dimension_correlation_node <- function(graph, weighted = FALSE){
  edges.name <- graph %>%
    get.data.frame("edges") %>%
    select(name) %>%
    deframe() %>%
    unique()

  vlist <- list()

   if(weighted == TRUE){



    for(j in 1:length(edges.name)) {

      v <- graph %>%
        activate(edges) %>%
        filter(name == edges.name[j])

      v <- centrality_neighborhood(v, weighted = TRUE)

      vlist[[edges.name[j]]] <- v

    }
    cor.mat <- c()

    for(i in 1:length(vlist)){
      for(j in 1:length(vlist)){
        cor <- cor(vlist[[i]], vlist[[j]])
        cor.mat <- c(cor.mat, cor)
      }
    }

  }
  else{


    for(j in 1:length(edges.name)) {

      v <- graph %>%
        activate(edges) %>%
        filter(name == edges.name[j])

      v <- centrality_neighborhood(v)

      vlist[[edges.name[j]]] <- v

    }
    cor.mat <- c()

    for(i in 1:length(vlist)){
      for(j in 1:length(vlist)){
        cor <- cor(vlist[[i]], vlist[[j]])
        cor.mat <- c(cor.mat, cor)
      }
    }


  }
  dimension.correlation <- matrix(cor.mat, nrow = length(vlist), ncol = length(vlist))
  colnames(dimension.correlation) <- edges.name
  rownames(dimension.correlation) <- edges.name


  return(dimension.correlation)
}
