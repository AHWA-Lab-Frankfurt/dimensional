#' dimension correlation
#'
#' Calculate the correlation of the dimensions of a graph based on the neighborhood centrality of the nodes in each layer. The output is a correlation matrix of the layers.
#' @param graph A graph of class tbl_graph
#' @param actors An actor or a vector of actors. Defaults to all actors in the graph.
#' @param dimensions The Dimensions of the edges that should be taken into account. A variable of the edgelist.
#' @param weighted Logical value if weight of the edges of the dimensions is considered or not. Defaults to unweighted network.
#' @param pvalue Locial value if pvalue should be computed. If it is then the output is a list with 2 matrices.
#' @import tidygraph
#' @import rlang
#' @import tidyverse
#' @import igraph
#' @export

dimension_correlation_node <- function(graph, actors, dimensions, weighted = FALSE, pvalue = FALSE){

  if(missing(dimensions)){
    stop("dimension missing without default")
  }
  else{

    dimensions <- rlang::enquo(dimensions)

  if(missing(actors)){

  }
  else{
    a <- actors

    graph <- graph %>%
      tidygraph::activate(nodes) %>%
      dplyr::filter(id %in% a)
  }

   edges.name <- graph %>%
    igraph::get.data.frame("edges") %>%
    dplyr::pull(!!dimensions) %>%
    unique()

  vlist <- list()

   if(weighted == TRUE){



    for(j in 1:length(edges.name)) {

      v <- graph %>%
        tidygraph::activate(edges) %>%
        #filtering the edgelist for the dimension wanted
        dplyr::filter_at(dplyr::vars(!!dimensions), dplyr::all_vars(. == edges.name[j])) %>%
        tidygraph::activate(nodes) %>%
        dplyr::mutate(deg = tidygraph::centrality_degree(weights = weight)) %>%
        dplyr::pull(deg)


      vlist[[edges.name[j]]] <- v

    }

  }
  else{


    for(j in 1:length(edges.name)) {

      v <- graph %>%
        tidygraph::activate(edges) %>%
        #filtering the edgelist for the dimension wanted
        dplyr::filter_at(dplyr::vars(!!dimensions), dplyr::all_vars(. == edges.name[j])) %>%
        tidygraph::activate(nodes) %>%
        dplyr::mutate(deg = tidygraph::centrality_degree()) %>%
        dplyr::pull(deg)


      vlist[[edges.name[j]]] <- v

    }


  }
  cor.mat <- c()
  pv.mat <- c()

  for(i in 1:length(vlist)){
    for(j in 1:length(vlist)){
      cor <- cor.test(vlist[[i]], vlist[[j]])
      cor.mat <- c(cor.mat, cor[[4]])
      pv.mat <- c(pv.mat, cor[[3]])
    }
  }

  dimension.correlation <- matrix(cor.mat, nrow = length(vlist), ncol = length(vlist))
  colnames(dimension.correlation) <- edges.name
  rownames(dimension.correlation) <- edges.name

if(pvalue == TRUE)  {
  dimension.pvalue <- matrix(pv.mat, nrow = length(vlist), ncol = length(vlist))
  colnames(dimension.pvalue) <- edges.name
  rownames(dimension.pvalue) <- edges.name

  return(list(dimension.correlation, dimension.pvalue))

}
 else{

return(dimension.correlation)
 }
}
}
