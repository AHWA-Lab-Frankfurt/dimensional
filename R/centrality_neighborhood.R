
#' Neighborhood centrality
#'
#' Calculate neighborhood centrality for a set of acotrs over a set of layers
#' @param graph A graph of class tbl_graph
#' @param actors An actor or a vector of actors. Defaults to all actors in the graph.
#' @param layers A layer or a vector of layers. Defaults to all layers of the graph
#' @import tidygraph
#' @importFrom rlang .data
#' @export

centrality_neighborhood <- function(graph, actors, layers){
  if (missing(actors) & missing(layers)) {
    x <- graph %>%
      morph(to_simple) %>%
      activate(nodes) %>%
      mutate(neighbours = centrality_degree()) %>%
      unmorph()
    return(x)
  } else if (missing(actors) & !missing(layers)){
    y <- graph %>%
      activate(edges) %>%
      filter(name == layers) %>%
      morph(to_simple) %>%
      activate(nodes) %>%
      mutate(neighbours = centrality_degree()) %>%
      unmorph()
    return(y)
  } else if (!missing(actors) & missing(layers)){
    z <- graph %>%
      activate(nodes) %>%
      filter(id == actors) %>%
      morph(to_simple) %>%
      activate(nodes) %>%
      mutate(neighbours = centrality_degree()) %>%
      unmorph()
    return(z)
  } else{
    graph %>%
      activate(edges) %>%
      filter(name == layers) %>%
      activate(nodes) %>%
      filter(id == actors) %>%
      morph(to_simple) %>%
      activate(nodes) %>%
      mutate(neighbours = centrality_degree()) %>%
      unmorph()
  }
}
