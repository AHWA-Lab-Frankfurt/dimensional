#' Neighborhood centrality
#'
#' Calculate neighborhood centrality for a set of acotrs over a set of layers. The output is a vector with values for every node.
#' @param graph A graph of class tbl_graph
#' @param actors An actor or a vector of actors. Defaults to all actors in the graph.
#' @param layers A layer or a vector of layers. Defaults to all layers of the graph
#' @param weighted Logical value if the neighborhood centrality should be weighted or not. Defaults to unweighted network.
#' @export

centrality_neighborhood <- function(graph, actors, layers, weighted = FALSE){
  if(weighted == TRUE) {
    if (missing(actors) & missing(layers)) {
      x <- graph %>%
        flatten(weighted = TRUE) %>%
        activate(nodes) %>%
        mutate(deg = centrality_degree(weights = weight)) %>%
        pull(deg)

      return(x)
    } else if (missing(actors) & !missing(layers)){
      y <- graph %>%
        flatten(weighted = TRUE) %>%
        activate(edges) %>%
        filter(name == layers) %>%
        activate(nodes) %>%
        mutate(deg = centrality_degree(weights = weight)) %>%
        pull(deg)

      return(y)
    } else if (!missing(actors) & missing(layers)){
      z <- graph %>%
        flatten(weighted = TRUE) %>%
        activate(nodes) %>%
        filter(id == actors) %>%
        mutate(deg = centrality_degree(weights = weight)) %>%
        pull(deg)
      return(z)
    } else{
      graph %>%
        flatten(weighted = TRUE) %>%
        activate(edges) %>%
        filter(name == layers) %>%
        activate(nodes) %>%
        filter(id == actors) %>%
        mutate(deg = centrality_degree(weights = weight)) %>%
        pull(deg)
    }
  }
  else{
    if (missing(actors) & missing(layers)) {
      a <- graph %>%
        flatten() %>%
        activate(nodes) %>%
        mutate(deg = centrality_degree()) %>%
        pull(deg)
      return(a)
    } else if (missing(actors) & !missing(layers)){
      b <- graph %>%
        activate(edges) %>%
        filter(name == layers) %>%
        morph(to_simple) %>%
        activate(nodes) %>%
        mutate(deg = centrality_degree()) %>%
        pull(deg)
      return(b)
    } else if (!missing(actors) & missing(layers)){
      c <- graph %>%
        activate(nodes) %>%
        filter(id == actors) %>%
        filter(!node_is_isolated()) %>%
        morph(to_simple)%>%
        activate(nodes) %>%
        mutate(deg = centrality_degree()) %>%
        pull(deg)
      return(c)
    } else{
      graph %>%
        activate(edges) %>%
        filter(name == layers) %>%
        activate(nodes) %>%
        filter(id == actors) %>%
        morph(to_simple) %>%
        mutate(deg = centrality_degree()) %>%
        pull(deg)
    }
  }
}
