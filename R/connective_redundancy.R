#' Connective Redundancy
#'
#' Calculate the connective redundancy of a set of actors over a set of layers within a multilayer graph.
#' @param graph A graph object of the tbl_graph type
#' @param actors An actor or a vector of actors, stored in the "id" column.
#' @param layers A layer or a vector of layers, stored in the "name" column.
#' @export

connective_redundancy <- function(graph, actors, layers){
  if (missing (actors) & missing (layers)){
    x <- graph %>%
      activate(nodes) %>%
      mutate(degree = centrality_degree()) %>%
      morph(to_simple) %>%
      mutate(neighbours = centrality_degree()) %>%
      unmorph %>%
      select(label, degree, neighbours) %>%
      mutate(redundancy = 1 - (neighbours/degree)) %>%
      as_tibble()
    return(x)
  } else if (missing(actors) & !missing(layers)){
    y <- graph %>%
      activate(edges) %>%
      filter(name == layers) %>%
      activate(nodes) %>%
      mutate(degree = centrality_degree()) %>%
      morph(to_simple) %>%
      mutate(neighbours = centrality_degree()) %>%
      unmorph %>%
      select(label, degree, neighbours) %>%
      mutate(redundancy = 1 - (neighbours/degree)) %>%
      as_tibble()
    return(y)
  } else if (!missing(actors) & missing(layers)){
    z <- graph %>%
      activate(nodes) %>%
      filter(id == actors) %>%
      mutate(degree = centrality_degree()) %>%
      morph(to_simple) %>%
      mutate(neighbours = centrality_degree()) %>%
      unmorph %>%
      select(label, degree, neighbours) %>%
      mutate(redundancy = 1 - (neighbours/degree)) %>%
      as_tibble()
    return(z)
  } else{
    graph %>%
      activate(edges) %>%
      filter(name == layers) %>%
      activate(nodes) %>%
      filter(id == actors) %>%
      mutate(degree = centrality_degree()) %>%
      morph(to_simple) %>%
      mutate(neighbours = centrality_degree()) %>%
      unmorph %>%
      select(label, degree, neighbours) %>%
      mutate(redundancy = 1 - (neighbours/degree)) %>%
      as_tibble()
  }
}
