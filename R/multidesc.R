#' A function for generating a list of descriptive measurements for the structure of a multilayer network
#' @import tidygraph
#' @importFrom rlang .data
#' @export
compare_layers <- function(graph, var){
  x <- flatten(graph, weighted = TRUE)
  order <- igraph::gorder(x)
  density <- igraph::graph.density(x)
  cc <- igraph::transitivity(x, type = "global")
  degcent <- igraph::centralization.degree(x)
  avgpath <- igraph::average.path.length(x, directed = FALSE)
  diameter <- igraph::diameter(x, directed = FALSE, weights = igraph::E(x)$weight)

  flattened <- tibble::tibble(order, density, cc, degcent$centralization, avgpath, diameter) %>%
    mutate(layer = "flat graph", .before = 1)


  layerwise.desc <- function(layer){
    layername <- rlang::sym(layer)
    layern <- dplyr::enquo(layer)

    x <- graph %>%
      activate(edges) %>%
      filter({{var}} == layername) %>%
      activate(nodes) %>%
      filter(!node_is_isolated())

    order <- igraph::gorder(x)
    density <- igraph::graph.density(x)
    cc <- igraph::transitivity(x, type = "global")
    degcent <- igraph::centralization.degree(x)
    avgpath <- igraph::average.path.length(x, directed = FALSE)
    diameter <- igraph::diameter(x, directed = FALSE, weights = igraph::E(x)$weight)

    layername <- tibble::tibble(order, density, cc, degcent$centralization, avgpath, diameter) %>%
      mutate(layer = {{layer}}, .before = 1)
  }

  #make a list of layers
  layerlist <- graph %>% activate(edges) %>% as_tibble() %>% distinct({{var}}) %>% pull({{var}}) %>% as.list()
  layerwise <- purrr::map(layerlist, layerwise.desc)
  layers <- layerwise %>% purrr::reduce(full_join)
  desc <- dplyr::bind_rows(flattened, layers)

  return(desc)

}
