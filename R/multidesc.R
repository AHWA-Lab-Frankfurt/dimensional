#' A function for generating a list of descriptive measurements for the structure of a multilayer network
#' @export
multidesc <- function(graph){
  x <- flatten(graph)
  order <- igraph::gorder(x)
  density <- igraph::graph.density(x)
  cc <- igraph::transitivity(x, type = "global")
  degcent <- igraph::centralization.degree(x)
  avgpath <- igraph::average.path.length(x, directed = FALSE)
  diameter <- igraph::diameter(x, directed = FALSE, weights = igraph::E(x)$weight_sum)

  flattened <- tibble(order, density, cc, degcent$centralization, avgpath, diameter)


  layerwise.desc <- function(layer){
    layername <- rlang::sym(layer)
    layern <- enquo(layer)

    x <- decor.graph %>%
      activate(edges) %>%
      filter(name == layername)

    order <- igraph::gorder(x)
    density <- igraph::graph.density(x)
    cc <- igraph::transitivity(x, type = "global")
    degcent <- igraph::centralization.degree(x)
    avgpath <- igraph::average.path.length(x, directed = FALSE)
    diameter <- igraph::diameter(x, directed = FALSE, weights = E(x)$weight_sum)

    layername <- tibble(order, density, cc, degcent$centralization, avgpath, diameter)
  }

  #make a list of layers
  layerlist <- graph %>% activate(edges) %>% as_tibble() %>% distinct(name) %>% pull(name) %>% as.list()
  layerwise <- lapply(layerlist, layerwise.desc)
  layers <- layerwise %>% purrr::reduce(full_join)
  desc <- dplyr::bind_rows(flattened, layers)

  return(desc)

}

#to do on this function:
# 1) we need names on the final table in order to know which dimensions the rows represent
# 2) there is something weird about the results from average path length and transitivity. Need to check that we are using them correctly. What about weighting these cases?
