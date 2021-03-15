#' A function to flatten a multilayer network into a single-layer network, see Dickison et al. 2016, p.39
#' @export
flatten <- function(graph, weight){
  weight = rlang::enquo(weight)
  if(missing(weight)) {
    x <- graph %>%
      igraph::simplify() %>%
      as_tbl_graph()
    return(x)
  } else {
    graph %>%
      igraph::simplify(edge.attr.comb = list(weight = "sum", "ignore")) %>%
      as_tbl_graph()
  }
 }
