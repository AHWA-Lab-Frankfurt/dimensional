#' Flatten a multilayer network
#'
#' A function to flatten a multilayer network into a single-layer network, see Dickison et al. 2016, p.39
#' @param graph A graph of the tbl-graph class
#' @param weighted should the flattening take edge weights into account (TRUE/FALSE)? Defaults to unweighted edges.
#' @param weights if weighted = TRUE, state the weight variable here.
#' @export
flatten <- function(graph, weighted = FALSE, weights){
  weight = rlang::enquo(weights)
  if(weighted == FALSE) {
    x <- graph %>%
      igraph::simplify(edge.attr.comb = "ignore") %>%
      tidygraph::as_tbl_graph()
    return(x)
  } else {
    graph %>%
      igraph::simplify(edge.attr.comb = list(weight = "sum", "ignore")) %>%
      tidygraph::as_tbl_graph()
  }
 }
