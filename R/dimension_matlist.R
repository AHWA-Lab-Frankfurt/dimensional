#' dimension correlation
#'
#' Creates a list containing the adjacency matrices for each layer of the network.
#' @param graph A graph of class tbl_graph
#' @param weighted Logical value if weight of the edges of the dimensions is considered or not. Defaults to unweighted network.
#' @import tidygraph
#' @importFrom rlang .data
#' @export
dimension_matlist <- function(graph, weighted = FALSE){
rlang::enquo(weighted)
  edges.name <- graph %>%
    igraph::get.data.frame("edges") %>%
    dplyr::pull(name) %>%
    unique()

  matlist <- list()
  cor.mat <- c()

  if(weighted == TRUE){

       for(j in 1:length(edges.name)) {
      mat <- graph %>%
       tidygraph::activate(edges) %>%
        dplyr::filter(name == edges.name[j]) %>%
        igraph::as_adjacency_matrix(attr = "weight")

      mat[lower.tri(mat, diag = TRUE)] <- NA
      mat <- mat %>%
        as.vector()%>%
        na.omit()

      matlist[[edges.name[j]]] <- mat

    }
    }


  else{

    for(j in 1:length(edges.name)) {
      mat <- graph %>%
       tidygraph::activate(edges) %>%
        dplyr::filter(name == edges.name[j]) %>%
        dplyr::select(-weight) %>%
        igraph::as_adjacency_matrix()

      mat[lower.tri(mat, diag = TRUE)] <- NA
      mat <- mat %>%
        as.vector()%>%
        na.omit()

      matlist[[edges.name[j]]] <- mat

    }
}
   return(matlist)

}
