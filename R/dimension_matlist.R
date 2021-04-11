#' dimension correlation
#'
#' Creates a list containing the adjacency matrices for each layer of the network. This can be used for further analyses of the layers (for example their correlation).
#' @param graph A graph of class tbl_graph
#' @param dimensions The Dimensions of the edges that should be taken into account. A variable of the edgelist.
#' @param weighted Logical value if weight of the edges of the dimensions is considered or not. Defaults to unweighted network.
#' @import tidygraph
#' @import rlang
#' @export

dimension_matlist <- function(graph, actors, dimensions, weighted = FALSE){
#if there is no dimension (variable of the edgelist)
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

#getting the names of the different dimensions, by getting all the different layers of the chosen aspect of the network
edges.name <- graph %>%
  igraph::get.data.frame("edges") %>%
  dplyr::pull(!!dimensions) %>%
  unique()

node.id <- graph %>%
  tidygraph::activate(nodes) %>%
  dplyr::pull(id)

 #the list where all the matrices will be saved
  matlist <- list()

  if(weighted == TRUE){

    #for-loop that extracts the adjacency matrix of every layer
       for(j in 1:length(edges.name)) {
      mat <- graph %>%
       tidygraph::activate(edges) %>%
      #filtering the edgelist for the dimension wanted
          dplyr::filter_at(dplyr::vars(!!dimensions), dplyr::all_vars(. == edges.name[j])) %>%
      #extracting the adjacency matrix
          igraph::as_adjacency_matrix(attr = "weight", type = "upper") %>%
        as.matrix()

      colnames(mat) <- node.id
      rownames(mat) <- node.id

#saving it under
      matlist[[edges.name[j]]] <- mat

    }
    }


  else{

    for(j in 1:length(edges.name)) {
      mat <- graph %>%
       tidygraph::activate(edges) %>%
        dplyr::filter_at(dplyr::vars(!!dimensions), dplyr::all_vars(. == edges.name[j])) %>%
                igraph::as_adjacency_matrix(type = "upper") %>%
        as.matrix()

   # mat[lower.tri(mat, diag = TRUE)] <- NA
      colnames(mat) <- node.id
      rownames(mat) <- node.id

      matlist[[edges.name[j]]] <- mat

    }
}
   return(matlist)

}
}

