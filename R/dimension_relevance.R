#' Dimension relevance
#'
#' Calculate the relevance of the dimensions(layers) for each node of a network. The output is a dataframe with the neighbor_centrality of the whole network and the relevance of each dimension for each node aswell as the mean relevance of each dimension for the whole network.
#' @param graph A graph of class tbl_graph
#' @param weighted Logical value if the neighborhood centrality should be weighted or not. Defaults to unweighted network.
#' @export

dimension_relevance <- function(graph, actors, dimensions, weighted = FALSE) {
  if(missing(dimensions)){
    stop("dimension missing without default")
  }
  else{
    dimensions <- rlang::enquo(dimensions)

  #graph <- enquote(graph)
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

  #the nodes.id, the nodes.label and the edges.name vectors are needed for the for-loop
  nodes.id <- graph %>%
   igraph::get.data.frame("vertices") %>%
    dplyr::pull(id)

  nodes.label <- graph %>%
    igraph::get.data.frame("vertices") %>%
    dplyr::pull(label)



  #a df in which the neighbor values of the different nodes will be documented
  dimcentrality.df <- data.frame(id = nodes.id, label = nodes.label)

  if(weighted == TRUE){
    deg <- graph%>%
     centrality_neighborhood(weighted = TRUE)

    deg <- data.frame(deg = deg)


    dimcentrality.df <- dimcentrality.df %>%
      add_column(deg)

    names(dimcentrality.df)[names(dimcentrality.df) == "deg"] <- "neighbor"

    for(j in 1:length(edges.name)){

      ##we create a vector of all the centralities of a layer
      deg <- decor.graph %>%
        tidygraph::activate(edges) %>%
        dplyr::filter_at(dplyr::vars(!!dimensions), dplyr::all_vars(. == edges.name[j])) %>%
        tidygraph::activate(nodes) %>%
        dplyr::mutate(deg = tidygraph::centrality_degree(weights = weight)) %>%
        dplyr::pull(deg)

      deg <- data.frame(deg = deg)

      dimcentrality.df <- dimcentrality.df %>%
        add_column(deg)

      names(dimcentrality.df)[names(dimcentrality.df) == "deg"] <- edges.name[j]
    }

  }
  else{

    deg <- graph%>%
     centrality_neighborhood()


    dimcentrality.df <- dimcentrality.df %>%
      add_column(deg)

    names(dimcentrality.df)[names(dimcentrality.df) == "deg"] <- "neighbor"

    for(j in 1:length(edges.name)){

      ##we create a vector of all the centralities of a layer
      deg <- decor.graph %>%
        tidygraph::activate(edges) %>%
        dplyr::filter_at(dplyr::vars(!!dimensions), dplyr::all_vars(. == edges.name[j])) %>%
        tidygraph::activate(nodes) %>%
        dplyr::mutate(deg = tidygraph::centrality_degree()) %>%
        dplyr::pull(deg)

      deg <- data.frame(deg = deg)


      dimcentrality.df <- dimcentrality.df %>%
        add_column(deg)

      names(dimcentrality.df)[names(dimcentrality.df) == "deg"] <- edges.name[j]

    }

  }

  dimrel.df <- dimcentrality.df %>%
    dplyr::mutate(across(4:ncol(dimcentrality.df), function(x) x / neighbor))

  dimrelmean <- dimrel.df %>%
    dplyr::select(-c(1:2)) %>%
    dplyr::summarise_all(mean) %>%
    add_column(id = 0, label = "mean")

  dimrel.df <- rbind(dimrel.df, dimrelmean) %>%
    dplyr::mutate_at(dplyr::vars(3:length(dimrel.df)), as.numeric)
  #rounding would make it more beautiful
  # %>%
   # dplyr::mutate_if(is.numeric, round(digits = 3))

  return(dimrel.df)
}
}
