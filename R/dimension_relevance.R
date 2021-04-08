#' Dimension relevance
#'
#' Calculate the relevance of the dimensions(layers) for each node of a network. The output is a dataframe with the neighbor_centrality of the whole network and the relevance of each dimension for each node aswell as the mean relevance of each dimension for the whole network.
#' @param graph A graph of class tbl_graph
#' @param weighted Logical value if the neighborhood centrality should be weighted or not. Defaults to unweighted network.
#' @export

dimension_relevance <- function(graph, actors, weighted = FALSE) {
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
    tidygraph::get.data.frame("vertices") %>%
    select(id) %>%
    deframe

  nodes.label <- graph %>%
    tidygraph::get.data.frame("vertices") %>%
    select(label) %>%
    deframe

  edges.name <- graph %>%
    tidygraph::get.data.frame("edges") %>%
    select(name) %>%
    deframe() %>%
    unique()

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
        centrality_neighborhood(layer = edges.name[j], weighted = TRUE)

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
        centrality_neighborhood(layer = edges.name[j])

      deg <- data.frame(deg = deg)


      dimcentrality.df <- dimcentrality.df %>%
        add_column(deg)

      names(dimcentrality.df)[names(dimcentrality.df) == "simple"] <- edges.name[j]

    }

  }

  dimrel.df <- dimcentrality.df %>%
    mutate(across(4:ncol(dimcentrality.df), function(x) x / neighbor))

  dimrelmean <- dimrel.df %>%
    select(-c(1:2)) %>%
    summarise_all(mean) %>%
    add_column(id = 0, label = "mean")

  dimrel.df <- rbind(dimrel.df, dimrelmean)

  return(dimrel.df)
}
