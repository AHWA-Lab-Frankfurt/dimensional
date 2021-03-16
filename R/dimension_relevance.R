dimension_relevance <- function(graph, weighted) {
  #graph <- enquote(graph)

  #the nodes.id, the nodes.label and the edges.name vectors are needed for the for-loop
  nodes.id <- graph %>%
    get.data.frame("vertices") %>%
    select(id) %>%
    deframe

  nodes.label <- graph %>%
    get.data.frame("vertices") %>%
    select(label) %>%
    deframe

  edges.name <- graph %>%
    get.data.frame("edges") %>%
    select(name) %>%
    deframe() %>%
    unique()

  #a df in which the neighbor values of the different nodes will be documented
  dimcentrality.df <- data.frame(id = nodes.id, label = nodes.label)

  if(missing(weighted) | weighted == FALSE){

    deg <- graph%>%
      centrality_neighborhood()

    dimcentrality.df <- dimcentrality.df %>%
      add_column(deg)

    names(dimcentrality.df)[names(dimcentrality.df) == "deg"] <- "neighbor"

    for(j in 1:length(edges.name)){

      ##we create a vector of all the centralities of a layer
      deg <- decor.graph %>%
        centrality_neighborhood(layer = edges.name[j])


      dimcentrality.df <- dimcentrality.df %>%
        add_column(deg)

      names(dimcentrality.df)[names(dimcentrality.df) == "deg"] <- edges.name[j]

    }
  }
  else{

    deg <- graph%>%
      centrality_neighborhood(weighted = TRUE)

    dimcentrality.df <- dimcentrality.df %>%
      add_column(deg)

    names(dimcentrality.df)[names(dimcentrality.df) == "deg"] <- "neighbor"

    for(j in 1:length(edges.name)){

      ##we create a vector of all the centralities of a layer
      deg <- decor.graph %>%
        centrality_neighborhood(layer = edges.name[j], weighted = TRUE)


      dimcentrality.df <- dimcentrality.df %>%
        add_column(deg)

      names(dimcentrality.df)[names(dimcentrality.df) == "deg"] <- edges.name[j]
    }

  }
  dimrel.df <- dimcentrality.df %>%
    mutate(across(4:ncol(dimcentrality.df), function(x) x / neighbor))

  dimrel.df <- dimcentrality.df %>%
    mutate(across(4:ncol(dimcentrality.df), function(x) x / neighbor))

  dimrelmean <- dimrel.df %>%
    select(-c(1:2)) %>%
    summarise_all(mean) %>%
    add_column(id = 0, label = "mean")

  dimrel.df <- rbind(dimrel.df, dimrelmean)

}
