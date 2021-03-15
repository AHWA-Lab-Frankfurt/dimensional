#' Edges between 22 archaeological sites in West Africa.
#'
#' An edgelist dataset containing the connections between 22 archaeological sites in West Africa
#'
#' @format A data frame with 661 rows and 6 variables:
#' \describe{
#'   \item{from}{originating node}
#'   \item{to}{receiving node}
#'   \item{weight}{weight of the edge}
#'   \item{type}{archaeological find class}
#'   \item{subtype}{type of observation}
#'   \item{name}{observation, this is the network layer}
#' }
"edges_decor"

#'Nodes of a network of 22 archaeological sites in West Africa
#'
#' A nodelist dataset containing 22 archaeological sites in West Africa as the nodes of a graph.
#'
#' @format A data frame with 22 rows and 4 columns
#' \describe{
#'   \item{id}{node id}
#'   \item{label}{name of site}
#'   \item{lat}{geographical latitude}
#'   \item{lon}{geographical longitude}
#' }
"nodes_decor"
