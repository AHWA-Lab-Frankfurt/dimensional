library(dimensional)

decor.graph <- tidygraph::tbl_graph(nodes = nodes_decor, edges = edges_decor, directed = FALSE)

cmu <- decor.graph %>% dimension_matlist(name)
cmw <- decor.graph %>% dimension_matlist(name, weighted = TRUE)
dimensions <- decor.graph %>% igraph::get.data.frame("edges") %>% dplyr::pull(name) %>% unique


is.binary <- function(x) all(x == 0 | x == 1)


testthat::test_that("there is matrix for each layer", {
  testthat::expect_identical(length(cmw), length(dimensions))
})


testthat::test_that("unweighted correlation matrices are binary", {
  for(i in 1:length(cmu)){
    testthat::expect_true(is.binary(cmu[[i]]))
  }
})


testthat::test_that("weighted correlation matrices are not binary", {
  for(i in 1:length(cmw)){
    testthat::expect_false(is.binary(cmw[[i]]))
  }
})

testthat::test_that("unweighted matrices match the network", {
  for(i in 1:length(cmw)){
    adj <- igraph::graph_from_adjacency_matrix(cmw[[i]], mode = "upper", diag = FALSE, weighted = TRUE) %>% igraph::get.data.frame("edges") %>%  dplyr::select(from, to) %>%  dplyr::arrange(from, to)
    graph <- decor.graph %>% igraph::get.data.frame("edges") %>% dplyr::filter(name == dimensions[i]) %>%  dplyr::select(from, to)%>%  dplyr::arrange(from, to)
    testthat::expect_equal(adj, graph)
  }
})

testthat::test_that("weighted matrices match the network", {
  for(i in 1:length(cmw)){
   adj <- igraph::graph_from_adjacency_matrix(cmw[[i]], mode = "upper", diag = FALSE, weighted = TRUE) %>% igraph::get.data.frame("edges")  %>%  dplyr::arrange(from, to)
   graph <- decor.graph %>% igraph::get.data.frame("edges") %>%  dplyr::filter(name == dimensions[i]) %>% dplyr::select(from, to, weight)%>%  dplyr::arrange(from, to)
   testthat::expect_equal(adj, graph)
  }
})

