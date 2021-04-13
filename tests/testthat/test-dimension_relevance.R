library(dimensional)

decor.graph <- tidygraph::tbl_graph(nodes = nodes_decor, edges = edges_decor, directed = FALSE)
dimensions <- decor.graph %>% igraph::get.data.frame("edges") %>% dplyr::pull(name) %>% unique
#dcna <- decor.graph %>% dimension_correlation_node(actors = c(2:7,15,18:19), dimensions = name)
dru <- dimension_relevance(decor.graph, dimensions = name)
drw <- dimension_relevance(decor.graph, dimensions = name, weighted = TRUE)



testthat::test_that("there is a column for each dimension", {
  testthat::expect_identical(as.numeric(ncol(drw)-3), as.numeric(length(dimensions)))
})

testthat::test_that("the output is a df class", {
  testthat::expect_true(is.data.frame(drw))
  testthat::expect_true(is.data.frame(dru))
})


