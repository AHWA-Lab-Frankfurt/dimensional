library(dimensional)

decor.graph <- tidygraph::tbl_graph(nodes = nodes_decor, edges = edges_decor, directed = FALSE)
dimensions <- decor.graph %>% igraph::get.data.frame("edges") %>% dplyr::pull(name) %>% unique
#dcna <- decor.graph %>% dimension_correlation_node(actors = c(2:7,15,18:19), dimensions = name)
dru <- dimension_relevance(decor.graph, dimensions = name)
drw <- dimension_relevance(decor.graph, dimensions = name, weighted = TRUE)

#is.binary <- function(x) all(x == 0 | x == 1)


testthat::test_that("there is a column for each dimension", {
  testthat::expect_identical(as.numeric(ncol(drw)-3), as.numeric(length(dimensions)))
})

testthat::test_that("the output is a df class", {
  testthat::expect_true(is.data.frame(drw))
  testthat::expect_true(is.data.frame(dru))
})

testthat::test_that("the matrix is symetric", {
  s <- sample(1:dim(dcnu)[1],2)
  testthat::expect_identical(dcnu[s[1],s[2]], dcnu[s[2],s[1]])
  testthat::expect_identical(dcnw[s[1],s[2]], dcnw[s[2],s[1]])
  testthat::expect_identical(dimension_correlation_node(decor.graph, dimensions = name, pvalue = TRUE)[[2]][s[1],s[2]], dimension_correlation_node(decor.graph, dimensions = name, pvalue = TRUE)[[2]][s[2],s[1]])
})

testthat::test_that("correlations are detected based on comparison with obviously or known correlations", {
  testthat::expect_true(dcnu["dragged_organic","knotted_cord_roulette"] == 1)
  testthat::expect_true(dcnu["straight_cord_mat","braided_strip_roulette"] > 0)
  testthat::expect_true(dcnw["dragged_organic","knotted_cord_roulette"] == 1)
  testthat::expect_true(dcnw["straight_cord_mat","braided_strip_roulette"] > 0)
})

testthat::test_that("pvalue is correct based on known very strong correlation", {
  testthat::expect_true(dcnu.pv["dragged_organic","knotted_cord_roulette"] < 0.05)
  testthat::expect_true(dcw.pv["dragged_organic","knotted_cord_roulette"] < 0.05)
})


