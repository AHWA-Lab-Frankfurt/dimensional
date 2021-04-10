library(dimensional)

decor.graph <- tidygraph::tbl_graph(nodes = nodes_decor, edges = edges_decor, directed = FALSE)

dcnu <- decor.graph %>% dimension_correlation_node(dimensions = name)
dcnw <- decor.graph %>% dimension_correlation_node(dimensions = name, weighted = TRUE)
dimensions <- decor.graph %>% igraph::get.data.frame("edges") %>% dplyr::pull(name) %>% unique
dcna <- decor.graph %>% dimension_correlation_node(actors = c(2:7,15,18:19), dimensions = name)
dcnu.pv <- dimension_correlation_node(decor.graph, dimensions = name, pvalue = TRUE)[[2]]
dcnw.pv <- dimension_correlation_node(decor.graph, dimensions = name, pvalue = TRUE, weighted = TRUE)[[2]]

is.binary <- function(x) all(x == 0 | x == 1)


testthat::test_that("there is a column vor each dimension", {
  testthat::expect_identical(ncol(dcnw), length(dimensions))
})

testthat::test_that("all outputs are of the right class", {
  testthat::expect_true(is.list(dimension_correlation_node(decor.graph, dimensions = name, pvalue = TRUE)))
  testthat::expect_true(is.matrix(dimension_correlation_node(decor.graph, dimensions = name)))
  testthat::expect_true(is.matrix(dimension_correlation_node(decor.graph, , dimensions = name, pvalue = TRUE)[[1]]))
  testthat::expect_true(is.matrix(dimension_correlation_node(decor.graph, , dimensions = name, pvalue = TRUE)[[2]]))
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


