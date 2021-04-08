library(dimensional)

decor.graph <- tidygraph::tbl_graph(nodes = nodes_decor, edges = edges_decor, directed = FALSE)
cmu <- decor.graph %>% dimension_matlist(name)
cmw <- decor.graph %>% dimension_matlist(name, weighted = TRUE)
dimensions <- decor.graph %>% igraph::get.data.frame("edges") %>% dplyr::pull(name) %>% unique
dcu <- dimension_correlation(cmu)
dcw <- dimension_correlation(cmw)
dcu.pv <- dimension_correlation(cmu, pvalue = TRUE)[[2]]
dcw.pv <- dimension_correlation(cmw, pvalue = TRUE)[[2]]


testthat::test_that("the matrix matches the dimensions(unweighted)", {
  testthat::expect_identical(length(dimensions), nrow(dcu))
})

testthat::test_that("the matrix matches the dimensions(weighted)", {
  testthat::expect_identical(length(dimensions), nrow(dcw))
})

testthat::test_that("all outputs are of the right class", {
  testthat::expect_true(is.list(dimension_correlation(cmu, pvalue = TRUE)))
  testthat::expect_true(is.matrix(dimension_correlation(cmu)))
  testthat::expect_true(is.matrix(dimension_correlation(cmu, pvalue = TRUE)[[1]]))
  testthat::expect_true(is.matrix(dimension_correlation(cmu, pvalue = TRUE)[[2]]))
})

testthat::test_that("the matrix is symetric", {
s <- sample(1:dim(dcu)[1],2)
   testthat::expect_identical(dcu[s[1],s[2]], dcu[s[2],s[1]])
   testthat::expect_identical(dcw[s[1],s[2]], dcw[s[2],s[1]])
   testthat::expect_identical(dimension_correlation(cmu, pvalue = TRUE)[[2]][s[1],s[2]], dimension_correlation(cmu, pvalue = TRUE)[[2]][s[2],s[1]])
})

testthat::test_that("correlations are detected based on comparison with obviously or known correlations", {
  testthat::expect_true(dcu["dragged_organic","knotted_cord_roulette"] == 1)
  testthat::expect_true(dcu["straight_cord_mat","braided_strip_roulette"] > 0)
  testthat::expect_true(dcw["dragged_organic","knotted_cord_roulette"] == 1)
  testthat::expect_true(dcw["straight_cord_mat","braided_strip_roulette"] > 0)
})

testthat::test_that("pvalue is correct based on known very strong correlation", {
  testthat::expect_true(dcu.pv["dragged_organic","knotted_cord_roulette"] < 0.05)
  testthat::expect_true(dcw.pv["dragged_organic","knotted_cord_roulette"] < 0.05)
})

