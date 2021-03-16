library(dimensional)
decor.graph <- tidygraph::tbl_graph(nodes = nodes_decor, edges = edges_decor, directed = FALSE)

unweight.flattened <- decor.graph %>% igraph::simplify(edge.attr.comb = "ignore") %>%
  tidygraph::as_tbl_graph()

uf <- flatten(decor.graph)
wf <- flatten(decor.graph, weighted = TRUE)
wwf <- flatten(decor.graph, weighted = TRUE, weights = weight)

test_that("unweighted flattening does not retain weights", {
  expect_false(igraph::is.weighted(uf))
})

test_that("weighted flattening retains weights in named column: weight", {
  expect_true(igraph::is.weighted(wf))
})

test_that("weighted flattening retains weights in specified column", {
  expect_true(igraph::is.weighted(wwf))
})
