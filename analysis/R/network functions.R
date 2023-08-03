##################################################
# Functions for network manipulation and analysis#
##################################################

library(igraph)
library(rlang)
library(dplyr)
library(tidyr)
library(tidygraph)
library(ggplot2)

# 1. function to mark edges crossing between louvain clusters

community_crossing <- function(weights = NULL) {
  #expect_nodes()
  weights <- enquo(weights)
  weights <- eval_tidy(weights, .E())
  if (is.null(weights)) {
    weights <- NA
  }
  group <- igraph::crossing(cluster_louvain(graph = .G(), weights = weights), .G())
}

# 2. function to extract individual layer

onelayer <- function(lay){
  layx <- rlang::sym(lay)
  prep.graph %>% activate(edges) %>% filter(layer == layx) %>% activate(nodes) %>% filter(!node_is_isolated()) %>% as.igraph
}

# 3. function to make a heatmap from matrix
heatmap_matrix <- function(matrix){
  matrix[lower.tri(matrix, diag = TRUE)] <- NA
  cor_tri <- as.data.frame(matrix) %>%
    mutate(Var1 = factor(row.names(.), levels=row.names(.))) %>%
    tidyr::gather(key = Var2, value = value, -Var1, na.rm = TRUE, factor_key = TRUE)

  ggplot(data = cor_tri, aes(Var2, Var1, fill = value)) +
    geom_tile()+
    scale_fill_viridis_c()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
}

# 4. flatten a multilevel graph
flatten <- function(graph, weighted = FALSE){
  if(weighted == FALSE) {
    x <- graph %>%
      igraph::simplify(edge.attr.comb = "ignore") %>%
      tidygraph::as_tbl_graph()
    return(x)
  } else {
    graph %>%
      igraph::simplify(edge.attr.comb = list(weight = "sum", "ignore")) %>%
      tidygraph::as_tbl_graph()
  }
}


# 5. Functions to test the validity of eigenvector and betweenness centrality in binary backbones
centrality.samples <- function(s, graph, centrality) {

  # get size of fraction to sample
  y <- (s / 100) * igraph::gsize(graph)

  # sample graph

   g <- graph %>%
    activate(edges) %>%
    tidygraph::sample_n(size = y) %>%
    flatten(weighted = TRUE) %>%
    as.igraph()


  # extract backbone
  bb <- backbone::disparity(g, alpha = 0.15, narrative = FALSE) %>%
    tidygraph::as_tbl_graph()


  # calculate and extract  centrality
  if (centrality == "eigen") {
    q <- bb %>%
      as_tbl_graph() %>%
      activate(nodes) %>%
      mutate(centr = centrality_eigen(weights = weight)) %>%
      as_tibble() %>%
      dplyr::select(centr) %>%
      dplyr::rename(!!paste0("s", s) := centr)
  }

  if (centrality == "betweenness") {
    q <- bb %>%
      as_tbl_graph() %>%
      activate(nodes) %>%
      mutate(centr = centrality_betweenness(weights = weight)) %>%
      as_tibble() %>%
      dplyr::select(centr) %>%
      dplyr::rename(!!paste0("s", s) := centr)

  }

  return(q)

}

spearman_samples <- function(graph, centrality) {

  # sample graph at 10 percent intervals
  samples <- seq(10, 100, 10)


  # iterate function over intervals
  centralities <-
    samples %>%
    purrr::map(centrality.samples, graph = graph, centrality = centrality) %>%
    dplyr::bind_cols()

  # 2. Spearman's rank correlation

  # get sample names
  cols <- centralities %>%
    select(-s100) %>%
    names()

  # rank correlation function
  rank.correlation <- function(x) {
    suppressWarnings(
      cor.test(centralities$s100, centralities[[x]], method = "spearman")
    )
  }

  # iterate for samples
  purrr::map(cols, rank.correlation) %>%
    purrr::map_df(`[`, c("estimate")) %>%
    pull(estimate)
}


test_centralities <- function(graph, centrality, reps) {

  # check for correct arguments
  if (centrality %in% c("eigen", "betweenness") == F) {
    stop("centrality must be 'eigen' or 'betweenness'")
  }

  # get sample names
  s <- seq(10, 90, 10)
  samples <- paste("s", s, sep = "")

  # replicate the test
  replicate(reps, spearman_samples(graph, centrality)) %>%
    as_tibble() %>%
    mutate(sample = samples) %>%
    pivot_longer(!sample, names_to = "iteration", values_to = "value") %>%
    select(-iteration) %>%
    # plot
    ggplot() +
    geom_boxplot(aes(sample, value)) +
    labs(y = expression(paste("Spearman's ", rho))) +
    ylim(0, 1) +
    theme_publish()
}

# 6. Function to extract binary backbone and return to tidygraph object

library(backbone)

extract_backbone <- function(graph, alpha = 0.05) {
  # extract backbone
  a <- backbone::disparity(as.igraph(graph), alpha = alpha, narrative = TRUE) %>%
    tidygraph::as_tbl_graph()

  # igraph transformation loses the node attributes, so we put them back
  nodes <- graph %>%
    activate(nodes) %>%
    as_tibble()

  a %>%
    activate(nodes) %>%
    mutate(label = nodes$label, lat = nodes$lat, lon = nodes$lon)
}

# 7. plot a binary backbone

plot.bb <- function(bb) {
  ggraph(bb, layout = "stress") +
    geom_node_point() +
    geom_edge_fan(color = "red", alpha = 0.8) +
    scale_edge_width(range = c(0.2, 3)) +
    theme_graph()
}
