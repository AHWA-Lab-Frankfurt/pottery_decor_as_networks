

# 1. Parameters for network graph coloured by edge type

coloured_edges <- list(geom_edge_link(aes(colour = name)),
                       geom_node_point(),
                       geom_node_text(aes(label = label))
                       )


# 2. Parameters for plotting graph communities

# edge parameters
edges_community <- list(geom_edge_fan(color = "gray",
                                      aes(edge_alpha = weight,
                                          edge_width = weight)),
                        scale_edge_width(range = c(0.2, 2)),
                        scale_edge_alpha(range = c(0.3, 0.6))
                        )

# colour nodes according to cluster and label
nodes_community <- list(geom_node_point(aes(color = louvain)),
                        geom_node_text(aes(label = label),
                                       col = "black",
                                       repel = TRUE)
                        )

# themes
theme_community <- list(labs(colour = "Community"),
                        guides(fill = "none",
                               alpha = "none",
                               size = "none",
                               edge_alpha = "none",
                               edge_width = "none",
                               colour = guide_legend(
                                 override.aes = list(size = 0.5)
                                 )
                               )
                        )
