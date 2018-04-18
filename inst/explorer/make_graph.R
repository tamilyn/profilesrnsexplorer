
make_edge_graph <- function(edges, title = "Sample Graph" ) {
  require(ggraph)
  require(tidygraph)
  require(tidyverse)
  require(igraph)

  g <- as_tbl_graph(edges)  %>% 
    mutate(Popularity = centrality_degree(mode= 'in'))
  
  ggraph(g, layout = 'fr') +
    geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE) +
    geom_node_point(aes(size = Popularity), show.legend = FALSE) +
    geom_node_label(aes(label = name)) +
    #facet_edges(~year) +
    theme_graph(foreground = 'steelblue', 
                fg_text_colour = 'white') + 
    ggtitle(title)
}
