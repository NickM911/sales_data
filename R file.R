setwd("C:/Users/NJM/Desktop/Computing/Deployment/Practice run/Recommendation/Network")
#Network analysis and visualisation of associations
# Packages
Packages = c("sna", "igraph", "intergraph", "visNetwork", "geomnet", 
             "reshape2", "stringr", "dplyr", "readr")
lapply(Packages, library, character.only = TRUE)


# LOAD NODE LIST IN AND FORMAT
nodes = read_csv('nodes.csv')
# function is to format data specifically for visNetwork package
format_nodes = function(nodes_df) {
  nodes_df = nodes_df
  colnames(nodes_df) = c("id", "PROD_ID")
  #nodes_df$group = gsub("1", "TC14", nodes_df$group) 
  #nodes_df$group = gsub("0", "Non-TC14", nodes_df$group) 
  nodes_df$PRODUCT = nodes_df$id
  
  return(nodes_df)
}
nodes = format_nodes(nodes)


# Load adjacency matrix in
weighted_matrix = as.data.frame(read_csv("adj_matrix.csv"))
# function is to format data specifically for visNetwork package
format_matrix = function(matrix_df) {
  matrix_df = matrix_df
  rownames(matrix_df) = colnames(matrix_df)
  matrix_df = as.matrix(matrix_df) # convert to matrix for melt() function to work in next function
  
  return(matrix_df) # returns object you can use to make edge list
}

weighted_matrix = format_matrix(weighted_matrix)

edges_from_matrix = function(matrix_df) {
  edges = melt(matrix_df) # create edges, which visNetwork() will use
  colnames(edges) = c("from", "to", "width")
  edges = subset(edges, edges$width > 0)
  edges$label = edges$width
  edges$width = edges$width/10
  
  return(edges)
  
}
edges = edges_from_matrix(weighted_matrix)


# Visualise network object
network_poc = visNetwork(nodes, edges, 
                         width = "100%", height = "1000px",
                         # background = "lightgrey",
                         main = "Products sharing transactions", 
                         submain = "Link indicates how many transactions each product shared")%>%
  
  visIgraphLayout(layout = "layout_in_circle") %>%
  
  visNodes(shape = "none", 
           color = list(
             background = "#0085AF",
             border = "#013848",
             highlight = "#FF8000"),
           font = list(size = 20)) %>%
  
  visEdges(shadow = FALSE,
           # color = "lightgrey",
           hoverWidth = 0.8,
           # Colour and size of edge LABELS
           font = list(size = 20, color = "black", opacity = 1)) %>%
  
  visOptions(highlightNearest = list(enabled = TRUE, degree = 0, hover = TRUE),
             selectedBy = list(variable = "PRODUCT", multiple = TRUE)) %>%
  # nodesIdSelection = TRUE) %>%
  
  visLegend(width = 0.1, position = "left", zoom = FALSE) %>% 
  
  visInteraction(navigationButtons = TRUE, keyboard = TRUE, hideEdgesOnDrag=TRUE, multiselect = TRUE)

# Save network object
visSave(network_poc, file = "network_poc.html")



