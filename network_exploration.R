# Load packages
library(tidyverse)
library(igraph)
library(ggraph)

# Read the nodes file into the variable nodes
nodes <- read_csv("data/nodes.csv")

# Read the ties file into the variable ties
ties <- read_csv("data/ties.csv")

# Print nodes
nodes # 64 nodes


# Print ties
ties # 243 ties

# Make the network from the data frame ties and print it
g <- graph_from_data_frame(ties, directed = FALSE, vertices = nodes)
#g

# Give the name "Terrorist network" to the network 
g$name <- "Terrorist network"
#g$name

# Add node attribute id and print the node `id` attribute
V(g)$id <- seq(1, vcount(g))
V(g)$id

# Explore the set of nodes
V(g)

# Print the number of nodes

vcount(g)

# Explore the set of ties
E(g)

# Print the number of ties
ecount(g)





# Print the tie `weight` attribute
E(g)$weight

############################
# Initial data vizualization
############################

# Visualize the network with the Kamada-Kawai layout 
ggraph(g, layout = "with_kk") + 
  # Add an edge link geometry mapping transparency to weight 
  geom_edge_link(aes(alpha = weight)) + 
  # Add a node point geometry
  geom_node_point() +
  # Add a node text geometry, mapping label to id and repelling
  geom_node_text(aes(label = id), repel = TRUE)

# The network has a typical core-periphery structure, with a densely knitted center and a sparser periphery around it.

# Visualize the network in a circular layout
ggraph(g, layout = "in_circle") + 
  # Map tie transparency to its weight
  geom_edge_link(aes(alpha = weight)) + 
  geom_node_point()

# Visualize the network in a gird layout
ggraph(g, layout = "on_grid") + 
  # Map tie transparency to its weight
  geom_edge_link(aes(alpha = weight)) + 
  geom_node_point()


#####################
# Centrality measures
#####################

# Find the most connected terrorists of the train bombing network using degree centrality:

nodes_with_centrality <- nodes %>%
  # Add a column containing the degree of each node
  mutate(degree = degree(g)) %>%
  # Arrange rows by descending degree
  arrange(desc(degree))

# See the result
nodes_with_centrality

# The ranking leader, Jamal Zougam, was in fact directly involved in the bombings and was one of the first to be arrested.


# Find the most strongly connected terrorists of the train bombing network using strength centrality:

nodes_with_centrality <- nodes %>%
  mutate(
    degree = degree(g),
    # Add a column containing the strength of each node
    strength = strength(g)
  ) %>%
  # Arrange rows by descending strength
  arrange(desc(strength))

# See the result
nodes_with_centrality

# Add betweenness and closeness

nodes_with_centrality <- nodes %>%
  mutate(
    degree = degree(g),
    # Add a column containing the strength of each node
    strength = strength(g),
    betweenness = betweenness(g),
    closeness = closeness(g)
  ) %>%
  # Arrange rows by descending strength
  arrange(desc(closeness))

# See the result
nodes_with_centrality


#################
# Tie betweenness
#################

# Ties with high betweenness may have considerable influence within a network 
# by virtue of their control over information passing between nodes. 
# Removing them will most disrupt communication between nodes.

# Calculate the reciprocal of the tie weights
dist_weight <- 1 / E(g)$weight

ties_with_betweenness <- ties %>%
  # Add an edge betweenness column weighted by dist_weight
  mutate(betweenness = edge_betweenness(g, weights = dist_weight))

# Review updated ties
ties_with_betweenness

ties_joined <- ties_with_betweenness %>% 
  left_join(nodes, by = c("from" = "id")) %>% 
  left_join(nodes, by = c("to" = "id"))

# Select only relevant variables
ties_selected <- ties_joined %>% 
    select(from, to, name_from = name.x, name_to = name.y, betweenness)

# See the result
ties_selected

ties_selected %>%
  # Arrange rows by descending betweenness
  arrange(desc(betweenness))


# Need to figure out how to add edge betweenness to g

##############################
# Finding strong and weak ties
##############################

tie_counts_by_weight <- ties %>% 
  # Count the number of rows with each weight
  count(weight) %>%
  # Add a column of the percentage of rows with each weight
  mutate(percentage = 100 * n / nrow(ties)) 

# See the result
tie_counts_by_weight

# 88% of the network ties are weak, quite an impressive share!

# Make is_weak TRUE whenever the tie is weak
is_weak <- E(g)$weight == 1

# Check that the number of weak ties is the same as before
sum(is_weak)

ggraph(g, layout = "with_kk") +
  # Add an edge link geom, mapping color to is_weak
  geom_edge_link(aes(color = is_weak))

ggraph(g, layout = "with_kk") + 
  # Map filter to is_weak
  geom_edge_link(aes(filter = is_weak), alpha = 0.5)


#####################
# Connection patterns
#####################

# From previous steps
ties_swapped <- ties %>%
  mutate(temp = to, to = from, from = temp) %>% 
  select(-temp)
ties_bound <- bind_rows(ties, ties_swapped)

# Using ties_bound, plot to vs. from, filled by weight
ggplot(ties_bound, aes(x = from, y = to, fill = factor(weight))) +
  # Add a raster geom
  geom_raster() +
  # Label the color scale as "weight"
  labs(fill = "weight")


# Get the weighted adjacency matrix
A <- as_adjacency_matrix(g, attr = "weight", names = FALSE)

# See the results
A

#####################
# Pearson correlation
#####################


#############################################
# Most similar and most dissimilar terrorists
#############################################


#########################
# Hierarchical clustering
#########################

############################
# Interactive vizualizations
############################

# Load visNetwork
library(visNetwork)

# Convert from igraph to visNetwork
data <- toVisNetworkData(g)

# Print the head of the data nodes
head(data$nodes)

# ... do the same for the edges (ties)
head(data$edges)

# Visualize the network
visNetwork(nodes = data$nodes, edges = data$edges)

# See a list of possible layouts
ls("package:igraph", pattern = "^layout_.")



visNetwork(nodes = data$nodes, edges = data$edges) %>%
  # Change the layout to be KK
  visIgraphLayout(layout = "layout_with_kk") %>%
  # Change the options to highlight the nearest nodes and ties
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
  








