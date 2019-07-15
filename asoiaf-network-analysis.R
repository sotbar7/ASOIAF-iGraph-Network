#########################################
#   A SONG OF ICE AND FIRE              #
#   SOCIAL NETWORK ANALYTICS (graph)    #
#   SOTIRIS BARATSAS                    #
#   Contact: sotbaratsas[at]gmail.com   #
#########################################

library(igraph)
library(igraphdata)
library(dplyr)
library(ggplot2)


# ----------
# QUESTION 1
# ----------

# CREATING A GRAPH, USING ONLY THE COLUMNS "source", "target" and "weight"
data <- read.csv(file="https://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-all-edges.csv", header=T, sep=",")[ ,c(1,2,5)]
data$weight<-as.character(data$weight)
str(data)

data2<-as.matrix(data)
head(data2)

# METHOD 1: READ FROM DATA FRAME
ig <- graph_from_data_frame(data, directed = FALSE, vertices = NULL)

# METHOD 2: READ FROM EDGELIST
ig2 <- graph_from_edgelist(data2[,1:2], directed = FALSE)
g <- graph.edgelist(data2[,1:2])
E(g)$weight <- as.numeric(data2[,3])

# GETTING A LOOK AT THE DATA
ig
V(ig) # names or labels of the vertices/ nodes
E(ig) # map of the connections or edges between vertices
edge_attr(ig) # weight
is.weighted(ig) # Checking if the graph is weighted
vertex_attr(ig) # Faction, name, label, color



# ----------
# QUESTION 2
# ----------


gorder(ig) # number of vertices in network
gsize(ig) # number of edges in network
diameter(ig) # diamater of the network

length(triangles(ig)) # count the number of triangles in the graph
sum(count_triangles(ig)) # alternative method
# However these methods take into account thrice the number of actual triangles, which should not be counted, because our graph is undirected.
# For example, the triangle [Tyrion-Lannister , Arya-Stark , Benjen-Stark] should be considered the same as the triangles [Arya-Stark , Benjen-Stark, Tyrion-Lannister] and [Benjen-Stark, Arya-Stark, Tyrion-Lannister].

# To find the actual number of triangles, we use:
graph.motifs(ig) # 5655 is the number representing triangles
graph.count.subisomorphisms.vf2(ig, graph.full(3)) / 6
#The result of graph.count.subisomorphisms.vf2 has to be divided by 6 because there are six possible ways to map a triangle to itself (by permuting the vertices), hence each triangle will be counted six times.

# Another way to correct this problem would be to use
length(triangles(ig)) / 3 # count the number of triangles in the graph
sum(count_triangles(ig)) / 3  # alternative method


head(sort(degree(ig), decreasing=TRUE), 10) # Top 10 characters of the network based on Degree
head(sort(strength(ig), decreasing=TRUE), 10) # Top 10 characters of the network based on Weighted Degree



# ----------
# QUESTION 3
# ----------

# Plotting the original network
plot(ig, vertex.label = NA, layout=layout_nicely, vertex.size=3, edge.arrow.width=0.75)
plot(ig, vertex.label = NA, layout=layout_on_sphere, vertex.size=3, edge.arrow.width=0.75)
rglplot(ig, vertex.label = NA, layout=layout_nicely, vertex.size=3, vertex.color="blue", edge.arrow.width=0.75)
rglplot(ig, vertex.label = NA, layout=layout_on_sphere, vertex.size=3, vertex.color="blue", edge.arrow.width=0.75)


### SUBGRAPH ###

# Creating a subgraph by keeping all vertices that have 10 or more (>9) connections in the network
vrt<-which(degree(ig) > 9)
sg <- induced_subgraph(ig, vrt)

# Getting a sense of the subgraph we created
gorder(sg) # number of vertices in network
gsize(sg) # number of edges in network
diameter(sg) # diamater of the network
head(sort(degree(sg), decreasing=TRUE), 10) # Top 10 characters of the network based on Degree
head(sort(strength(sg), decreasing=TRUE), 10) # Top 10 characters of the network based on Degree

# Plotting the subgraph
plot(sg, vertex.label = NA, layout=layout_nicely, vertex.size=3, edge.arrow.width=0.75)
plot(sg, vertex.label = NA, layout=layout_on_sphere, vertex.size=3, edge.arrow.width=0.75)
rglplot(sg, vertex.label = NA, layout=layout_nicely, vertex.size=3, vertex.color="blue", edge.arrow.width=0.75)
rglplot(sg, vertex.label = NA, layout=layout_on_sphere, vertex.size=3, vertex.color="blue", edge.arrow.width=0.75)


### EDGE DENSITY

# Calculating Edge Density for the whole graph
edge_density(ig, loops = FALSE) # loops=F because we don't have any self-loops in the network

# Calculating Edge Density for the subgraph we created
edge_density(sg, loops = FALSE)

# As we can see, the Edge Density in both cases is low. That means that most characters are only connected to a few other characters. If the Edge Density was equal to 1, which is its maximum possible value, it would mean that every  characters would have interacted with all the other characters, meaning that every vertex would be connected to all the other vertices. In the subgraph, we have removed characters with less than 9 interactions. In this way, we have kept the more popular characters, which are more likely to be connected with other characters. That's why our subgraph has a considerably larger edge density than the original network (0.117 vs 0.009).

# ----------
# QUESTION 4
# ----------

# BETWEENNESS: Meaning that these characters act as "bridges" for networks of other characters who are connected to each other through them.
head(sort(betweenness(ig, directed = FALSE), decreasing=TRUE), 15) # Top 10 characters of the network based on Betweenness Centrality

# CLOSENESS: Meaning that these characters need to traverse a smaller number of vertices on average to reach all other vertices of the network
head(sort(closeness(ig), decreasing=TRUE), 15) # Top 10 characters of the network based on Closeness Centrality

# WHERE DOES JON SNOW RANK IN THESE 2 METRICS ?
betweenness(ig, v="Jon-Snow", directed=FALSE)
closeness(ig, v="Jon-Snow")

# What's the ranking of Jon Snow in terms of Betweenness Centrality?
which(names(sort(betweenness(ig, directed = FALSE), decreasing=TRUE)) == "Jon-Snow")

# What's the ranking of Jon Snow in terms of Closeness Centrality?
which(names(sort(closeness(ig), decreasing=TRUE)) == "Jon-Snow")

# ----------
# QUESTION 5
# ----------

pgrnk <- page_rank(ig, algo="prpack" , directed=FALSE)$vector

# Here are all the characters ranked in descending order, based on their Pagerank value
ranked <- sort(pgrnk, decreasing=TRUE) 
ranked

head(ranked, 15) # Top 15 characters of the network based on the Pagerank algorithm

nodesize=pgrnk*300
plot.igraph(ig,vertex.label=NA,layout=layout.fruchterman.reingold,vertex.size=nodesize)

plot(ig, vertex.label = NA, layout=layout_nicely, vertex.size=nodesize)
plot(ig, vertex.label = NA, layout=layout_on_sphere, vertex.size=nodesize)
rglplot(ig, vertex.label = NA, layout=layout_nicely, vertex.size=nodesize, vertex.color="blue")
rglplot(ig, vertex.label = NA, layout=layout_on_sphere, vertex.size=nodesize, vertex.color="blue")

# CREATING A SUBGRAPH TO GET A MORE CLEAR PICTURE

vrt<-which(degree(ig) > 40)
ng <- induced_subgraph(ig, vrt)
pgrnk <- page_rank(ng, algo="prpack" , directed=FALSE)$vector
nodesize=pgrnk*300

plot(ng, layout=layout_nicely, vertex.size=nodesize)
plot(ng, layout=layout_on_sphere, vertex.size=nodesize)
rglplot(ng, layout=layout_nicely, vertex.size=nodesize, vertex.color="orange")
rglplot(ng, layout=layout_on_sphere, vertex.size=nodesize, vertex.color="orange", vertex.label.color="black")
