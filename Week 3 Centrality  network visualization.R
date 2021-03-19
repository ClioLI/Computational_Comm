
# network centrality

install.packages("igraph")
library(igraph)


### load in the data 
# DATASET : edgelist & nodelist
nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T)
links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T)


# explore the data is the first step when you start your analysis

# Examine the data:
# view the first six lines of the data
head(nodes) 
head(links)
# number of the node, number of unqie node
nrow(nodes); length(unique(nodes$id))
# number of the edge, number of unqie edge (some have different types)
nrow(links); nrow(unique(links[,c("from", "to")]))


# Notice that there are more links than unique from-to combinations. 
# That means we have cases in the data where there are multiple links between the same two nodes. 
# We will collapse all links of the same type between the same two nodes by summing their weights, 
# using aggregate() by ¡°from¡±, ¡°to¡±, & ¡°type¡±
links <- aggregate(links[,3], links[,-3], sum)
links <- links[order(links$from, links$to),]
colnames(links)[4] <- "weight"
rownames(links) <- NULL


# create the graph
?graph.data.frame
net <- graph.data.frame(links, nodes, directed=T)

# Examine the resulting object:
class(net)
net 

# It's easy to access nodes, edges, and their attributes:
E(net)
V(net)
E(net)$type
V(net)$media

# Let's just simply plot the network
plot(net)




### CALCULATE CENTRALITY MEASURES 

# in degree
degree(net,mode='in')

# out degree
degree(net,mode='out')

# Closeness centrality
closeness(net,mode = "all")

# Betweenness centrality
betweenness(net)

# Eigenvector centrality 
eigen_centrality(net)$vector

# combine the centralities metrics with the origin node list
nodeList_centrality <- data.frame(nodes,in_degree=degree(net,mode='in'),out_degree=degree(net,mode='out'),
                                  Closeness_centrality=closeness(net,mode = "all"), 
                                  Betweenness_centrality=betweenness(net),
                                  Eigenvector_centrality=eigen_centrality(net)$vector )

View(nodeList_centrality)





### network visualization

# Let's just simply plot the network
plot(net)
# Removing loops from the graph:
net <- simplify(net, remove.multiple = F, remove.loops = T) 
#plot again
plot(net)


# Adjust the position of nodes and edges -- layout options
# two ways to grab a layout:
# 1.tkplot
# tkplot can help use to adjust the layout
tkid <- tkplot(net) #tkid is the id of the tkplot that will open
tkl <- tkplot.getcoords(tkid) # grab the coordinates from tkplot
plot(net, layout=tkl)
# 2.built in layout function
#set a layout, and all the following plot will use the same layout
l <- layout.fruchterman.reingold(net)
l <- tkl



# try to  adjust some of the plotting setting

# The labels are currently node IDs by defult.
plot(net,layout=l)
# Setting them to NA will render no labels:
plot(net,vertex.label=NA,layout=l)

# Let's and reduce the arrow size and remove the labels:
plot(net, edge.arrow.size=.4,vertex.label=NA,layout=l)

# Plot with curved edges (edge.curved=.1) and reduce arrow size:
plot(net, edge.arrow.size=.4, edge.curved=.1,vertex.label=NA,layout=l)


# Set node color to orange and the border color to hex #555555
# Replace the vertex label with the node names stored in "media"
plot(net, edge.arrow.size=.4, edge.curved=0.1,
     vertex.frame.color="#555555", vertex.label=V(net)$media,
     vertex.label.color="black", vertex.label.cex=1,layout=l) 

# compare the original plot and the plots after adjustments
par(mfrow=c(2,2))
plot(net,layout=l)
plot(net, edge.arrow.size=.1,vertex.label=NA,layout=l)
plot(net, edge.arrow.size=.1, edge.curved=.2,vertex.label=NA,layout=l)
plot(net, edge.arrow.size=.1, edge.curved=.2,
     vertex.frame.color="#555555", vertex.label=V(net)$media,
     vertex.label.color="black", vertex.label.cex=.7,layout=l) 
dev.off()


# Generate colors base on media type:
net # five attributes for vertex
colrs <- c("gray50", "tomato", "gold")
V(net)$color <- colrs[V(net)$media.type]
net # one more attribute
plot(net, edge.arrow.size=.4,vertex.label=V(net)$media,vertex.label.color="black",layout=l)

# Compute node degrees (#links) and use that to set node size:
par(mfrow=c(1,2))
deg <- degree(net, mode="all")
V(net)$size <- deg*3
plot(net, edge.arrow.size=.1, edge.curved=.2,vertex.label=V(net)$media,vertex.label.color="black",layout=l)
# We could also use the audience size value:
V(net)$size <- V(net)$audience.size*0.6
plot(net, edge.arrow.size=.1, edge.curved=.2,vertex.label=V(net)$media,vertex.label.color="black",layout=l)
dev.off()




# Set edge width based on weight:
E(net)$width <- E(net)$weight/6

#change arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"
plot(net,vertex.label=V(net)$media,vertex.label.color="black",layout=l) 

# We can also add a legend explaining the meaning of the colors we used:
legend(x=-1.5, y=-1.1, c("Newspaper","Television", "Online News"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)


# Sometimes, especially with semantic networks, we may be interested in 
# plotting only the labels of the nodes:
plot(net, vertex.shape="none", vertex.label=V(net)$media, 
     vertex.label.font=2, vertex.label.color="gray30",
     vertex.label.cex=V(net)$audience.size^0.5*0.3, edge.color="gray50",layout=l)


# Let's color the edges of the graph based on their source node color.
# We'll get the starting node for each edge with "get.edges"
edge.start <- get.edges(net, 1:ecount(net))[,1]
edge.col <- V(net)$color[edge.start]

plot(net, edge.color=edge.col, edge.curved=.1,vertex.label=NA,layout=l)


# We can identify the type and size of nodes, but cannot see much about the structure 
# since the links we¡¯re examining are so dense. One way to approach this is to see if
# we can sparsify the network, keeping only the most important ties and discarding the rest.
# There are more sophisticated ways to extract the key edges, but for the purposes of this 
# exercise we¡¯ll only keep ones that have weight higher than the mean for the network. 
# In igraph, we can delete edges using??delete.edges(net, edges): 

cut.off <- mean(links$weight) 
E(net)[weight<cut.off]
net.sp <- delete.edges(net, E(net)[weight<cut.off])
plot(net.sp,vertex.label=NA,layout=l) 


# Another way to think about this is to plot the two tie types 
# (hyperlik & mention) separately:
plot(net, edge.color=c("dark red", "slategrey")[(E(net)$type=="hyperlink")+1],
     vertex.label=V(net)$media,layout=l)

# Another way to delete edges:  
net.m <- net - E(net)[E(net)$type=="hyperlink"]
net.h <- net - E(net)[E(net)$type=="mention"]

# Plot the two links separately:
# Make sure the nodes stay in place in both plots:
par(mfrow=c(1,2))

plot(net.h, vertex.color="orange",vertex.label=V(net)$media, layout=l, main="Tie: Hyperlink")
plot(net.m, vertex.color="lightsteelblue2",vertex.label=V(net)$media, layout=l, main="Tie: Mention")

dev.off()









  
# Hubs and authorities
# help us to examinate the key node of a network  
  # The hubs and authorities algorithm developed by Jon Kleinberg was initially used 
  # to examine web pages. Hubs were expected to contain catalogues with a large number 
  # of outgoing links; while authorities would get many incoming links from hubs, 
  # presumably because of their high-quality relevant information. 

hs <- hub_score(net, weights=NA)$vector
as <- authority_score(net, weights=NA)$vector

par(mfrow=c(1,2))
plot(net, vertex.size=hs*50, main="Hubs",vertex.label=V(net)$media,vertex.label.color="black",layout=l)
plot(net, vertex.size=as*30, main="Authorities",vertex.label=V(net)$media,vertex.label.color="black",layout=l)
dev.off()






#  ------->> Communities --------

# A number of algorithms aim to detect groups that consist of densely connected nodes
# with fewer connections across groups. 

# Community detection based on edge betweenness (Newman-Girvan)
# High-betweenness edges are removed sequentially (recalculating at each step)
# and the best partitioning of the network is selected.
ceb <- cluster_edge_betweenness(net) 
dendPlot(ceb, mode="hclust")
plot(ceb,vertex.label=V(net)$media, net, edge.arrow.size=0.2 ,layout=l) 

# Let's examine the community detection igraph object:
class(ceb)
length(ceb)     # number of communities
membership(ceb) # community membership for each node
crossing(ceb, net)   # boolean vector: TRUE for edges across communities
modularity(ceb) # how modular the graph partitioning is

# remove the edges that across communities
cutted.edges <- which(crossing(ceb, net)==T)
net.ce <- net - E(net)[cutted.edges]
plot(ceb,vertex.label=V(net)$media, net.ce, edge.arrow.size=0.2 ,layout=l) 

# High modularity for a partitioning reflects dense connections within communities 
# and sparse connections across communities.



# Community detection based on greedy optimization of modularity
cfg <- cluster_fast_greedy(as.undirected(net))
dendPlot(cfg, mode="hclust")
plot(cfg, vertex.label=V(net)$media, as.undirected(net), layout=l)

# Let's examine the community detection igraph object:
class(cfg)
length(cfg)     # number of communities
membership(cfg) # community membership for each node
crossing(cfg, net)   # boolean vector: TRUE for edges across communities
modularity(cfg) # how modular the graph partitioning is

par(mfrow=c(2,2))

dendPlot(ceb, mode="hclust")
dendPlot(cfg, mode="hclust")

plot(ceb,vertex.label=V(net)$media,vertex.label.color="black", net, edge.arrow.size=0.1 ,layout=l) 
plot(cfg, vertex.label=V(net)$media,vertex.label.color="black", as.undirected(net), layout=l)

dev.off()


# We can also plot the communities without relying on their built-in plot:
V(net)$community <- cfg$membership
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
plot(net, vertex.label=V(net)$media,vertex.label.color="black",vertex.color=colrs[V(net)$community], layout=l)




