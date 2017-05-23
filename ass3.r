folder = 'C:/Users/Alon/Desktop/Alon/School/4th year/SemesterB/Data science/ex3'
setwd(folder)
#install.packages('igraph')
library(igraph)
ga.data <- read.csv('ga_edgelist.csv', header = T)
#first question- A
g <- graph.data.frame(ga.data,directed = F)
degr.score <- degree(g)
V(g)$size <- degr.score * 2 # multiply by 2 for scale 
plot(g) 
dg <- decompose.graph(g)
plot(dg[[1]])
vec<-betweenness(dg[[1]])
maxB <- as.numeric(which(max(vec) == vec))
vec<-closeness(dg[[1]])
maxC <- as.numeric(which(max(vec) == vec))
evcent <- evcent(dg[[1]])
vec <- evcent$vector
me <- as.numeric(which(max(vec) == vec))
V(dg[[1]])[me]
#first question - B
#First algorithem- walktrap
community <- walktrap.community(g)
plot(g,  vertex.size=5, vertex.color=community$membership, asp=FALSE)
sizes(community)
modularity(community)
#second algorithem edge-betweenness
community <- edge.betweenness.community(g)
plot(g, vertex.size=5, vertex.color=community$membership, asp=FALSE)
sizes(community)
modularity(community)
