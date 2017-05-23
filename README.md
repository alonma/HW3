# HW3

First Question:
At first, we wanted to build a graph based on the "Grey's anatomy" database and extracting the biggest component:

```{r}
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
```
![Alt text](https://github.com/alonma/HW3/blob/master/g1.JPG "Biggest component")

Now we can calcualte the needed factors.
1. betweenes - Sloan with 115.3667
2. closeness - tores with 0.01754
3. Eigencetor - Karev with 1/24 

Now we can start analysing comunities, first of all we'll use the walktrap algorithm.

```{r}
community <- walktrap.community(g)
plot(g, layout=default_layout, vertex.size=5, vertex.color=community$membership, asp=FALSE)
sizes(community)
modularity(community)
```
The results that we got:

| Community | 1 |  2 | 3 | 4 | 5 | 6 | 7 |
|:---------:|:-:|:--:|:-:|:-:|:-:|:-:|---|
|   Size    | 5 | 13 | 3 | 3 | 2 | 3 | 3 |

Modularity value :  0.5147059

![Alt text](https://github.com/alonma/HW3/blob/master/g2.JPG "WalkTrap")

Second algorithm is edge-betweenness

```{r}
community <- edge.betweenness.community(g)
plot(g, layout=default_layout, vertex.size=5, vertex.color=community$membership, asp=FALSE)
sizes(community)
modularity(community)
```
The results that we got:

| Community | 1 |  2 | 3 | 4 | 5 | 6 | 7 |
|:---------:|:-:|:--:|:-:|:-:|:-:|:-:|---|
|   Size    | 8 | 5 | 4 | 4 | 5 | 3 | 3 |

Modularity value :  0.5774221

![Alt text](https://github.com/alonma/HW3/blob/master/g3.JPG "edge-betweenness")
