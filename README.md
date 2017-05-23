# HW3
# Alon Maharshak and Miri Hazanov
## **First Question**:
### At first, we wanted to build a graph based on the "Grey's anatomy" database and extracting the biggest component:

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

### Now we can calcualte the needed factors.
1. betweenes - Sloan with 115.3667
2. closeness - tores with 0.01754
3. Eigencetor - Karev with 1/24 

### Now we can start analysing comunities, first of all we'll use the walktrap algorithm.

```{r}
community <- walktrap.community(g)
plot(g, layout=default_layout, vertex.size=5, vertex.color=community$membership, asp=FALSE)
sizes(community)
modularity(community)
```
### The results that we got:

| Community | 1 |  2 | 3 | 4 | 5 | 6 | 7 |
|:---------:|:-:|:--:|:-:|:-:|:-:|:-:|---|
|   Size    | 5 | 13 | 3 | 3 | 2 | 3 | 3 |

**Modularity value** :  0.5147059

![Alt text](https://github.com/alonma/HW3/blob/master/g2.JPG "WalkTrap")

### Second algorithm is edge-betweenness

```{r}
community <- edge.betweenness.community(g)
plot(g, layout=default_layout, vertex.size=5, vertex.color=community$membership, asp=FALSE)
sizes(community)
modularity(community)
```
### The results that we got:

| Community | 1 |  2 | 3 | 4 | 5 | 6 | 7 |
|:---------:|:-:|:--:|:-:|:-:|:-:|:-:|---|
|   Size    | 8 | 5 | 4 | 4 | 5 | 3 | 3 |

**Modularity value :  0.5774221**

![Alt text](https://github.com/alonma/HW3/blob/master/g3.JPG "edge-betweenness")



## **Second Question**:
### We decided to use the twitter API to get our data, we used the timeline function and with a bit of data preperation we were ready to create a graph:

First we needed to connect to the twitter API.

```{r}
library(twitteR)
library(httr)
library(jsonlite)
library(tm)
library(igraph)
library(wordcloud)
consumer_key <- "hgb9Ul8CmVJUnTorVdAs8Kgs0"
consumer_secret <- "5kemgD6GsyE5eGfmMSn2a0vsRc22p71ZVhCbqcmRAb4hfdrhDL"
access_token <- "857218734674595840-oSnoYNhJbYd451TMKcXHYf0wQkYmiCj"
access_secret <- "0GRPvnPdk6LmhoiXByKKjp9SeeGV2wx28aArWmh5szJhp"
myapp = oauth_app("twitter", key=consumer_key, secret=consumer_secret)
net <- setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
```

### Now we can get the data from twitter, and manipulate it.
we preforemd a series of action to get good data,
1. lower case
2. remove stop words
3. remove numbers
4. remove Punctuation
5. Remove URLS's

### After that we extracted the last 20 twitts of Roger Federer and turend them into Term-Doc adjacency metrix, after that we converted it into a graph.

```{r}
rawTweets <- userTimeline("federer_roger", n=20)
df = do.call("rbind",lapply(rawTweets,as.data.frame))
myCorpus = Corpus(VectorSource(df$text))
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus <- tm_map(myCorpus, content_transformer(removePunctuation))
myCorpus <- tm_map(myCorpus, content_transformer(removeNumbers))
myCorpus = tm_map(myCorpus, removeWords, stopwords("english"))
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))  #??
terms <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
terms <- as.matrix(terms)
terms[terms>=1] <- 1
termMatrix <- terms %*% t(terms)
g <- graph.adjacency(termMatrix, weighted=T, mode = "undirected")
g <- simplify(g)
summary(g)
plot(g)
```
![Alt text](https://github.com/alonma/HW3/blob/master/g4.JPG "Federer_tweets")

### Now we can repeat the assigments from question one on the new graph that we creted, that describing the connection between words in the tweets.

```{r}
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
community <- walktrap.community(g)
plot(g,  vertex.size=5, vertex.color=community$membership, asp=FALSE)
sizes(community)
modularity(community)
community <- edge.betweenness.community(g)
plot(g,  vertex.size=5, vertex.color=community$membership, asp=FALSE)
sizes(community)
modularity(community)
```


### The results that we got:

1. betweenes - Tennis with 459.66275946
2. closeness - amazing with 0.007462687
3. Eigencetor - tshirts with 1/74

### First algorithm is Walktrap
| Community |  1 | 2 |  3 |  4 | 5 | 6 | 7 | 8 | 9 |
|:---------:|:--:|:-:|:--:|:--:|:-:|:-:|---|---|---|
|    Size   | 38 | 8 | 10 | 13 | 3 | 4 | 5 | 6 | 3 |

**Modularity value** :  0.4631074

![Alt text](https://github.com/alonma/HW3/blob/master/g5.JPG "WalkTrap")

### Second algorithm is edge-betweenness

```{r}
community <- edge.betweenness.community(g)
plot(g, layout=default_layout, vertex.size=5, vertex.color=community$membership, asp=FALSE)
sizes(community)
modularity(community)
```
### The results that we got:

| Community | 1 | 2 |  3 |  4 | 5 | 6 | 7 | 8 | 9  | 10 | 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20|
|:---------:|:-:|:-:|:--:|:--:|:-:|:-:|---|---|----|----|----|----|----|----|----|----|----|----|----|---|
|    Size   | 3 | 8 | 3  | 18 | 4 | 2 | 8 | 3 | 4  | 5  | 2  | 6  | 1  | 3  | 13  | 1  | 1  | 1 | 1  | 3 |

**Modularity value** :  0.404236

![Alt text](https://github.com/alonma/HW3/blob/master/g6.JPG "edge-betweenness")
