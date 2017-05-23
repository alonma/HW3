folder = 'C:/Users/Alon/Desktop/Alon/School/4th year/SemesterB/Data science/ex3'
setwd(folder)
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
rawTweets <- userTimeline("barackobama", n=10)
df = do.call("rbind",lapply(rawTweets,as.data.frame))
myCorpus = Corpus(VectorSource(df$text))
myCorpus <- map(myCorpus, content_transformer(tolower))
myCorpus <- map(myCorpus, content_transformer(removePunctuation))
myCorpus <- map(myCorpus, content_transformer(removeNumbers))
myCorpus = map(myCorpus, removeWords, stopwords("english"))
terms <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
terms <- as.matrix(tdm)
# change it to a Boolean matrix
terms[terms>=1] <- 1
#transform into a term-term adjacency matrix
termMatrix <- terms %*% t(terms)

g <- graph.adjacency(termMatrix, weighted=T, mode = "undirected")
# remove loops
summary(g)
g <- simplify(g)

summary(g)
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
community <- walktrap.community(g)
plot(g,  vertex.size=5, vertex.color=community$membership, asp=FALSE)
sizes(community)
modularity(community)
community <- edge.betweenness.community(g)
plot(g,  vertex.size=5, vertex.color=community$membership, asp=FALSE)
sizes(community)
modularity(community)