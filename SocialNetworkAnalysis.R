library(igraph)
library(readr)

actors <- read_csv("https://raw.githubusercontent.com/OPER682-Tucker/Social-Network-Analysis/master/Actors.csv")
movies <- read_csv("https://raw.githubusercontent.com/OPER682-Tucker/Social-Network-Analysis/master/Movies.csv")

actors
movies

#graphing directional relationships with list of actors in various movies
actor.network <- graph_from_data_frame(d = movies, vertices = actors, directed = F)
plot(actor.network)

#color code the links between actors
E(actor.network)$color <- ifelse(E(actor.network)$Movie == "Forrest Gump", "green",
                          ifelse(E(actor.network)$Movie == "Apollo 13", "black",
                                 "orange"))
plot(actor.network)

#color code the nodes based on characteristics of actors
V(actor.network)$color <- ifelse(V(actor.network)$BestActorActress == "Winner", "gold",
                          ifelse(V(actor.network)$BestActorActress == "Nominated", "grey",
                                 "lightblue"))
plot(actor.network)

#creating a legend for the color coded graph
plot(actor.network, vertex.frame.color = "white")
legend("bottomright", c("Winner", "Nominee", "Not Nominated"), pch = 21,
       col = "#777777", pt.bg = c("gold", "grey", "lightblue"), pt.cex = 2, cex = .8)
legend("topleft", c("Forest Gump", "Apollo 13", "The Rock"),
       col = c("green", "black", "orange"), lty = 1, cex = .8)

#degree centrality to measure the number of connections for each node
degree(actor.network, mode = "all")

#closeness centrality to evaluate the proximity of the node
closeness(actor.network, mode = "all", weights = NA, normalized = T)

#betweeness centrality to measure the capacity of standing on paths that connect them
betweenness(actor.network, directed = F, weights = NA, normalized = T)

#Bacon's law - any two people on earth are six or fewer acquaintance links apart
#distances calculates the shortest paths between nodes
distances(actor.network, v = V(actor.network)["Kevin Bacon"], to = V(actor.network), weights = NA)

