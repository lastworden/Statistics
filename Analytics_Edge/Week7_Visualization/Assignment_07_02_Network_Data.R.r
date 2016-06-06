
edges = read.csv("edges.csv")

users = read.csv("users.csv")

str(users)

str(edges)

summary(users)

292/59

table(users$school,users$locale)

table(users$school,users$gender)

library(igraph)

?graph.data.frame

g = graph.data.frame(edges, FALSE, users) 

str(g)

plot(g, vertex.size=5, vertex.label=NA)

sort(degree(g),decreasing = TRUE)

V(g)$size = degree(g)/2+2

summary(V(g)$size)

plot(g,vertex.label = NA)

V(g)$color = "black"

V(g)$color[]

V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"

plot(g, vertex.label =NA)

V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"

plot(g, vertex.label =NA)

V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"

plot(g, vertex.label =NA, edge.width=3)

?igraph.plotting

?plot.igraph


