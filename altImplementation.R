library(igraph)
#####------------***********--------------#########
network.data = read.csv("Final.csv", header=T, as.is=T)
attach(network.data)
# net.data =network.data[c('Vendor',
#                          'INV_PYMT_TRMS',	'Diff_Inv_Gross_Calc_Amt',	
#                          'SubmitBy.PORelease',	'CreatedBy.PORelease',
#                          'UpdatedBy.PORelease',	'AssignedEngineer',
#                          'IncAmt.Yes_No.',	'NewCompltnDate.Y_N.',
#                          'ChangeOfScope',	'SubmitBy.PCR',	'CreatedBy.PCR',
#                          'UpdatedBy.PCR','EngineerName')]
#attach(net.data)

net.data =network.data[c('Vendor','INV_PYMT_TRMS',	
         'AssignedEngineer','IncAmt.Yes_No.',
         'NewCompltnDate.Y_N.')]


library(tidyverse)
# install.packages("proxy",repos = "https://cran.rstudio.com/")
library(proxy)
# ============Transform into an adjacency matrix =============
sim = function(x,y) sum(x==y) - sum(x==0 & y==0)

adj.net = as.matrix(dist(net.data,method=sim))

net.graph = graph.adjacency(adj.net,
                            weighted=T,mode="undirected")
# remove loops
net.graph = simplify(net.graph)
V(net.graph)$label = V(net.graph)$name
V(net.graph)$degree = degree(net.graph)

# ===== set seed to make the graph reproducible ===============
set.seed(54321)

# layout.1=layout.davidson.harel(net.graph)
plot(net.graph,layout=layout.kamada.kawai)

# ============= Community Detection ====================
cfg = cluster_fast_greedy(as.undirected(net.graph))

plot(cfg, as.undirected(net.graph),vertex.size=7,
     vertex.color = rainbow(5, .8, .8, alpha= .8),
     vertex.label.color = "black", vertex.label.cex = 0.8,
     vertex.label.degree = -pi/2,
     edge.arrow.size = 0.15, edge.arrow.width = 0.25,
     edge.color = "black",
     layout=layout.kamada.kawai)

plot(net.graph,edge.width = 1,edge.arrow.size=.4,
     edge.arrow.width = 0.3,vertex.size = 5,
     vertex.size2 = 3,
     vertex.label.cex = 1.10,
     asp = 0.05,
     margin = -0.2)

# ================== Best one ================================
set.seed(54321)
cfg = cluster_fast_greedy(as.undirected(net.graph))
plot(cfg,as.undirected(net.graph), vertex.size=7,
     #vertex.color = rainbow(5, .8, .8, alpha= .8),
     vertex.label.color = "black", vertex.label.cex = 0.6,
     vertex.label.degree = -pi/2,
     edge.arrow.size = 0.15, edge.arrow.width = 0.25,
     edge.color = "black",
     layout=layout.kamada.kawai) # Best one
