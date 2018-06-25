# Network Analysis

# Create networks
#install.packages("igraph",repos = "https://cran.rstudio.com/")
library(igraph)

#####------------***********--------------#########
network.data = read.csv("Final.csv", header=T, as.is=T)

attach(network.data)
net.data =network.data[c('PO_Type',	'Vendor','VendorID',
                    'INV_PYMT_TRMS',	'Diff_Inv_Gross_Calc_Amt',	
                    'SubmitBy.PORelease',	'CreatedBy.PORelease',
                    'UpdatedBy.PORelease',	'AssignedEngineer',
                    'IncAmt.Yes_No.',	'NewCompltnDate.Y_N.',
                    'ChangeOfScope',	'SubmitBy.PCR',	'CreatedBy.PCR',
                    'UpdatedBy.PCR','EngineerName')]

attach(net.data)
#links.eng = aggregate(AssignedEngineer,net.data['Vendor'],count)
#links.eng = links.eng[order(AssignedEngineer),]
#install.packages("tidyverse", repos = "https://cran.rstudio.com/")
library(tidyverse)

# ========= *********** USAGE************** =================
#------******GROUPING VENDORS BY DIFFERENT PARAMETERS******---------#
#--------------------------------------------------------------------------
# Grouping Vendors By CreatedBy.PORelease - 
links.POCreatedBy = group_by(net.data['Vendor'],
                             as.vector(CreatedBy.PORelease))
#,drop = T)
links.POCreatedBy = data.frame(links.POCreatedBy) 
# dataframe required for graph object

# Grouping Vendors By SubmitBy.PORelease
links.POSubmitBy = group_by(net.data['Vendor'],as.vector(POSubmitByName))
#,drop = T)
links.POSubmitBy = data.frame(links.POSubmitBy)


# Grouping Vendors By EngineerName
links.eng = group_by(net.data['Vendor'],as.vector(EngineerName))
                       #,drop = T)
links.eng = data.frame(links.eng)

# Grouping Vendors By NewCompltnDate.Y_N.
links.new.date = group_by(net.data['Vendor'],as.vector(NewCompltnDate.Y_N.))
#,drop = T) 
links.new.date = data.frame(links.new.date)

# Grouping Vendors By IncAmt.Yes_No.
links.inc.amt = group_by(net.data['Vendor'],as.vector(IncAmt.Yes_No.))
#,drop = T)
links.inc.amt = data.frame(links.inc.amt)

# Grouping Vendors By Invoice Payment Terms.
links.inv.terms = group_by(net.data['Vendor'],as.vector(INV_PYMT_TRMS))
#,drop = T)
links.inv.terms = data.frame(links.inv.terms)

# ==== USAGE ==========================================
#------****** CREATING GRAPH OBJECTS******---------#
#--------------------------------------------------------------------------
# Create a Graph Object for CreatedBy.PORelease
fraud.net.POCreate = graph_from_data_frame(d=links.POCreatedBy,directed=T)
fraud.net.POCreate = igraph::simplify(fraud.net.POCreate
                              ,remove.multiple = T,remove.loops = T)

# Create a Graph Object for SubmitBy.PORelease
fraud.net.POSubmit = graph_from_data_frame(d=links.POSubmitBy,directed=T)
fraud.net.POSubmit = igraph::simplify(fraud.net.POSubmit,remove.multiple = T,
                         remove.loops = T)

# Create a Graph Object for AssignedEngineer
fraud.net.eng = graph_from_data_frame(d=links.eng,directed=T)
fraud.net.eng = igraph::simplify(fraud.net.eng,remove.multiple = T,
                         remove.loops = T)

# Create a Graph Object for NewCompltnDate.Y_N.
fraud.net.newdate = graph_from_data_frame(d=links.new.date,directed=T)
fraud.net.newdate = igraph::simplify(fraud.net.newdate,remove.multiple = T,
                              remove.loops = T)

# Create a Graph Object for IncAmt.Yes_No.
fraud.net.incamt = graph_from_data_frame(d=links.inc.amt,directed=T)
fraud.net.incamt = igraph::simplify(fraud.net.incamt,remove.multiple = T,
                         remove.loops = T)

# Create a Graph Object for Inv Payment Terms.
fraud.net.invterms = graph_from_data_frame(d=links.inv.terms,directed=T)
fraud.net.invterms = igraph::simplify(fraud.net.invterms,remove.multiple = T,
                                    remove.loops = T)
# =================== USAGE ===========================

# Create Network Diagrams for All Different parameters
set.seed(1234) # Set seed to repeat results

plot(fraud.net.POCreate,edge.width = 1,edge.arrow.size=.4,
     edge.arrow.width = 0.3,vertex.size = 5,
     vertex.size2 = 3,
     vertex.label.cex = 0.75,
     asp = 0.75,
     margin = -0.22)

vertex.colour = vertex.attributes(fraud.net.incamt,index = V(fraud.net.incamt))
plot(fraud.net.POSubmit,edge.width = 1,edge.arrow.size=.4,
     edge.arrow.width = 0.3,vertex.size = 5,
     vertex.size2 = 3,
     vertex.label.cex = 0.75,
     asp = 0.75,
     margin = -0.22)

plot(fraud.net.eng,edge.width = 1,edge.arrow.size=.4,
     edge.arrow.width = 0.3,vertex.size = 5,
     vertex.size2 = 3,
     vertex.label.cex = 0.75,
     asp = 0.75,
     margin = -0.20)

plot(fraud.net.newdate,edge.width = 1,edge.arrow.size=.4,
     edge.arrow.width = 0.3,vertex.size = 5,
     vertex.size2 = 3,
     vertex.label.cex = 0.75,
     asp = 0.75,
     margin = -0.22)

plot(fraud.net.incamt,edge.width = 1,edge.arrow.size=.4,
     edge.arrow.width = 0.3,vertex.size = 5,
     vertex.size2 = 3,
     vertex.label.cex = 0.75,
     asp = 0.75,
     margin = -0.20)

plot(fraud.net.invterms,edge.width = 1,edge.arrow.size=.4,
     edge.arrow.width = 0.3,vertex.size = 5,
     vertex.size2 = 3,
     vertex.label.cex = 0.75,
     asp = 0.75,
     margin = -0.22)


plot(fraud.net.all,edge.width = 1,edge.arrow.size=.4,
     edge.arrow.width = 0.3,vertex.size = 5,
     vertex.size2 = 3,
     vertex.label.cex = 0.75,
     asp = 0.75,
     margin = -0.22)

#------------- EXPERIMENT WITH DIFFERENT PLOTS TO FIND BEST PLOT -------------------

#Fruchterman.Reingold Layout
layout2 = layout.fruchterman.reingold(fraud.net.eng)
plot(fraud.net.eng, layout=layout2)

#Traditional Plot # THIS IS THE BEST PLOT SO FAR!!!!!!!!!!!!!
# plot(multi.net.links,edge.width = 1,edge.arrow.size=.4, # attempt ####
#      edge.arrow.width = 0.3,vertex.size = 5,
#      vertex.size2 = 3,
#      vertex.label.cex = 0.75,
#      asp = 0.75,
#      margin = -0.1) # Doesnt work #===============
#,vertex.label=unique(EngineerName))


plot(fraud.net.eng,edge.width = 1,edge.arrow.size=.4,
     edge.arrow.width = 0.3,vertex.size = 5,
     vertex.size2 = 3,
     vertex.label.cex = 0.75,
     asp = 0.75,
     margin = -0.1)
     #,vertex.label=unique(EngineerName))

plot( fraud.net.eng, #layout = layout_as_tree(fraud.net.eng, 
                                             #root = numeric(),
                                             #circular = F,rootlevel = numeric(),
                                             #mode = "out", flip.y = TRUE),
      layout =layout.reingold.tilford,
      edge.width = 1,
      edge.arrow.width = 0.3,
      vertex.size = 5,
      edge.arrow.size = 0.5,
      vertex.size2 = 3,
      vertex.label.cex = 0.75,
      asp = 0.75,
      margin = -0.1) 
