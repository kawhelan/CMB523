####CESA polytomy tree###

#load necessary libraries to create the tree 

#these libraries are used for data manipulation
library (tidyverse)
library(glue)
#this library allows us to read in tree objects
library (treeio)
#these libraries are used in plotting
library (ggtree)
library (ggrepel)
library (phangorn)

#Read in your RAXML output file: should be the bipartitions file
file <- "raxml_outputs/RAxML_bipartitions.CESA_Aligned.output"
tree <-read.tree(file)
tree

#reroot and collapse unsupported branches
#see https://www.mail-archive.com/r-sig-phylo@r-project.org/msg05580.html
tree <- root(tree, outgroup = "Penium_pm009480g0010", resolve.root = TRUE, edgelabel = TRUE)
target <- which(as.numeric(tree$node.label) < 70) #position is list of low bootstrap
nodes <- tree$edge[, 2] #nodes listed in order 
target <- target + length(tree$tip.label) #correct position given tips weren't included in node labels
i <- match(target, nodes) #find the nodes (number) based on the position is list id'd in target

tree2 <- tree #copied tree to collapse branches and leave original tree intact
tree2$edge.length[i] <- 0 #make branches 0

#Read in a metadata file containing information on species names
metadatafile <- "Species_labels_edited.csv"
metadata <-read.csv(metadatafile, header=TRUE, sep=",")
metadata

#transform the tree object into a datatype called a tibble that will allow you to manipulate 
#variables in the tree object

x <- as_tibble(tree2)
#transform metadata object into a tibble
y <-select(as_tibble(metadata), -node)


z <-left_join(x,y, by ="label")
z$lab <- paste0(z$species, z$CesA) #need Pp labels to include cesa #

tree2$tip.label <- z$lab[1:length(tree2$tip.label)] #get just tip labels (not nodes)

#Use ggtree to create a figure from tree1 and d2, with branches colored by species
ggtree(tree2, aes(color=z$species)) + 
  geom_treescale(x=2, y=0) + #scale the figure
  geom_tiplab(aes(label=z$lab), hjust=-0.2) +  #add tip labels and adjust position
  #  geom_label(aes(label=bootstrap), size=3.5) + #add bootstrap labels to nodes
  xlim(0,3) + #define plot limits
  theme(legend.position='none') #remove the legend

ggsave("CESAtree1_polytomy.png", width = 8, height = 8)




