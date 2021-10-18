library(ape) # for reading in nexus files
library(RevGadgets)
library(coda)
library(ggplot2)
library(ggtree)
library(grid)
library(gridExtra)

setwd("/Users/neshcheret/Documents/GitHub/articles/stability/beast")

# read in the tree
mcct.tree<-read.nexus("stability_covarion_relaxed.mcct_renamed.trees")

# find out, which nodes are language families
#plot(mcct.tree, type = "phylogram",tip.color = "black",font=1,label.offset = 0.05,cex = 0.5)
#nodelabels()
# 107 Mongolic, 106 Koreanic, 96 Tungusic, 84 Japonic, 64 Turkic

setwd("/Users/neshcheret/Documents/GitHub/articles/stability")
pdf(file="mcct-tree.pdf", width = 6, height = 8)
plot(mcct.tree, type = "phylogram",tip.color = "black",font=1,label.offset = 0.05,cex = 0.5)
#tiplabels(col="black", bg=get_feature$V1,pch=21,cex = 1)
#nodelabels(node = c(64,107,96,106,84),pie = unlist(asr_matrix), piecol = co, cex = 0.5)
nodelabels(node = c(64,107,96,106,84),text = c("Turkic", "Mongolic", "Tungusic","Koreanic","Japonic"),adj = c(1.4, 0), cex = .75, bg = "white")
nodelabels(round(mcct.tree$edge.length, digits = 2))
dev.off()

tree <- readTrees("stability_covarion_relaxed.mcct_renamed.trees")
plot <- plotTree(tree = mcct.tree,
                 # label nodes the with posterior probabilities
                 node_labels = "posterior", 
                 # offset the node labels from the nodes
                 node_labels_offset = 0.005,
                 # make tree lines more narrow
                 line_width = 0.5,
                 # italicize tip labels 
                 tip_labels_italics = TRUE)



library(ggtree)
library(ips)
tree <- read.beast("stability_covarion_relaxed.mcct_renamed.trees") 

# format the numbers
tree@data['rposterior'] <- sprintf("%0.2f", as.numeric(tree@data[['posterior']])) 
tree@data['rposterior'][tree@data['rposterior'] == 'NA',] <- NA 

ggtree(tree, ladderize=TRUE, size=1.2) + 
  geom_tiplab(align=TRUE, linesize=0) + 
  geom_label( 
    aes(label=rposterior), label.size=0.3, na.rm=TRUE, size=2, 
    nudge_x=-0.4, nudge_y=0 
  ) + 
  theme_tree2() 


