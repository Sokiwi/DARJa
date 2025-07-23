load("darja_data3.RData")  # object called d
load("linguistic_distance_matrix4.RData")  # object called m
unique_ids <- unique(d$id)
total_locs <- length(unique(d$id))

# compute trees for all the dialects using all standard methods;
# however, also tried was single, WPGMC, UPGMC, but they do not 
# produce useful results
WardD <- hclust(as.dist(m), method = "ward.D")
WardD2 <- hclust(as.dist(m), method = "ward.D2")
complete <- hclust(as.dist(m), method = "complete")
UPGMA <- hclust(as.dist(m), method = "average")
WPGMA <- hclust(as.dist(m), method = "mcquitty")

# trees are saved for future use
save(WardD, file="WardD.RData")
save(WardD2, file="WardD2.RData")
save(complete, file="complete.RData")
save(UPGMA, file="UPGMA.RData")
save(WPGMA, file="WPGMA.RData")

# in order to use a method like cutree() a tree has to be ultrametric,
# excluding methods like Neighbor-Joining
# initial experiments were carried out to see if it might work to 
# force an NJ tree to be ultrametric, but they were without immediate success
# force.ultrametric() of phytools and 
# extend_tips_to_ultrametricize() of BioGeoBEARS were among the tools tried out
