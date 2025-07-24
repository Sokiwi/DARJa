##########################################
## Preparations for cluster validation
##########################################

# load object called d, which contains basic data on locations and features
load("darja_data3.RData")

# get a vector of IDs of locations
# note: the ids 628 and 2329 are missing from the data
unique_ids <- unique(d$id)

# get a vector of features
features <- unique(d$f)

# read in trees
load("UPGMA.RData")
load("WardD.RData")
load("WardD2.RData")
load("complete.RData")
load("WPGMA.RData")

##########################################
## Adjusted Rand index
##########################################

# the goal here is to see which number of clusters and which method
# best accounts for distributions in the data
library(pdfCluster)  # adj.rand.index()

# function for cutting the tree at k number of clusters and outputting a 
# vector where the numbers indicate membership of each location in a cluster
get.tree.groups <- function(k, tree) {
  groups <- cutree(tree, k = k)
  tree.groups <- as.vector(groups)
  return(tree.groups)
}

# function for getting a classification of all locations in the data,
# i.e. the ones in unique_ids, based on a given feature; NAs will be
# introduced when attestations are lacking
# any location for which more than one 
# value of a feature is attested is excluded
get.feature.groups <- function(feature) {
  d.red <- d[d$f==feature,]  # data reduced to where the feature is
  tab <- table(d.red$orig_id)
  tab.vals <- as.vector(tab)
  tab.names <- names(tab)
  repeated <- tab.names[which(tab.vals > 1)]
  to.delete <- which(d.red$orig_id %in% repeated)
  d.red2 <- d.red[-to.delete,]
  d.red3 <- d.red2[match(unique_ids,d.red2$id),]  # one row per location
  return(d.red3$fv)
}

# get mean adjusted Rand index (mari) across features for 
# a given number of clusters, k
mari.function <- function(k, tree, version) {
  tree.groups <- get.tree.groups(k, tree)
  aris <- c()
  for (i in 1:length(features)) {
    feature.groups <- get.feature.groups(features[i])
    aris[i] <- adj.rand.index(tree.groups, feature.groups)
  }
  return(mean(aris, na.rm=TRUE))
}

# function which will output ARI values for different numbers of clusters and
# clusterization methods
get_ARIs <- function() {
  no_clusters <- 2:10
  ARIs_WardD <- c()
  ARIs_WardD2 <- c()
  ARIs_complete <- c()
  ARIs_UPGMA <- c()
  ARIs_WPGMA <- c()
  ARIs <- list(ARIs_WardD, ARIs_WardD2, ARIs_complete, ARIs_UPGMA, ARIs_WPGMA)
  ks <- 2:10
  maris <- c()
  trees <- list(WardD, WardD2, complete, UPGMA, WPGMA)
  for (i in 1:5) {
    cat("doing method", i, "\n")
    for (j in 1:length(ks)) {
      maris[j] <- mari.function(ks[j], trees[[i]])
    }
    ARIs[[i]] <- round(maris, 5)
  }
  results <- data.frame(no_clusters, ARIs[[1]], ARIs[[2]], ARIs[[3]], ARIs[[4]], ARIs[[5]])
  names(results) <- c("no_clusters", "WardD", "WardD2", "complete", "UPGMA", "WPGMA")
  write.table(results, file="optimal_number_clusters2.txt", sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
}
# runs as get_ARIs()
# Optimal number: WardD: 5; WardD2: 4; complete: 4; UPGMA: 6; WPGMA: 4

## Optional
# if desired, for number of clusters from 2 to 4192 get the 
# mean adjusted Rand indices and plot results
# serves to show that the mari is near-monotonically decreasing 
# beyond the k=10, which was the max number of cluster investigated in
# the previous, shorter version of the same experiment
ks <- 2:4192
maris <- c()
for (i in 1:length(ks)) {
  # replace tree in the below with one of the following: 
  # WardD, WardD2, complete, UPGMA, WPGMA
  maris[i] <- mari.function(ks[i], WardD)
  cat("clusters:", ks[i], "   mari:", maris[i], "\n")
}

results <- cbind(ks[1:length(maris)], maris)
colnames(results) <- c("k", "mari")
write.table(results, file="fit_no_clusters_features.txt", 
            sep="\t", quote=FALSE, row.names=FALSE)

# produce a nice plot with mari for the first 2:39 k values
# with an inset plot showing all values
results <- read.table(file="fit_no_clusters_features.txt", quote="", header=TRUE)
dev.off()
par(fig = c(0,1,0,1))
plot(results$k[1:39], results$mari[1:39], type="b", pch=20, main="Fit of number of clusters",
     ylab="mean adjusted Rand index", 
     xlab=substitute(paste("number of clusters, ",italic("k"))), 
     ylim=c(0.05,0.13))
abline(v=5, col="darkgray", lty=2)
text(substitute(paste(italic("k"), "= 5")), x = 7, y = 0.065, col="darkgray")

par(fig = c(0.55, 0.95, 0.35, 0.95), new = T)  
plot(results$k, results$mari, type="b", pch=20, main="",
     ylab="", xlab="", ylim=c(0,0.13))
dev.off()

##########################################
## silhouette scores
##########################################

library(cluster)  # 
load("linguistic_distance_matrix4.RData")  #  m

WardD_vals <- c()
WardD2_vals <- c()
complete_vals <- c()
UPGMA_vals <- c()
WPGMA_vals <- c()

# using silhouette() from the cluster package
# custom function to get silhouette widths (should be maximized)
# for different trees and methods
get.sil <- function(tree, k) {
  x1 <- silhouette(cutree(tree, k), m)
  x2 <- summary(x1)
  aw <- round(x2$avg.width,4)
  return(aw)
}

all_vals <- list(WardD_vals, WardD2_vals, complete_vals, 
                 UPGMA_vals, WPGMA_vals)
trees <- list(WardD, WardD2, complete, UPGMA, WPGMA)

for (i in 1:length(trees)) {
  cat("doing method", i, "\n")
  all_vals[[i]][1] <- NA
  for (j in 2:101) {
    if (j %% 10 == 0) {
      cat("     k =",j,"out of 200\n")
    }
    all_vals[[i]][j] <- get.sil(trees[[i]], j)
  }
}

sil_res <- data.frame(all_vals[[1]], all_vals[[2]], 
      all_vals[[3]], all_vals[[4]], all_vals[[5]])
names(sil_res) <- c("WardD", "WardD2", "complete", "UPGMA", "WPGMA")
write.table(sil_res, file="optimal_number_clusters_sil.txt", sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE)
# for all trees 2 is the optimal number of clusters

##########################################
## Stability scores
##########################################

library(fpc)
# the following is run on the five different methods,
# trying from 2 to 8 clusters
# in all cases k = 2 is most stable, but there
# are differences between methods, where ward.D and
# ward.D2 generally perform best across k's
# stability score theoretically range from between 0 and 1, 
# where greater than .75 is considered stable (never occurs in the data)
cb <- clusterboot(m, B = 100, bootmethod="boot", clustermethod=disthclustCBI, 
                  method="mcquitty", k = 5, showplots=FALSE)
# available methods: "ward.D", "ward.D2", "complete", 
# "average" (UPGMA), "mcquitty" (WPGMA) 
# results can be summarized using the mean of the bootstrap means
mean(cb$bootmean)

##########################################
## qualitative observations on the performance of different methods
##########################################

# the following are some observations of cluster results as
# depicted using the custum dmap() function

# WardD
# for k=5 yields 5 relatively even-sized clusters, clear NW outlier, small Chukhloma
# 95 outliers
# would work

# WardD2; for k=5 yields 5 relatively even-sized clusters, 
# no NW outliers, small Chukhloma
# 74 outliers
# would not work, because of missing NW outliers

# complete
# for k=5, 3 clusters are relatively even-sized clusters, but the northern area large 
# and the southern on small 
# barely any NW outlier, small Chukhloma
# 54 outliers
# would not work, because of large N area, no NW cluster, mostly no Chukhloma

# UPGMA
# for k=5 yields 3 large and 2 very small clusters
# only yields outliers with minpts <= 4; then 42 outliers
# would not work because clusters are too uneven

# WPGMA
# for k=5 yields 5 relatively even-sized clusters, large Chukhloma, no NW outlier
# 82 outliers
# would not work because of missing NW cluster, and Chukhloma is maybe too large
