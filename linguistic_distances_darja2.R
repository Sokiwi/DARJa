load("darja_data3.RData")  # object called d

## compute linguistic distances
# computation giving equal weight to each feature value
# f means feature
# v means value
pair_dist2 <- function(vf1, vfv1, vf2, vfv2) {  # v means vector
  # function for individual features, for which a 
  # location may have more than one value
  feat_dist <- function(f1, f2) {
    1 - length(intersect(f1,f2))/length(union(f1,f2))
  }
  is <- intersect(vf1, vf2)
  L <- length(is)
  distances <- rep(NA, L)
  for (i in 1:length(is)) {
    f1 <- vfv1[which(vf1==is[i])]
    f2 <- vfv2[which(vf2==is[i])]
    distances[i] <- feat_dist(f1, f2)
  }
  return(round(sum(distances)/L,4))
}

# two lists of vectors of features and feature values are made,
# with one vector per location
# the vectors of features and feature values have the same length
# then pairwise distances are put in a matrix
# the lists has length 4193 and the location IDs are in unique_ids 

F <- list()
FV <- list()

total_locs <- length(unique(d$id))
u <- unique(d$id)

# takes about 45 secs
for (i in 1:total_locs) {
  F[[i]] <- d[which(d$id==u[i]),"f"]
  FV[[i]] <- d[which(d$id==u[i]),"fv"]
  if ( i %% 100 == 0 ) {
    cat("doing", i, "out of", total_locs, "\n")
  }
}

# create the empty matrix
m <- matrix(NA, nrow=total_locs, ncol=total_locs, dimnames=list(u,u))

# run pairwise distances and fill the matrix
# first time it is run it is saved 
pairs <- (total_locs * (total_locs-1))/2
count <- 0
for (i in 1:(length(F)-1)) {
  for (j in (i+1):length(F)) {
    count <- count + 1
    m[as.character(u[i]),as.character(u[j])] <- m[as.character(u[j]),as.character(u[i])] <- pair_dist2(F[[i]], FV[[i]], F[[j]], FV[[j]])
    if ( count %% 100 == 0) {
      cat("doing", count, "out of", pairs, "\n")
    }
  }
} 

# put zeros in the diagonal
diag(m) <- rep(0, nrow(m))

# save m
save(m, file="linguistic_distance_matrix4.RData")

# so next time m is needed just do
load("linguistic_distance_matrix4.RData")

## toy example
# first vector of features
vf1 <- c("A", "B", "B", "C", "D")
# first vector of feature values
vfv1 <- c("A_1", "B_1", "B_2", "C_1", "D_1")
# second vector of features
vf2 <- c("A", "B", "B", "C")
# second vector of feature values
vfv2 <- c("A_1", "B_1", "B_3", "C_2")
# they have 3 attested features in common (A, B, C), so the distance 
# should be normalized by 3; feature D is not relevant
# for feature A they have the same value, so distance here is 0
# for feature B they share 1 of 3 values, so the distance is 1 - 1/3 = 2/3
# for feature C they share 0 values, so the distance is 1
# total distance is the sum of individual distances divided by 3: 
# 0 + 2/3 + 1 = (5/3)/3 = 5/9 = 0.5556 (rounded to 4 decimal places)
# testing the function:
pair_dist2(vf1, vfv1, vf2, vfv2)  # gives 0.5556 as also computed by hand

