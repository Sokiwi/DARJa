load("darja_data3.RData")  # object called d

# as with the linguistic distances, prepare the computation putting the
# basis for computations (here coordinates) in two lists
Gx <- list()
Gy <- list()
u <- unique(d$id)

total_locs <- length(u)
for (i in 1:total_locs) {
  Gx[[i]] <- as.numeric(d[which(d$id==u[i])[1],"lon"])
  Gy[[i]] <- as.numeric(d[which(d$id==u[i])[1],"lat"])
  if ( i %% 100 == 0 ) {
    cat("doing", i, "out of", total_locs, "\n")
  }
}

# make empty matrix
mgeo <- matrix(NA, nrow=total_locs, ncol=total_locs, dimnames=list(u,u))
total <- (total_locs * (total_locs-1))/2

library(geosphere)
# function for computing the Haversine Great Circle Distance 
# (in km) given two sets of coordinates
geo_dist <- function(X1, Y1, X2, Y2) {
  dh <- distHaversine(c(X1, Y1),c(X2, Y2))
  return(round(dh/1000, 0))
}

# now compute the geographical distances
# takes maybe 1/2 hour
count <- 0
for (i in 1:(4192)) {
  for (j in (i+1):4193) {
    count <- count + 1
    mgeo[i,j] <- mgeo[j,i] <- geo_dist(Gx[[i]], Gy[[i]], Gx[[j]], Gy[[j]])
    if ( count %% 100 == 0) {
      cat("doing", count, "out of", total, "\n")
    }
  }
} 

save(mgeo, file="geographical_distance_matrix3.RData")
# next time mgeo is needed don't run the loop, just do
## FROM HERE
load("geographical_distance_matrix3.RData")

