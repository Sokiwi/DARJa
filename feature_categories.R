# load data
load("linguistic_distance_matrix4.RData")  # m
load("geographical_distance_matrix3.RData")  # mgeo
load("darja_data3.RData")  # d 

# cluster features to check if different categories of features  work similarly
# in terms of how they partition the locations

# first clean away all cases where a feature has more than one value 
# for a location from d
check <- paste(d$id, d$f, sep="_")
dup <- duplicated(check) | duplicated(check, fromLast = TRUE)
dup_inv <- !dup
d2 <- d[dup_inv,]

# function for computing the ARI for two features
library(pdfCluster)  # adj.rand.index
mari.feas <- function(F1, F2) {
  w_F1 <- which(d2$f==F1)
  w_F2 <- which(d2$f==F2)
  F1_data <- data.frame(d2$id[w_F1], d2$fv[w_F1])
  F2_data <- data.frame(d2$id[w_F2], d2$fv[w_F2])
  names(F1_data) <- names(F2_data) <- c("id", "fv")
  int <- intersect(F1_data$id, F2_data$id)
  if (length(int) == 0) {
    return(NA)
  } else {
    w_int_F1 <- match(int, F1_data$id)
    w_int_F2 <- match(int, F2_data$id)
    part_F1 <- F1_data$fv[w_int_F1]
    part_F2 <- F2_data$fv[w_int_F2]
    ari <- adj.rand.index(part_F1, part_F2)
    # convert to a scale from 0 to 1 and then convert to a distance
    ari_conv <- (ari + 1)/2
    ari_conv2 <- 1 - ari_conv
    return(ari_conv)
  }
}


# create empty matrix of features
feas <- sort(unique(d2$f))
total_feas <- length(feas)
mf <- matrix(NA, nrow=total_feas, ncol=total_feas)
diag(mf) <- 0  # same features should have a distance of 0
rownames(mf) <- colnames(mf) <- feas
pairsf <- (total_feas * (total_feas-1))/2

# run pairwise distances and fill the matrix
# takes a couple of minutes
count <- 0
for (i in 1:(total_feas-1)) {
  for (j in (i+1):total_feas) {
    count <- count + 1
    mf[i,j] <- mf[j,i] <- mari.feas(feas[i], feas[j])
    if ( count %% 100 == 0) {
      cat("doing", count, "out of", pairsf, "\n")
    }
  }
} 

# optionally save the matrix
# save(mf, file="feature_distance_matrix.RData")
# load("feature_distance_matrix.RData")

# remove NAs from mf by recursively removing the feature with
# most NAs (in its columns, but this is the same as in its rows)
# until no NAs are left
# this will leave 284 features; removing all rows and columns with an
# NA in them in one go would only have left 110 features
mfr <- mf  # reduced version of mf
NAs <- 1 # the 1 just some positive number for initiating the while loop
while(NAs > 0) {
  rs <- rowSums(is.na(mfr))
  mx <- which(rs==max(rs))[1]
  print(names(mx))
  w_max <- unname(mx)
  mfr <- mfr[-w_max,-w_max]
  rs <- rowSums(is.na(mfr))
  NAs <- sum(rs)
}

# Produce classical MDS in order to see if there are different clusterings
# of lexical, phonetic, morphological, and syntactic features

# Prepare MDS plot
ds <- mfr
fit <- cmdscale(mfr,eig=TRUE, k=2) # k is the number of dim
x <- fit$points[,1]
y <- fit$points[,2]
# color the features according to categories
rn <- row.names(mfr)
w_L <- grep("L", rn)
w_P <- grep("P", rn)
w_M <- grep("M", rn)
w_S <- grep("S", rn)
color_vector <- vector("character", length=length(rn))
color_vector[w_L] <- "red"
color_vector[w_P] <- "green"
color_vector[w_M] <- "blue"
color_vector[w_S] <- "black"
srn <- sapply(rn, function(z) strsplit(z, "_")[[1]][2])

# MDS plot 
plot(x, y, xlab="Dimension 1", ylab="Dimension 2",
     main="", type="n")
text(x, y, labels = srn, cex=.5, col=color_vector)
segments(x0=-0.07,y0=-0.1,x1=-0.07,y1=0.1)
segments(x0=-0.07,y0=0.1,x1=0.11,y1=0.1)
segments(x0=0.11,y0=0.1,x1=0.11,y1=-0.1)
segments(x0=0.11,y0=-0.1,x1=-0.07,y1=-0.1)

# zooming in on the dense cloud
plot(x, y, xlab="Dimension 1", ylab="Dimension 2",
     main="", type="n", xlim=c(-0.07,0.11), ylim=c(-0.1,0.1))
text(x, y, labels = srn, cex=.5, col=color_vector)
