library(sp)  # point.in.polygon()
library(sf)  # read_sf()

# read in biome data from Olson & Dinerstein (1998) 
# requires downloaded file: 
# https://files.worldwildlife.org/wwfcmsprod/files/Publication/file/6kcchn7e3u_official_teow.zip 
# found at: https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world#:~:text=There%20are%20867%20terrestrial%20ecoregions,conserve%20biodiversity%20around%20the%20world.
shape <- read_sf(dsn = "official_teow/official", 
                 layer = "wwf_terr_ecos")

# create polygon for European Russia to check which polygons in 
# the shape file overlap with it and thus are relevant 
eur_rus <- matrix(c(27.13, 49.83, 48.51, 49.83, 48.51, 61.67, 27.13, 61.67, 
                    27.13, 49.83), ncol = 2, byrow = TRUE)
erp <- st_polygon(list(eur_rus))
sf_erp <- st_sf(geometry = st_sfc(erp), crs = 4326)

# go through the entire shape file and find the polygons that overlap
# with the DARJa area
# put their indices in a vector called 'relevant'
relevant <- c()
count <- 0
for (i in 1:length(shape$geometry)) {
  ans <- st_intersects(shape$geometry[i][[1]], sf_erp)[[1]]
  if ( length(ans) > 0 ) {
    count <- count + 1
    relevant[count] <- i
    cat("found one relevant\n")
  }
}

# collect information on the polygons
eco_name <- c()
biome_code <- c()
G200_region <- c()
G200_biome <- c()

for (i in 1:length(relevant)) {
  eco_name[i] <- shape[relevant[i],]$ECO_NAME
  biome_code[i] <- shape[relevant[i],]$BIOME
  G200_region <- shape[relevant[i],]$G200_REGIO
  G200_biome <- shape[relevant[i],]$G200_BIOME
}

# output some basic information to a file
basic_info <- data.frame(eco_name, biome_code, G200_region, G200_biome)
write.table(basic_info, file="biomes_eur_russia_info.txt", sep="\t", quote=FALSE, row.names = FALSE)

# reduce the shape file to just the relevant entries
sr <- shape[relevant,]  # shape relevant

load("darja_data3.RData")  # object called d 
load("linguistic_distance_matrix4.RData")  # object called m
load("geographical_distance_matrix3.RData")  # mgeo
diag(mgeo) <- rep(0, nrow(mgeo))

# make a data frame with location IDs and coordinates for all locations, using d
u <- unique(d$id)
locs <- d[match(u,d$id),c("id","lon","lat")]
L <- nrow(locs)
b_code <- rep(0,L)

# function for checking which biome a given point pertains to
check_point <- function(lon, lat) {
  point <- c(lon, lat)
  point_sf <- st_sf(geom = st_sfc(st_point(point), crs = 4326))
  for (j in 1:nrow(sr)) {
    res <- st_contains(sr[j,]$geometry, point_sf)[[1]]
    if (length(res) > 0) {
      return(sr[j,]$BIOME)
    }
  }
  return(NA)
}

# prepare a column to be added to the locs data frame frame with 
# a number representing the biome of each location, using the check_point()
# function; takes some 15 mins
for (i in 1:L) {
  lon <- as.numeric(locs$lon[i])
  lat <- as.numeric(locs$lat[i])
  if (i %% 100 == 0) {
    cat("doing", i, "out of", L, "\n")
  }
  b_code[i] <- check_point(lon, lat)
}

# add the column to locs
locs <- data.frame(locs, b_code)

# for each pair of locations check whether biomes are same or different
# and output to file; 0 means "same", 1 means "different"
IDs <- rownames(mgeo)
L <- length(IDs)
all <- L*(L-1)/2
count <- 0
cat("id1\tid2\tgeodist\tlingdist\tbiome_difference\n", file="biomes3.txt")
for (i in 1:(L-1)) {
  for (j in (i+1):L) {
    count <- count + 1
    if (count %% 1000 == 0) {
      cat("doing", count, "out of", all, "\n")
    }
    geodist <- mgeo[IDs[i],IDs[j]]
    lingdist <- m[IDs[i],IDs[j]]
    w_ID_i <- which(locs$id==IDs[i])
    w_ID_j <- which(locs$id==IDs[j])
    biome_i <- locs$b_code[w_ID_i]
    biome_j <- locs$b_code[w_ID_j]
    if (biome_i==biome_j) {
      biome_difference <- 0
    } else {
      biome_difference <- 1
    }
    cat(IDs[i], "\t", IDs[j], "\t", geodist, "\t", lingdist, "\t", biome_difference, "\n",
        file="biomes3.txt", append=TRUE)
  }
}

