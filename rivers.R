# download geographical data from rnaturalearth if not already in memory
geodata <- c("world_s", "rivers_s", "lakes_s", 
             "world_m", "rivers_m", "lakes_m", 
             "world_l", "rivers_l", "lakes_l")

if (sum(geodata %in% ls()) < 9) {
  library(rnaturalearth)
  
  world_s <- ne_countries(scale = "small", returnclass = "sf")
  rivers_s <- ne_download(scale = "small", type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")
  lakes_s <- ne_download(scale = "small", type = "lakes", category = "physical", returnclass = "sf")
  
  world_m <- ne_countries(scale = "medium", returnclass = "sf")
  rivers_m <- ne_download(scale = "medium", type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")
  lakes_m <- ne_download(scale = "medium", type = "lakes", category = "physical", returnclass = "sf")
  
  world_l <- ne_countries(scale = "large", returnclass = "sf")
  rivers_l <- ne_download(scale = "large", type = "rivers_lake_centerlines", category = "physical", returnclass = "sf")
  lakes_l <- ne_download(scale = "large", type = "lakes", category = "physical", returnclass = "sf")
}

load("darja_data2.RData")  # object called d 
load("linguistic_distance_matrix4.RData")  # object called m
load("geographical_distance_matrix3.RData")  # mgeo

library(sf)

crc <- function(lon1, lat1, lon2, lat2, rivers_sf) {  # count_rivers_crossed
  # Check for degenerate line
  if (lon1 == lon2 && lat1 == lat2) {
    stop("The two points are identical; cannot form a valid line.")
  }
  # tweak the coordinates a little bit to avoid a crash in the special
  # situation where either longitudes or latitudes are identical
  if (lon1 == lon2) {
    lon1 <- lon1 + 0.00001
  }
  if (lat1 == lat2) {
    lat1 <- lat1 + 0.00001
  }
  # Temporarily disable S2 geometry engine
  old_s2_setting <- invisible(sf_use_s2(FALSE))
  
  invisible(on.exit(sf_use_s2(old_s2_setting)))  # restore original setting afterward
  
  # Construct line geometry
  coords <- matrix(c(lon1, lat1, lon2, lat2), ncol = 2, byrow = TRUE)
  line_sf <- st_sfc(st_linestring(coords), crs = 4326)
  
  # Transform CRS if necessary
  if (st_crs(rivers_sf) != st_crs(line_sf)) {
    line_sf <- st_transform(line_sf, st_crs(rivers_sf))
  }
  
  # Crop rivers to line bounding box
  rivers_crop <- suppressWarnings(st_crop(rivers_sf, st_bbox(line_sf)))
  
  # Find intersections
  crossed <- st_crosses(rivers_crop, line_sf, sparse = FALSE)
  num_crossed <- sum(crossed)
  
  return(num_crossed)
}

pairschecked <- 0
pairsused <- 0
L <- nrow(mgeo)  # same as nrow(m)
cat("id1\tid2\tgeodist\tlingdist\tnumber_rivers\n", file="rivers_m.txt")
for (i in 1:(L-1)) {
  for (j in (i+1):L) {
    if(pairschecked %% 100 == 0) {
      cat("checked", pairschecked, "pairs\n")
    }
    if(pairsused %% 100 == 0) {
      cat("used", pairsused, "pairs\n")
    }
    pairschecked <- pairschecked + 1
    geodist <- mgeo[i,j]
    if (geodist < 300) {
      lingdist <- m[i,j]
      pairsused <- pairsused + 1
      w_i <- match(rownames(mgeo)[i], d$id)
      w_j <- match(rownames(mgeo)[j], d$id)
      if (sum(is.na(w_i), is.na(w_j)) == 0) {
        lon1 <- as.numeric(d$lon[w_i])
        lon2 <- as.numeric(d$lon[w_j])
        lat1 <- as.numeric(d$lat[w_i])
        lat2 <- as.numeric(d$lat[w_j])
        number_rivers <- invisible(crc(lon1, lat1, lon2, lat2, rivers_m))
        cat(i, "\t", j, "\t", geodist, "\t", lingdist, "\t", number_rivers, "\n",
            file="rivers_m.txt", append=TRUE)
      }
    }
  }
}

