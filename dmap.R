# libraries
library(rnaturalearth)  # ne_countries(), ne_download()
library(sf)  # for plotting
library(ggplot2)  # for plotting
library(rnaturalearthdata)  # for plotting
library(ggspatial)  # for plotting
library(ggrepel)  # for plotting
library(sf)  # for plotting
library(dplyr)  # for plotting
library(dbscan)  # lof(), dbscan()
library(e1071)  # svm()
library(FNN)  # get.knn(), get.knnx() 
library(mclust)  # Mclust()
library(loon)  # l_colorName()

# R data files
load("darja_data2.RData")  # object called d
load("linguistic_distance_matrix4.RData")  #  m
load("WardD.RData")  # WardD (promising)
load("WardD2.RData")  # WardD2 (missing NW outliers)
load("complete.RData")  # complete (large N area, no NW cluster, mostly no Chukhloma)
load("UPGMA.RData")  # UPGMA (clusters uneven)
load("WPGMA.RData")  # WPGMA (missing NW cluster,  Chukhloma maybe too large)
load("cities.RData")  # data for plotting anchoring cities

# text data files
biomes_locs <- read.table(file="biomes_locs.txt")

# R scripts
source("locations.R")

# derived objects needed by the function
total_locs <- length(unique(d$id))

# the tree can be cut up into k groups, for instance k = 4,
# and mapped; the following is a function for that, called dmap
# for dialect map

# it also has option for taking into account different methods
# of identifying outliers

# lof scores are scores of how well a location diverges geographically 
# from its fellows cluster members (=local outlier factor); this seem to work best
# minpnts is the number of nearest neighbors used in defining the local 
# neighborhood of a point (includes the point itself)
# it needs to be above 63 to recognize the outlier in the north as an outlier
# the lofcutoff could be lowered to include more of the points in the north, but 
# then it will also include some dots that just happen to be a bit
# removed from fellow cluster members for geographical reasons

# SVM is a support vector machine identifying items as outliers if they are not
# predicted by the model; doesn't work so well, perhaps might work better with
# some parameter tuning

# DBSCAN identifies noisy points, which can be considered outliers; results are
# rather similar to lof

# KNN counts the number of neighbors belonging to the same group; if this
# is below the SGC (same group count) threshold the location is counted as
# an outlier; didn't immediately manage to tune parameters to get a meaningful
# result

# GMM (Gaussian Mixture Models) produces a log-likelihood for the classification
# of a given point in its particular cluster
# if this log-likelihood falls beneath a certain max probability (MP) cutoff 
# it is defined as an outlier
# somehow this doesn't work well; some inspection and parameter tuning necessary,
# but this may not be worth the effort because each fitting takes several seconds
# making it tedious to do a lot of experiments

# setting lonlim and latlim enables a view of part of the area
# for instance lonlim=c(27.8,31.5), latlim=c(58,60) shows the diversity in the NW
# and lonlim=c(40,45), latlim=c(58,60) shows the northen island
# also possible to choose one of 9 squares: "NW", "NC", "NE", "CW", "CC", ..., "SE"

# labels showing IDs are added automatically when zooming in on a certain area
# in the GEOPLOT=FALSE mode

# when one of the outlier identifying functions is turned on
# the following information will be added to the 
# dataframe with outgroup information: outgroup, id_inmate, orig_id_inmate, 
# id_outmate, orig_id_outmate, hamming_inmate, hamming_outmate

# A generic map can be made, showing all locations, but not clusters
# For instance 
# dmap(GENERICPLOT=TRUE, SCALE="L", MARGIN=7, POINTSIZE=.5, CITYSIZE=1.5, CITYSHAPE = 20)
# and choosing width of 800 when exporting

# A data frame called cities used for plotting some major cities
# in the area were prepared in the following way
# library(toponym) - see https://github.com/Lennart05/toponym
# top(strings=c("Saint Petersburg", "Vologda", "Pskov", "Moscow", 
#   "Nizhniy Novgorod", "Smolensk", "Tambov", "Belgorod"), 
#   countries="RU", name="cities_all")
# that produces a data frame cities_all from geonames.org, but with some cities 
# whose names are synonymous, so the right ones are filtered by their geonames IDs
# ids <- c(498817, 472459, 504341, 524901, 520555, 491687, 484646, 578072)
# w_ids <- match(ids, cities_all$geonameid)
# cities <- cities_all[w_ids,c("name", "latitude", "longitude", "group")]
# There is a column called group, which is reused here with the same name
# but just given a factor of 1 as values
# cities$group <- as.factor(1)
# save(cities, file="cities.RData")

# download geographical data from rnaturalearth if not already in memory
geodata <- c("world_s", "rivers_s", "lakes_s", 
             "world_m", "rivers_m", "lakes_m", 
             "world_l", "rivers_l", "lakes_l")

if (sum(geodata %in% ls()) < 9) {
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

dmap <- function(k=5, tree=UPGMA, lonlim=c(0,0), latlim=c(0,0), AREA="NONE", 
                 lofvals=FALSE, lofcutoff=1.5, minpts=65, SVM=FALSE, 
                 DBSCAN=FALSE, EPS=1.52, KNN=FALSE, K=50, SGC=10, GMM=FALSE, 
                 MP=0.45, LINES=FALSE, GEOPLOT=FALSE, FILL="antiquewhite", 
                 SCALE="L", GENERICPLOT=FALSE, MARGIN=7, POINTSIZE=0.1,
                 CITYSIZE=1, CITYSHAPE=18, BIOMEPLOT=FALSE) {
  # trees available: WardD, WardD2, complete, UPGMA, WPGMA, 
  # choosing one of these areas allows to zoom in on predefined regions
  # they are the same regions shown by lines if the LINES argument is
  # set to TRUE and GEOPLOT=FALSE
  if (AREA=="SW") {latlim <- c(49,54); lonlim <- c(27,34)}
  if (AREA=="CW") {latlim <- c(54,58); lonlim <- c(27,34)}
  if (AREA=="NW") {latlim <- c(58,62); lonlim <- c(27,34)}
  if (AREA=="SC") {latlim <- c(49,54); lonlim <- c(34,41)}
  if (AREA=="CC") {latlim <- c(54,58); lonlim <- c(34,41)}
  if (AREA=="NC") {latlim <- c(58,62); lonlim <- c(34,41)}
  if (AREA=="SE") {latlim <- c(49,54); lonlim <- c(41,49)}
  if (AREA=="CE") {latlim <- c(54,58); lonlim <- c(41,49)}
  if (AREA=="NE") {latlim <- c(58,62); lonlim <- c(41,49)}
  graphics.off()
  returned_item <- NA
  groups <- cutree(tree, k = k)
  group <- as.vector(groups)
  id <- d$id[match(1:total_locs, d$id)]
  orig_id <- d$orig_id[match(1:total_locs, d$id)]
  x <- as.numeric(d$lon[match(1:total_locs, d$id)])
  y <- as.numeric(d$lat[match(1:total_locs, d$id)])
  # the classification put in a data frame together with 
  # coordinates for the purposes of a plot
  pdata <- data.frame(id,orig_id,x,y,group)  # stands for plotting data
  w_na <- union(which(is.na(pdata$x)), which(is.na(pdata$y)))
  if (length(w_na) > 0) {
    pdata <- pdata[-w_na,]
  }
  if (BIOMEPLOT==TRUE) {
    cat("\nBiome key:\n4: Temperate Broadleaf & Mixed Forests\n6: Boreal Forests/Taiga\n8: Temperate Grasslands, Savannas & Shrublands\n98: Water\n")
    w_id <- match(pdata$id, biomes_locs$id)
    biome <- as.factor(biomes_locs$b_code[w_id])
    pdata2 <- data.frame(pdata, biome)
    my_points <- pdata2[,c("x", "y", "biome")]
    names(my_points) <- c("lon", "lat", "biome")

    # Convert points to an sf object
    my_points_sf <- st_as_sf(my_points, coords = c("lon", "lat"), crs = 4326)

    # Bounding box for zooming
    # this is identical to the whole area unless lonlim and latlim are specified
    if (identical(lonlim, c(0,0)) & identical(latlim, c(0,0))) {
      bbox <- st_bbox(my_points_sf)
    } else {
      bbox <- st_bbox(c(xmin = lonlim[1], xmax = lonlim[2], ymax = latlim[2], ymin = latlim[1]), crs = st_crs(4326))
    }
    
    # Prepare map data (countries, rivers, lakes)
    if (SCALE=="S") {world <- world_s; rivers <- rivers_s; lakes <- lakes_s}
    if (SCALE=="M") {world <- world_m; rivers <- rivers_m; lakes <- lakes_m}
    if (SCALE=="L") {world <- world_l; rivers <- rivers_l; lakes <- lakes_l}
    
    # Create map with ggplot2
    ggmap <- ggplot() +
      geom_sf(data = world, fill = FILL, color = "gray60") +
      geom_sf(data = lakes, fill = "lightblue", color = NA) +
      geom_sf(data = my_points_sf, aes(color = biome), size = 1) +
      geom_sf(data = rivers, color = "lightblue", size = 0.6) +
      # scale_color_viridis_d(option = "D") +
      scale_color_manual(values = rainbow(length(unique(my_points$biome)))) +
      annotation_scale(location = "bl", width_hint = 0.2) +
      # annotation_north_arrow(location = "bl", which_north = "true") +
      coord_sf(
        xlim = c(bbox["xmin"] - 0.5, bbox["xmax"] + 0.5),
        ylim = c(bbox["ymin"] - 0.5, bbox["ymax"] + 0.5),
        expand = FALSE
      ) +
      theme_minimal() +
      theme(legend.position = "right") +
      labs(title = "",
           subtitle = "",
           x = "Lon", y = "Lat", color = "biome")
    return(ggmap)
    ggmap
  }
  if (GENERICPLOT==TRUE) {
    my_points <- pdata[,c("x", "y", "group")]
    my_points$group <- factor(1)
    names(my_points) <- c("lon", "lat", "group")
    my_cities <- cities
    names(my_cities) <- c("city", "lat", "lon", "group")
    
    # Convert points to an sf object
    my_points_sf <- st_as_sf(my_points, coords = c("lon", "lat"), crs = 4326)
    my_cities_sf <- st_as_sf(my_cities, coords = c("lon", "lat"), crs = 4326)
    
    # Bounding box for zooming
    # this is identical to the whole area unless lonlim and latlim are specified
    if (identical(lonlim, c(0,0)) & identical(latlim, c(0,0))) {
      bbox <- st_bbox(my_points_sf)
    } else {
      bbox <- st_bbox(c(xmin = lonlim[1], xmax = lonlim[2], ymax = latlim[2], ymin = latlim[1]), crs = st_crs(4326))
    }
      
    # Prepare map data (countries, rivers, lakes)
    if (SCALE=="S") {world <- world_s; rivers <- rivers_s; lakes <- lakes_s}
    if (SCALE=="M") {world <- world_m; rivers <- rivers_m; lakes <- lakes_m}
    if (SCALE=="L") {world <- world_l; rivers <- rivers_l; lakes <- lakes_l}
    
    # prepare city labels
    my_cities_coords <- my_cities_sf %>%
      mutate(lon = st_coordinates(.)[,1],
             lat = st_coordinates(.)[,2]) %>%
      st_drop_geometry()  # drop geometry to avoid sf-related issues

    # Create map with ggplot2
    ggmap <- ggplot() +
      geom_sf(data = world, fill = FILL, color = "gray60") +
      geom_sf(data = lakes, fill = "lightblue", color = NA) +
      # geom_sf(data = my_points_sf, aes(color = group), size = POINTSIZE) +
      geom_sf(data = my_points_sf, color="red", size = POINTSIZE, shape=20) +
      geom_sf(data = rivers, color = "lightblue", size = 0.6) +
      geom_sf(data = my_cities_sf, color="black", size = CITYSIZE, shape=CITYSHAPE) +
      scale_color_viridis_d(option = "D") +
      scale_color_manual(values = rainbow(1)) +
      geom_text_repel(
        data = my_cities_coords,
        aes(x = lon, y = lat, label = city),
        size = 2,
        color = "black",
        segment.color = "gray50",
        max.overlaps = Inf,
        box.padding = 0.1,
        point.padding = 0.05
      ) +
      annotation_scale(location = "bl", width_hint = 0.2) +
      # if arrow pointing north is desired:
      # annotation_north_arrow(location = "bl", which_north = "true") +
      coord_sf(
        xlim = c(bbox["xmin"] - MARGIN, bbox["xmax"] + MARGIN),
        ylim = c(bbox["ymin"] - MARGIN, bbox["ymax"] + MARGIN),
        expand = FALSE
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(title = "",
           subtitle = "",
           x = "Lon", y = "Lat")
    return(ggmap)
    ggmap
  }
  if (GEOPLOT==TRUE) {
    if (lofvals==FALSE) {
      my_points <- pdata[,c("x", "y", "group")]
      my_points$group <- factor(my_points$group)
      names(my_points) <- c("lon", "lat", "group")
      
      # Convert points to an sf object
      my_points_sf <- st_as_sf(my_points, coords = c("lon", "lat"), crs = 4326)
      
      # Bounding box for zooming
      # this is identical to the whole area unless lonlim and latlim are specified
      if (identical(lonlim, c(0,0)) & identical(latlim, c(0,0))) {
        bbox <- st_bbox(my_points_sf)
      } else {
        bbox <- st_bbox(c(xmin = lonlim[1], xmax = lonlim[2], ymax = latlim[2], ymin = latlim[1]), crs = st_crs(4326))
      }
      
      # Prepare map data (countries, rivers, lakes)
      if (SCALE=="S") {world <- world_s; rivers <- rivers_s; lakes <- lakes_s}
      if (SCALE=="M") {world <- world_m; rivers <- rivers_m; lakes <- lakes_m}
      if (SCALE=="L") {world <- world_l; rivers <- rivers_l; lakes <- lakes_l}
      
      # Create map with ggplot2
      ggmap <- ggplot() +
        geom_sf(data = world, fill = FILL, color = "gray60") +
        geom_sf(data = lakes, fill = "lightblue", color = NA) +
        geom_sf(data = my_points_sf, aes(color = group), size = 1) +
        geom_sf(data = rivers, color = "lightblue", size = 0.6) +
        # scale_color_viridis_d(option = "D") +
        scale_color_manual(values = rainbow(length(unique(my_points$group)))) +
        annotation_scale(location = "bl", width_hint = 0.2) +
        # annotation_north_arrow(location = "bl", which_north = "true") +
        coord_sf(
          xlim = c(bbox["xmin"] - 0.5, bbox["xmax"] + 0.5),
          ylim = c(bbox["ymin"] - 0.5, bbox["ymax"] + 0.5),
          expand = FALSE
        ) +
        theme_minimal() +
        theme(legend.position = "right") +
        labs(title = "",
             subtitle = "",
             x = "Lon", y = "Lat", color = "Cluster")
      return(ggmap)
      ggmap
    }
    if (lofvals==TRUE) {
      LOF <- rep(0, nrow(pdata))
      for (i in 1:k) {
        w_i <- which(pdata$group==i)
        locs_i <- as.matrix(pdata[w_i,c("x","y")])
        LOF[w_i] <- round(lof(locs_i, minPts=minpts),4)
      }
      pdp <- data.frame(pdata, LOF)  # stat pro pdata plus
      pdp$group <- factor(pdp$group)
      my_points <- pdp[,c("x", "y", "group")]
      names(my_points) <- c("lon", "lat", "group")
      # outliers (pch = 0)
      my_squares <- pdp[pdp$LOF>lofcutoff,c("x", "y", "group")]
      names(my_squares) <- c("lon", "lat", "group")
      
      # Convert points to an sf object
      my_points_sf <- st_as_sf(my_points, coords = c("lon", "lat"), crs = 4326)
      my_squares_sf <- st_as_sf(my_squares, coords = c("lon", "lat"), crs = 4326)
      
      # Bounding box for zooming
      # this is identical to the whole area unless lonlim and latlim are specified
      if (identical(lonlim, c(0,0)) & identical(latlim, c(0,0))) {
        bbox <- st_bbox(my_points_sf)
      } else {
        bbox <- st_bbox(c(xmin = lonlim[1], xmax = lonlim[2], ymax = latlim[2], ymin = latlim[1]), crs = st_crs(4326))
      }
      
      # Prepare map data (countries, rivers, lakes)
      if (SCALE=="S") {world <- world_s; rivers <- rivers_s; lakes <- lakes_s}
      if (SCALE=="M") {world <- world_m; rivers <- rivers_m; lakes <- lakes_m}
      if (SCALE=="L") {world <- world_l; rivers <- rivers_l; lakes <- lakes_l}
      
      # 3. Create map with ggplot2
      ggmap <- ggplot() +
        geom_sf(data = world, fill = FILL, color = "gray60") +
        geom_sf(data = lakes, fill = "lightblue", color = NA) +
        geom_sf(data = my_points_sf, aes(color = group), size = 2) +
        geom_sf(data = my_squares_sf, aes(color = group), size = 2, shape=15) +
        geom_sf(data = rivers, color = "lightblue", size = 0.6) +
        # scale_color_viridis_d(option = "D") +
        scale_color_manual(values = rainbow(length(unique(my_points$group)))) +
        annotation_scale(location = "bl", width_hint = 0.2) +
        # annotation_north_arrow(location = "bl", which_north = "true") +
        coord_sf(
          xlim = c(bbox["xmin"] - 0.5, bbox["xmax"] + 0.5),
          ylim = c(bbox["ymin"] - 0.5, bbox["ymax"] + 0.5),
          expand = FALSE
        ) +
        theme_minimal() +
        theme(legend.position = "right") +
        labs(title = "",
             subtitle = "",
             x = "Lon", y = "Lat", color = "Cluster")
      return(ggmap)
      ggmap
    }
  }
  if (GEOPLOT==TRUE & lofvals==TRUE) {
    oi <- pdp[pdp$LOF>lofcutoff,]  # stat pro outlier info
  }
  som <- sum(lofvals, SVM, DBSCAN, KNN, GMM)
  if (som==0) {
    if (identical(lonlim, c(0,0)) & identical(latlim, c(0,0))) {
      plot(pdata$x, pdata$y, col=pdata$group, 
           xlab="lon", ylab="lat", pch=20)
      if (LINES==TRUE) {
        abline(h = c(54,58), v = c(34,41), col="white")
      }
    } else {
      plot(pdata$x, pdata$y, col=pdata$group, 
           xlab="lon", ylab="lat", pch=19, xlim=lonlim, ylim=latlim)
    }
  }
  if (lofvals==TRUE & GEOPLOT==FALSE) {
    LOF <- rep(0, nrow(pdata))
    for (i in 1:k) {
      w_i <- which(pdata$group==i)
      locs_i <- as.matrix(pdata[w_i,c("x","y")])
      LOF[w_i] <- round(lof(locs_i, minPts=minpts),4)
    }
    pdp <- data.frame(pdata, LOF)  # stat pro pdata plus
    if (identical(lonlim, c(0,0)) & identical(latlim, c(0,0))) {
      plot(pdp$x, pdp$y, col=pdp$group, 
           xlab="lon", ylab="lat", pch=20)
      points(pdp$x[pdp$LOF>lofcutoff], pdp$y[pdp$LOF>lofcutoff], 
             col=pdp$group[pdp$LOF>lofcutoff], pch=0)
      if (LINES==TRUE) {
        abline(h = c(54,58), v = c(34,41), col="white")
      }
    } else {
      plot(pdp$x, pdp$y, col=pdp$group, 
           xlab="lon", ylab="lat", pch=19, xlim=lonlim, ylim=latlim)
      points(pdp$x[pdp$LOF>lofcutoff], pdp$y[pdp$LOF>lofcutoff], 
             col=pdp$group[pdp$LOF>lofcutoff], pch=0)
      text(pdp$x[pdp$LOF>lofcutoff], pdp$y[pdp$LOF>lofcutoff], 
           pdp$id[pdp$LOF>lofcutoff], col=pdp$group[pdp$LOF>lofcutoff], pos=1, cex=.7)
    }
    oi <- pdp[pdp$LOF>lofcutoff,]  # stat pro outlier info
  }
  if (SVM==TRUE) {
    outlier <- rep(0, nrow(pdata))
    for (i in 1:k) {
      w_i <- which(pdata$group==i)
      locs_i <- as.matrix(pdata[w_i,c("x","y")])
      model <- svm(locs_i, type = 'one-classification', nu = 0.05, kernel = 'radial')
      predictions <- predict(model, locs_i)
      outliers <- which(!predictions)
      outlier[w_i[outliers]] <- 1
    }
    pdp2 <- data.frame(pdata, outlier)  # stat pro pdata plus
    if (identical(lonlim, c(0,0)) & identical(latlim, c(0,0))) {
      plot(pdp2$x, pdp2$y, col=pdp2$group, 
           xlab="lon", ylab="lat", pch=20)
      points(pdp2$x[pdp2$outlier==1], pdp2$y[pdp2$outlier==1], 
             col=pdp2$group[pdp2$outlier==1], pch=0)
      if (LINES==TRUE) {
        abline(h = c(54,58), v = c(34,41), col="white")
      }
    } else {
      plot(pdp2$x, pdp2$y, col=pdp2$group, 
           xlab="lon", ylab="lat", pch=19, xlim=lonlim, ylim=latlim)
      points(pdp2$x[pdp2$outlier==1], pdp2$y[pdp2$outlier==1], 
             col=pdp2$group[pdp2$outlier==1], pch=0)
    }
  }
  if (DBSCAN==TRUE) {
    library(dbscan)
    outdbs <- rep(0, nrow(pdata))  # stat pro outlier for dbscan
    for (i in 1:k) {
      w_i <- which(pdata$group==i)
      locs_i <- as.matrix(pdata[w_i,c("x","y")])
      db <- dbscan(locs_i, eps = EPS, minPts = minpts)
      outliers <- which(db$cluster == 0)
      outdbs[w_i[outliers]] <- 1
    }
    pdp3 <- data.frame(pdata, outdbs)  # stat pro pdata plus
    if (identical(lonlim, c(0,0)) & identical(latlim, c(0,0))) {
      plot(pdp3$x, pdp3$y, col=pdp3$group, 
           xlab="lon", ylab="lat", pch=20)
      points(pdp3$x[pdp3$outdbs==1], pdp3$y[pdp3$outdbs==1], 
             col=pdp3$group[pdp3$outdbs==1], pch=0)
      if (LINES==TRUE) {
        abline(h = c(54,58), v = c(34,41), col="white")
      }
    } else {
      plot(pdp3$x, pdp3$y, col=pdp3$group, 
           xlab="lon", ylab="lat", pch=19, xlim=lonlim, ylim=latlim)
      points(pdp3$x[pdp3$outdbs==1], pdp3$y[pdp3$outdbs==1], 
             col=pdp3$group[pdp3$outdbs==1], pch=0)
    }
    oi <- pdp3[pdp3$outdbs==1,]
  }
  if (KNN==TRUE) {
    outknn <- rep(0, nrow(pdata))  # stat pro outlier for dbscan
    for (i in 1:k) {
      w_i <- which(pdata$group==i)
      locs_i <- as.matrix(pdata[w_i,c("x","y")])
      knn_result <- get.knn(locs_i, K)
      outlier_flags <- sapply(1:nrow(locs_i), function(j) {
        neighbor_ids <- knn_result$nn.index[j, ]
        same_group_count <- sum(groups[neighbor_ids] == groups[j])
        return(same_group_count < SGC)  # tune threshold
      })
      
      outliers <- which(outlier_flags)
      outknn[w_i[outliers]] <- 1
    }
    pdp4 <- data.frame(pdata, outknn)  # stat pro pdata plus
    if (identical(lonlim, c(0,0)) & identical(latlim, c(0,0))) {
      plot(pdp4$x, pdp4$y, col=pdp4$group, 
           xlab="lon", ylab="lat", pch=20)
      points(pdp4$x[pdp4$outknn==1], pdp4$y[pdp4$outknn==1], 
             col=pdp4$group[pdp4$outknn==1], pch=0)
      if (LINES==TRUE) {
        abline(h = c(54,58), v = c(34,41), col="white")
      }
    } else {
      plot(pdp4$x, pdp4$y, col=pdp4$group, 
           xlab="lon", ylab="lat", pch=19, xlim=lonlim, ylim=latlim)
      points(pdp4$x[pdp4$outknn==1], pdp4$y[pdp4$outknn==1], 
             col=pdp4$group[pdp4$outknn==1], pch=0)
    }
    oi <- pdp4[pdp4$outknn==1,]
  }
  if (GMM==TRUE) {
    outgmm <- rep(0, nrow(pdata))  # stat pro outlier for GMM
    for (i in 1:k) {
      w_i <- which(pdata$group==i)
      locs_i <- as.matrix(pdata[w_i,c("x","y")])
      gmm <- Mclust(locs_i)
      ll <- gmm$z  # z: posterior probabilities, ll stat pro log likelihoods
      max_probs <- apply(log_likelihoods, 1, max)
      outliers <- which(max_probs < MP)
      outgmm[w_i[outliers]] <- 1
    }
    pdp5 <- data.frame(pdata, outgmm)  # stat pro pdata plus
    if (identical(lonlim, c(0,0)) & identical(latlim, c(0,0))) {
      plot(pdp5$x, pdp5$y, col=pdp5$group, 
           xlab="lon", ylab="lat", pch=20)
      points(pdp5$x[pdp5$outgmm==1], pdp5$y[pdp5$outgmm==1], 
             col=pdp5$group[pdp5$outgmm==1], pch=0)
      if (LINES==TRUE) {
        abline(h = c(54,58), v = c(34,41), col="white")
      }
    } else {
      plot(pdp5$x, pdp5$y, col=pdp5$group, 
           xlab="lon", ylab="lat", pch=19, xlim=lonlim, ylim=latlim)
      points(pdp5$x[pdp5$outgmm==1], pdp5$y[pdp5$outgmm==1], 
             col=pdp5$group[pdp5$outgmm==1], pch=0)
    }
    oi <- pdp5[pdp5$outgmm==1,]
  }
  # If any of the outlier identifying routines were turned on
  # information will be added about the outliers
  if (som > 0) {
    fo <- function(q) {  # stat pro find outgroup
      out <- rep(0, nrow(oi))
      for (i in 1:nrow(oi)) {
        # remove the outlier itself
        excl <- match(oi$id[i], pdata$id)
        pdata2 <- pdata[-excl,]
        coords <- pdata2[,c("x","y")]
        Q <- matrix(c(oi$x[i], oi$y[i]), ncol=2)
        knn_result <- get.knnx(data = coords, query = Q, k = q)
        outgroup_candidates <- pdata2[knn_result$nn.index,]$group
        outgroup_candidate <- as.numeric(names(table(outgroup_candidates)[1]))
        if(outgroup_candidate==oi$group[i]) {
          outgroup_candidate <- as.numeric(names(table(outgroup_candidates)[2]))
        }
        out[i] <- outgroup_candidate
      }
      return(out)
    }
    # the function above identifies the outgroup from q nearest neighbors
    # it was tested that 9 nearest neighbors it what it minimally takes to
    # always find an outgroup that is not the same as the group of the outlier
    outgroup <- fo(9)
    oi2 <- data.frame(oi, outgroup)
    
    # find most similar inmate (msi) and outmates (mso)
    id_msi <- rep(0, nrow(oi2))
    orig_id_msi <- rep(NA, nrow(oi2))
    x_msi <- rep(0, nrow(oi2))
    y_msi <- rep(0, nrow(oi2))
    min_in_hd <- rep(0, nrow(oi2))
    id_mso <- rep(0, nrow(oi2))
    orig_id_mso <- rep(NA, nrow(oi2))
    x_mso <- rep(0, nrow(oi2))
    y_mso <- rep(0, nrow(oi2))
    min_out_hd <- rep(0, nrow(oi2))
    msio <- function(i) {  # stat pro most similar in- and outmates
      mate <- oi2$id[i]  # the id of the target location
      ingroup <- oi2$group[i]  # the ingroup number
      outgroup <- oi2$outgroup[i]  # the outgroup number
      w_ingroup <- which(pdata$group==ingroup)
      w_outgroup <- which(pdata$group==outgroup)
      ids_ingroup <- pdata$id[w_ingroup]
      ids_outgroup <- pdata$id[w_outgroup]
      all_hds <- data.frame(id=as.numeric(rownames(m)),hd=m[,mate])
      all_hds$hd[mate] <- 1
      in_hds <- all_hds[match(ids_ingroup, all_hds$id),]
      out_hds <- all_hds[match(ids_outgroup, all_hds$id),]
      min_in_hd_i <- min(in_hds$hd)
      w_msi_i <- which(in_hds$hd==min_in_hd_i)[1]
      id_msi_i <- in_hds$id[w_msi_i]
      min_out_hd_i <- min(out_hds$hd)
      w_mso_i <- which(out_hds$hd==min_out_hd_i)[1]
      id_mso_i <- out_hds$id[w_mso_i]
      w_msi_i_pdata <- which(pdata$id==id_msi_i)
      w_mso_i_pdata <- which(pdata$id==id_mso_i)
      orig_id_msi_i <- pdata$orig_id[w_msi_i_pdata]
      orig_id_mso_i <- pdata$orig_id[w_mso_i_pdata]
      x_msi_i <- pdata$x[w_msi_i_pdata]
      x_mso_i <- pdata$x[w_mso_i_pdata]
      y_msi_i <- pdata$y[w_msi_i_pdata]
      y_mso_i <- pdata$y[w_mso_i_pdata]
      return(c(id_msi_i, orig_id_msi_i, x_msi_i, y_msi_i, min_in_hd_i, id_mso_i, orig_id_mso_i, x_mso_i, y_mso_i, min_out_hd_i))
    }
    for (i in 1:nrow(oi2)) {
      msio_res <- msio(i)
      id_msi[i] <- as.numeric(msio_res[1])
      orig_id_msi[i] <- msio_res[2]
      x_msi[i] <- as.numeric(msio_res[3])
      y_msi[i] <- as.numeric(msio_res[4])
      min_in_hd[i] <- as.numeric(msio_res[5])
      id_mso[i] <- as.numeric(msio_res[6])
      orig_id_mso[i] <- msio_res[7]
      x_mso[i] <- as.numeric(msio_res[8])
      y_mso[i] <- as.numeric(msio_res[9])
      min_out_hd[i] <- as.numeric(msio_res[10])
    }
    oi3 <- data.frame(oi2, id_msi, orig_id_msi, x_msi, y_msi, min_in_hd, id_mso, 
                      orig_id_mso, x_mso, y_mso, min_out_hd)
    # add information about difference between minimal outgroup and ingroup hd
    dif_min_oi_hd <- round(oi3$min_out_hd - oi3$min_in_hd,4)
    # function to assign places to one of 9 regions, for better orientation
    get_area <- function(lon,lat) {
      if ((lat > 49 & lat <= 54) & (lon > 27 & lon <= 34)) {A <- "SW"}
      if ((lat > 54 & lat <= 58) & (lon > 27 & lon <= 34)) {A <- "CW"}
      if ((lat > 58 & lat <= 62) & (lon > 27 & lon <= 34)) {A <- "NW"}
      if ((lat > 49 & lat <= 54) & (lon > 34 & lon <= 41)) {A <- "SC"}
      if ((lat > 54 & lat <= 58) & (lon > 34 & lon <= 41)) {A <- "CC"}
      if ((lat > 58 & lat <= 62) & (lon > 34 & lon <= 41)) {A <- "NC"}
      if ((lat > 49 & lat <= 54) & (lon > 41 & lon <= 49)) {A <- "SE"}
      if ((lat > 54 & lat <= 58) & (lon > 41 & lon <= 49)) {A <- "CE"}
      if ((lat > 58 & lat <= 62) & (lon > 41 & lon <= 49)) {A <- "NE"}
      return(A)
    }
    area <- rep(NA, nrow(oi3))
    for (i in 1:nrow(oi3)) {
      area[i] <- get_area(oi3$x[i], oi3$y[i])
    }
    # get names of colors in addition to numbers for the groups
    hex <- palette()[1:k]
    colornames <- l_colorName(hex)
    colornames <- as.vector(unlist(lapply(colornames, function(x) {strsplit(x, "[1-9]")[[1]][1]})))
    colorin <- colornames[oi3$group]
    colorout <- colornames[oi3$outgroup]
    oi4 <- data.frame(oi3, dif_min_oi_hd, colorout, colorin, area)
    # get names of villages and their oblast'
    village <- c()
    oblast <- c()
    for (i in 1:nrow(oi4)) {
      village_info <- get_metadata_location(oi4$orig_id[i])
      village[i] <- village_info$nazvanie_np
      oblast[i] <- village_info$oblast
    }
    oi5 <- data.frame(oi4, village, oblast)
    returned_item <- oi5
    write.table(oi5, file="outliers.txt", sep="\t", row.names=FALSE, quote=FALSE)
    cat("\nregistered", nrow(oi5), "outliers; see outliers.txt\n")
  }
  # return(returned_item)
}
