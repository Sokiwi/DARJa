# This produces a table with average linguistic distances within 30 km 
# geographical distance bins for cases where two locations are either
# not separated by a river or separated by (just) one river or lake;
# also provides p-values from a t-test of differences in the means and N
# Takes either rivers_m.txt or rivers_l.txt as input
# These are respectively medium (1:50m; medium resolution) and 
# large scale (1:10m; higher resolution) data
# Medium scale includes relatively few rivers, large scale relatively many
# See https://www.naturalearthdata.com/downloads/
# The data were produced with rivers.R, using the rnaturalearth package

x <- read.table(file="rivers_m.txt", header=TRUE, strip.white=TRUE)
nrow(x)

zero <- x[x$number_rivers==0,]
one <- x[x$number_rivers==1,]

# zero_one <- rbind(zero, one)
nrow(zero) + nrow(one)

# bins_zero <- cut(zero$geodist, breaks=10)
bins_zero <- cut(zero$geodist, breaks=seq(0, 300, 30))
zero <- data.frame(zero, bins_zero)

# bins_one <- cut(one$geodist, breaks=10)
bins_one <- cut(one$geodist, breaks=seq(0, 300, 30))
one <- data.frame(one, bins_one)

no_riv_aggr <- aggregate(zero$lingdist, by=list(bins_zero), mean)
riv_aggr <- aggregate(one$lingdist, by=list(bins_one), mean)

test1 <- t.test(zero$lingdist[zero$bins_zero=="(0,30]"],one$lingdist[one$bins_one=="(0,30]"])
p1 <- test1$p.value
# (0,30], significant

test2 <- t.test(zero$lingdist[zero$bins_zero=="(30,60]"],one$lingdist[one$bins_one=="(30,60]"])
p2 <- test2$p.value
# (30,60], significant

test3 <- t.test(zero$lingdist[zero$bins_zero=="(60,90]"],one$lingdist[one$bins_one=="(60,90]"])
p3 <- test3$p.value
# (60,90], significant

test4 <- t.test(zero$lingdist[zero$bins_zero=="(90,120]"],one$lingdist[one$bins_one=="(90,120]"])
p4 <- test4$p.value
# (90,120], significant

test5 <- t.test(zero$lingdist[zero$bins_zero=="(120,150]"],one$lingdist[one$bins_one=="(120,150]"])
p5 <- test5$p.value
# (120,150], significant

test6 <- t.test(zero$lingdist[zero$bins_zero=="(150,180]"],one$lingdist[one$bins_one=="(150,180]"])
p6 <- test6$p.value
# (150,180], not significant

test7 <- t.test(zero$lingdist[zero$bins_zero=="(180,210]"],one$lingdist[one$bins_one=="(180,210]"])
p7 <- test7$p.value
# (180,210], significant

test8 <- t.test(zero$lingdist[zero$bins_zero=="(210,240]"],one$lingdist[one$bins_one=="(210,240]"])
p8 <- test8$p.value
# (210,240], significant

test9 <- t.test(zero$lingdist[zero$bins_zero=="(240,270]"],one$lingdist[one$bins_one=="(240,270]"])
p9 <- test9$p.value
# (240,270], significant

test10 <- t.test(zero$lingdist[zero$bins_zero=="(270,300]"],one$lingdist[one$bins_one=="(270,300]"])
p10 <- test10$p.value
# (270,300], significant

N1 <- sum(length(zero$lingdist[zero$bins_zero=="(0,30]"]),length(one$lingdist[one$bins_one=="(0,30]"]))
N2 <- sum(length(zero$lingdist[zero$bins_zero=="(30,60]"]),length(one$lingdist[one$bins_one=="(30,60]"]))
N3 <- sum(length(zero$lingdist[zero$bins_zero=="(60,90]"]),length(one$lingdist[one$bins_one=="(60,90]"]))
N4 <- sum(length(zero$lingdist[zero$bins_zero=="(90,120]"]),length(one$lingdist[one$bins_one=="(90,120]"]))
N5 <- sum(length(zero$lingdist[zero$bins_zero=="(120,150]"]),length(one$lingdist[one$bins_one=="(120,150]"]))
N6 <- sum(length(zero$lingdist[zero$bins_zero=="(150,180]"]),length(one$lingdist[one$bins_one=="(150,180]"]))
N7 <- sum(length(zero$lingdist[zero$bins_zero=="(180,210]"]),length(one$lingdist[one$bins_one=="(180,210]"]))
N8 <- sum(length(zero$lingdist[zero$bins_zero=="(210,240]"]),length(one$lingdist[one$bins_one=="(210,240]"]))
N9 <- sum(length(zero$lingdist[zero$bins_zero=="(240,270]"]),length(one$lingdist[one$bins_one=="(240,270]"]))
N10 <- sum(length(zero$lingdist[zero$bins_zero=="(270,300]"]),length(one$lingdist[one$bins_one=="(270,300]"]))

mean_lingdist <- data.frame(no_riv_aggr$Group.1, round(riv_aggr$x,4), round(no_riv_aggr$x,4))
names(mean_lingdist) <- c("bins", "rivers", "no_rivers")
pvals <- round(c(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10),4)
N <- c(N1, N2, N3, N4, N5, N6, N7, N8, N9, N10)
mean_lingdist <- data.frame(mean_lingdist, pvals, N)
mean_lingdist
