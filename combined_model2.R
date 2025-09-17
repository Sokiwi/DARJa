# read the data files
bi <- read.table(file="biomes3.txt", header=TRUE, strip.white=TRUE)
ri <- read.table("rivers_l_all.txt2", sep="\t", header=TRUE)
ad <- read.table(file="admin_units.txt", sep="\t", header=TRUE)

# select only pairs with a geographical distance of 650 km or less
# since the max distance available for all is 676 km (oblasts inducing the limit)
bi2 <- bi[bi$geodist <= 650,]
ri2 <- ri[ri$geodist <= 650,]
ad2 <- ad[ad$geodist <= 650,][,c("id1", "id2", "geodist", "lingdist", "oblast")]

# turn the 0/1 values into factors
bi3 <- bi2
bi3$biome_difference <- factor(bi3$biome_difference, levels = c(0, 1))
# change number of rivers into 1 if the number is greater than 1
rivers <- as.numeric(as.logical(ri2$number_rivers))
# add that column to the data frame
ri3 <- data.frame(ri2, rivers)
ri3 <- ri3[,-5]
# change the presence/absence of rivers to factors for the sake of the gam
ri3$rivers <- factor(ri3$rivers, levels=c(0,1))
ad3 <- ad2
ad3$oblast <- factor(ad3$oblast, levels=c(0,1))

# free up some memory
rm(ri, bi, ad, ri2, bi2, ad2, rivers)

# join the three data frames
library(dplyr)
df <- ad3 %>%
  inner_join(bi3, by = c("id1", "id2", "geodist", "lingdist")) %>%
  inner_join(ri3, by = c("id1", "id2", "geodist", "lingdist"))

# gam model
library(mgcv)
model_all <- bam(
  lingdist ~ s(geodist) + 
    biome_difference + 
    rivers + 
    oblast,
  data = df, method = "fREML"
)
summary(model_all)

# Family: gaussian 
# Link function: identity 

# Formula:
#   lingdist ~ s(geodist) + biome_difference + rivers + oblast

# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       4.836e-01  2.369e-04 2040.90   <2e-16 ***
#   biome_difference1 3.776e-02  9.459e-05  399.25   <2e-16 ***
#   rivers1           7.775e-03  1.632e-04   47.64   <2e-16 ***
#   oblast1           1.715e-02  1.904e-04   90.05   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(geodist) 8.978      9 105438  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# R-sq.(adj) =  0.437   Deviance explained = 43.7%
# fREML = -3.4812e+06  Scale est. = 0.0054134  n = 2924232

library(itsadug)
plot_smooth(model_all, view="geodist", rug=FALSE, main="Effect of distance")
# plot_parametric(model_all, pred=c("rivers"))  # barplot of parametric terms

## relative contributions

# Full model
m_full <- bam(
  lingdist ~ s(geodist) + biome_difference + rivers + oblast,
  data = df, method = "fREML"
)

# Reduced models (drop each predictor)
m_nogeodist <- bam(lingdist ~ biome_difference + rivers + oblast, data = df, method="fREML")
m_nobiome   <- bam(lingdist ~ s(geodist) + rivers + oblast, data = df, method="fREML")
m_norivers  <- bam(lingdist ~ s(geodist) + biome_difference + oblast, data = df, method="fREML")
m_nooblast  <- bam(lingdist ~ s(geodist) + biome_difference + rivers, data = df, method="fREML")

dev_full      <- summary(m_full)$dev.expl
dev_nogeodist <- summary(m_nogeodist)$dev.expl
dev_nobiome   <- summary(m_nobiome)$dev.expl
dev_norivers  <- summary(m_norivers)$dev.expl
dev_nooblast  <- summary(m_nooblast)$dev.expl

# Drops in deviance explained
contr_geodist <- dev_full - dev_nogeodist
contr_biome   <- dev_full - dev_nobiome
contr_rivers  <- dev_full - dev_norivers
contr_oblast  <- dev_full - dev_nooblast

# Normalize to percentages of total explained deviance
contr_total <- contr_geodist + contr_biome + contr_rivers + contr_oblast
contr_perc <- c(
  Geography = contr_geodist / contr_total * 100,
  Biome     = contr_biome   / contr_total * 100,
  Oblast    = contr_oblast  / contr_total * 100,
  Rivers    = contr_rivers  / contr_total * 100
)
contr_perc

barplot(contr_perc,
        # main = "Relative contributions to linguistic distance",
        ylab = "Percentage of explained variance (%)",
        col = c("darkblue", "forestgreen", "skyblue", "orange"))

  

