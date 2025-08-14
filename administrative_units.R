## This script both creates a file with information on
# id1, id2, geodist, lingdist, oblast rajon
# and also analyzes the influence of administrative units

load("darja_data3.RData")  # d
# extract only ids, one row per location
# the original id and the id using 4193 numbers are needed for 
# matching to atlas_spravka_svod_unicode.txt
w_unique <- match(unique(d$id), d$id)
du <- d[w_unique, c("id", "orig_id")]  # unique of d

ass <- read.table(file="atlas_spravka_svod_unicode.txt", sep="\t", header = TRUE, fileEncoding = "UTF-16LE")
names(ass) <- c("nomer_np", "tom_atlasa", "tip_np", "nazvanie_np", "oblast", "rajon", "god")
length(unique(ass$oblast))  # 37
length(unique(ass$rajon))  # 895

# check if rajons are true subdivisions of oblasts--are there rajons
# spread over multiple oblasts?
rajon_in_multiple_oblast <- ass %>% 
  distinct(rajon, oblast) %>%
  group_by(rajon) %>%
  filter(n() > 1) %>%
  arrange(rajon, oblast)
print(as.data.frame(rajon_in_multiple_oblast))
# yes, there are many cases, which is because same names of rayons
# can be found in multiple oblasts

# routine for creating same ID type in ass as in du
orig_id <- c()
for (i in 1:nrow(ass)) {
  if (ass$tom_atlasa[i]=="восток") {
    orig_id[i] <- paste0(ass$nomer_np[i], "_vostok")
  } else if (ass$tom_atlasa[i]=="запад") {
    orig_id[i] <- paste0(ass$nomer_np[i], "_zapad")
  } else if (ass$tom_atlasa[i]=="север") {
    orig_id[i] <- paste0(ass$nomer_np[i], "_sever")
  } else if (ass$tom_atlasa[i]=="северо-запад") {
    orig_id[i] <- paste0(ass$nomer_np[i], "_severo-zapad")
  } else if (ass$tom_atlasa[i]=="юг") {
    orig_id[i] <- paste0(ass$nomer_np[i], "_jug")
  } else {
    orig_id[i] <- NA
  }
}
ass2 <- data.frame(orig_id, ass)
id <- c()
for (i in 1:nrow(ass2)) {
  w_orig_id <- which(du$orig_id==ass2$orig_id[i])
  if (length(w_orig_id)==0) {
    id[i] <- NA
  } else {
    id[i] <- du$id[w_orig_id]
  }
}
ass3 <- data.frame(id, ass2)
ass4 <- ass3[-which(is.na(ass3$id)),]
# get rid of duplicates
u <- unique(ass4$id)
matches <- match(u, ass4$id)
ass5 <- ass4[matches,]

# create a file with all combinations of locations
# and info as per the heading below
# there should be a 0 for same oblast or rajon
# and a 1 for different ones
load("geographical_distance_matrix3.RData")  # mgeo
load("linguistic_distance_matrix4.RData")  # m
# free up some memory
rm(d); rm(ass); rm(ass2); rm(ass3); rm(ass4); rm(du); rm(matches); rm(orig_id); rm(u)

count <- 0
all <- (nrow(ass5) * (nrow(ass5)-1)) / 2
cat("id1\tid2\tgeodist\tlingdist\toblast\trajon\n", file="admin_units.txt")
for (i in 1:(nrow(ass5)-1)) {
  for (j in (i+1):nrow(ass5)) {
    count <- count + 1
    if (count%%1000==0) {
      cat("doing", count, "out of", all, "\n")
    }
    id1 <- ass5$id[i]
    id2 <- ass5$id[j]
    geodist <- mgeo[as.character(id1), as.character(id2)]
    lingdist <- m[as.character(id1), as.character(id2)]
    if (ass5$oblast[i]==ass5$oblast[j]) {
      oblast <- 0
    } else {
      oblast <- 1
    }
    if (ass5$rajon[i]==ass5$rajon[j] & ass5$oblast[i]==ass5$oblast[j]) {
      rajon <- 0
    } else {
      rajon <- 1
    }
    cat(id1, "\t", id2, "\t", geodist, "\t", lingdist, "\t", oblast, "\t", rajon, "\n", file="admin_units.txt", append=TRUE)
  }
}

# free up memory
rm(mgeo); rm(m)
df <- read.table(file="admin_units.txt", sep="\t", header=TRUE)
# change the same/different numbers to factors for the sake of the gam
df$oblast <- factor(df$oblast, levels=c(0,1))
df$rajon <- factor(df$rajon, levels=c(0,1))

# sanity checks
# plot histogram of geographic distances for different oblasts
hist(df$geodist[df$oblast==1])
# plot histogram of geographic distances for same oblasts
hist(df$geodist[df$oblast==0])
# plot histogram of geographic distances for different rajons
hist(df$geodist[df$rajon==1])
# plot histogram of geographic distances for same rajons
hist(df$geodist[df$rajon==0])

# for a gam model for oblast exclude geographical distances beyond which
# there are no cases of same oblast
max_dist_same_oblast <- max(df[df$oblast==0,]$geodist)
df2 <- df[df$geodist <= max_dist_same_oblast,]

# run a gam (bam) model for oblast
library(mgcv)
bam_model_oblast <- bam(
  lingdist ~ oblast + s(geodist, by = oblast),
  data = df2,
  method = "fREML"
)
summary(bam_model_oblast)

# Family: gaussian 
# Link function: identity 

# Formula:
#   lingdist ~ oblast + s(geodist, by = oblast)
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.4647649  0.0007046  659.66   <2e-16 ***
#   oblast1     0.0680913  0.0007053   96.54   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(geodist):oblast0 8.865  8.989  11563  <2e-16 ***
#   s(geodist):oblast1 8.940  8.998 348931  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# R-sq.(adj) =  0.414   Deviance explained = 41.4%
# fREML = -7.0307e+06  Scale est. = 0.0055929  n = 5987768

# plot the results
library(itsadug)
plot_smooth(bam_model_oblast,
            view = "geodist",
            plot_all = "oblast",
            rug = FALSE,
            ylab = "Predicted linguistic distance",
            xlab = "Geographic distance (km)",
            col = c("blue", "red"),
            legend_plot_all = "right",
            main = "Oblast")

# Rajon
max_dist_same_rajon <- max(df[df$rajon==0,]$geodist)
df3 <- df[df$geodist <= max_dist_same_rajon,]

# run a gam (bam) model for rajon
library(mgcv)
bam_model_rajon <- bam(
  lingdist ~ rajon + s(geodist, by = rajon),
  data = df3,
  method = "fREML"
)
summary(bam_model_rajon)

# Family: gaussian 
# Link function: identity 

# Formula:
#   lingdist ~ rajon + s(geodist, by = rajon)

# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.385996   0.006163   62.63   <2e-16 ***
#   rajon1      0.011158   0.006163    1.81   0.0702 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(geodist):rajon0 7.096  7.868  340.8  <2e-16 ***
#   s(geodist):rajon1 8.796  8.975 6830.3  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# R-sq.(adj) =  0.169   Deviance explained = 16.9%
# fREML = -5.9866e+05  Scale est. = 0.0030469  n = 405121

# plot the results
library(itsadug)
plot_smooth(bam_model_rajon,
            view = "geodist",
            plot_all = "rajon",
            rug = FALSE,
            ylab = "Predicted linguistic distance",
            xlab = "Geographic distance (km)",
            col = c("blue", "red"),
            main = "Rajon",
            legend_plot_all = "right")


#################################
#ALTERNATIVE ANALYSIS USING BINS#
#################################

## simple inspection using binning
# oblast
df2$bin <- cut(df2$geodist,
               breaks = seq(0, 680, by = 20),
               include.lowest = TRUE,
               right = FALSE,
               labels = paste0(seq(0, 660, by = 20), "-", seq(20, 680, by = 20)))

# Initialize result container
library(dplyr)
summary_df_oblast <- df2 %>%
  group_by(bin) %>%
  summarise(
    oblast_dif  = round(mean(lingdist[oblast == 1], na.rm = TRUE),4),
    oblast_same = round(mean(lingdist[oblast == 0], na.rm = TRUE),4),
    p = ifelse(
      length(lingdist[oblast == 1]) > 1 &&
        length(lingdist[oblast == 0]) > 1,
      round(t.test(lingdist[oblast == 1],
                   lingdist[oblast == 0])$p.value,4),
      NA_real_
    ),
    N = n()
  ) %>%
  ungroup()

# Preview
print(summary_df_oblast)
print(as.data.frame(summary_df_oblast))
# write.table(summary_df_oblast, file="oblast_effects_bins.txt", sep="\t", quote=FALSE, row.names=FALSE)

# rajon
df3$bin <- cut(df3$geodist,
              breaks = seq(0, 140, by = 20),
              include.lowest = TRUE,
              right = FALSE,
              labels = paste0(seq(0, 120, by = 20), "-", seq(20, 140, by = 20)))

# Initialize result container
library(dplyr)
summary_df_rajon <- df3 %>%
  group_by(bin) %>%
  summarise(
    rajon_dif  = round(mean(lingdist[rajon == 1], na.rm = TRUE),4),
    rajon_same = round(mean(lingdist[rajon == 0], na.rm = TRUE),4),
    p = ifelse(
      length(lingdist[rajon == 1]) > 1 &&
        length(lingdist[rajon == 0]) > 1,
      round(t.test(lingdist[rajon == 1],
                   lingdist[rajon == 0])$p.value,4),
      NA_real_
    ),
    N = n()
  ) %>%
  ungroup()

# Preview
print(summary_df_rajon)
print(as.data.frame(summary_df_rajon))
# write.table(summary_df_rajon, file="rajon_effects_bins.txt", sep="\t", quote=FALSE, row.names=FALSE)
