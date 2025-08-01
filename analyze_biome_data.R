library(mgcv)  # bam()

df <- read.table(file="biomes3.txt", header=TRUE, strip.white=TRUE)

# log transform
df_logged <- df
df_logged$log_geodist <- log(df_logged$geodist + 1)  # add 1 to avoid log(0)
model_log <- lm(lingdist ~ log_geodist * biome_difference, data = df_logged)
summary(model_log)

# best model to date
# requires first turning biome_difference into factors
df$biome_difference <- factor(df$biome_difference, levels = c(0, 1))

bam_model <- bam(
  lingdist ~ biome_difference + s(geodist, by = biome_difference),
  data = df, 
  method = "fREML"
)
summary(bam_model)

# Family: gaussian 
# Link function: identity 

# Formula:
#   lingdist ~ biome_difference + s(geodist, by = biome_difference)
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       5.559e-01  3.652e-05 15222.7   <2e-16 ***
#   biome_difference1 2.670e-02  5.646e-05   472.9   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df      F p-value    
# s(geodist):biome_difference0 8.987      9 450911  <2e-16 ***
#   s(geodist):biome_difference1 8.979      9 349590  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# R-sq.(adj) =  0.529   Deviance explained = 52.9%
# fREML = -1.0322e+07  Scale est. = 0.0055891  n = 8788528

library(itsadug)
plot_smooth(bam_model,
            view = "geodist",
            plot_all = "biome_difference",
            rug = FALSE,
            main = "Effect of Distance by Biome Difference",
            ylab = "Predicted Linguistic Distance",
            xlab = "Geographic Distance (km)",
            col = c("blue", "red"), 
            legend = TRUE)


# simple inspection using binning
df$bin <- cut(df$geodist,
              breaks = seq(0, 1440, by = 30),
              include.lowest = TRUE,
              right = FALSE,
              labels = paste0(seq(0, 1410, by = 30), "-", seq(30, 1440, by = 30)))

# Initialize result container
library(dplyr)

summary_df <- df %>%
  group_by(bin) %>%
  summarise(
    biome_dif  = round(mean(lingdist[biome_difference == 1], na.rm = TRUE),4),
    biome_same = round(mean(lingdist[biome_difference == 0], na.rm = TRUE),4),
    p = ifelse(
      length(lingdist[biome_difference == 1]) > 1 &&
        length(lingdist[biome_difference == 0]) > 1,
      round(t.test(lingdist[biome_difference == 1],
             lingdist[biome_difference == 0])$p.value,4),
      NA_real_
    ),
    N = n()
  ) %>%
  ungroup()

# Preview
print(summary_df)
print(as.data.frame(summary_df))
write.table(summary_df, file="biome_effects_bins.txt", sep="\t", quote=FALSE, row.names=FALSE)

