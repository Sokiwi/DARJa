## Analyze the influence of rivers
x <- read.table("rivers_l.txt", sep="\t", header=TRUE)

# change number of rivers into 1 if the number is greater than 1
rivers <- as.numeric(as.logical(x$number_rivers))
# add that column to the data frame
df <- data.frame(x, rivers)
# change the presence/absence of rivers to factors for the sake of the gam
df$rivers <- factor(df$rivers, levels=c(0,1))

# run a gam (bam) model
library(mgcv)
bam_model <- bam(
  lingdist ~ rivers + s(geodist, by = rivers),
  data =df,
  method="fREML"
)
summary(bam_model)

# Family: gaussian 
# Link function: identity 

# Formula:
#   lingdist ~ rivers + s(geodist, by = rivers)

# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 0.4471159  0.0001024 4366.33   <2e-16 ***
#   rivers1     0.0050319  0.0001200   41.95   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Approximate significance of smooth terms:
#   edf Ref.df     F p-value    
# s(geodist):rivers0 8.963  9.000 22171  <2e-16 ***
#   s(geodist):rivers1 8.854  8.988 33457  <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# R-sq.(adj) =  0.268   Deviance explained = 26.8%
# fREML = -2.3349e+06  Scale est. = 0.0038799  n = 1720665

# plot the results
library(itsadug)
plot_smooth(bam_model,
            view = "geodist",
            plot_all = "rivers",
            rug = FALSE,
            ylab = "Predicted linguistic distance",
            xlab = "Geographic distance (km)",
            col = c("blue", "red"),
            legend_plot_all = "right")


#################################
#ALTERNATIVE ANALYSIS USING BINS#
#################################

max(df$geodist)  # 299
df$bin <- cut(df$geodist,
               breaks = seq(0, 300, by = 30),
               include.lowest = TRUE,
               right = FALSE,
               labels = paste0(seq(0, 270, by = 30), "-", seq(30, 300, by = 30)))

# Initialize result container
library(dplyr)
summary_df <- df %>%
  group_by(bin) %>%
  summarise(
    river_pres  = round(mean(lingdist[rivers == 1], na.rm = TRUE),4),
    river_abs = round(mean(lingdist[rivers == 0], na.rm = TRUE),4),
    p = ifelse(
      length(lingdist[rivers == 1]) > 1 &&
        length(lingdist[rivers == 0]) > 1,
      round(t.test(lingdist[rivers == 1],
                   lingdist[rivers == 0])$p.value,4),
      NA_real_
    ),
    N = n()
  ) %>%
  ungroup()

# Preview
print(summary_df)
print(as.data.frame(summary_df))
# write.table(summary_df, file="river_effects_bins.txt", sep="\t", quote=FALSE, row.names=FALSE)
