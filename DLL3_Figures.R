# Package imports 
library(dplyr) # For parsing
library(ggpubr) # For pretty scatter plots; http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/

# Data is from JCO 2018 dataset (aggarwal)

# Import data. Ignore warning
dat <- read.csv("DLL3_STEAP1_EXP.csv",  header = TRUE, row.names = "samples")
dat2 <- read.csv("DLL3_FOLH1_EXP.csv",  header = TRUE, row.names = "samples")

# Convert to vectors for plotting
STEAP1 <- as.numeric(dat['STEAP1',]) # Vector with STEAP1 EXP
dll3 <- as.numeric(dat['DLL3',]) # Vector with DLL3 EXP
psma <- as.numeric(dat2['FOLH1',]) # Vector with PSMA EXP

nepc <- dat['Subtype',] # Vector with whether NEPC/AdPC

# Convert NEPC/AdPC to colors for the scatterplot
ne <- vector(mode = "list", length = 119)
for (i in 1:119){
  if (nepc[i] == 'NEPC'){ne[i] = 'red'}
  else{ne[i] = 'grey'}
}

jpeg("dll3_JCO18_data.jpg", units='in', width = 15, height = 5, res = 300)
fig1 <- gghistogram(dll3, bins = 20, fill = 'blue', xlab = 'normalized expression', ylab = '% mCRPC samples', main = 'DLL3 Expression')
fig2 <- gghistogram(psma, bins = 20, fill = 'blue', xlab = 'normalized expression', ylab = '% mCRPC samples', main = 'FOLH1 Expression')

# Plot the scatterplot of folh1 vs DLL3. Ignore the warning message. 
df <- data.frame("normalized_DLL3" = dll3, "normalized_PSMA" = psma)
fig3 <- ggscatter(df, y = "normalized_DLL3", x = "normalized_PSMA",
                  main = 'FOLH1 vs DLL3',
                  xlab = 'normalized PSMA expression',
                  ylab = 'normalized DLL3 expression',
                  add = "reg.line", # Add regression line
                  col = unlist(ne),
                  add.params = list(color = "blue",
                                    fill = "lightgray")
)+stat_cor(method = "pearson", label.x = 19, label.y = 15)+ylim(4,16)+xlim(6,24)  

# Plot the scatterplot of STEAP1 vs DLL3. Ignore the warning message. 
df <- data.frame("normalized_DLL3" = dll3, "normalized_STEAP1" = STEAP1)
fig4 <- ggscatter(df, y = "normalized_DLL3", x = "normalized_STEAP1",
                  main = 'STEAP1 vs DLL3',
                  xlab = 'normalized STEAP1 expression',
                  ylab = 'normalized DLL3 expression',
                  add = "reg.line", # Add regression line
                  col = unlist(ne),
                  add.params = list(color = "blue",
                                    fill = "lightgray")
)+stat_cor(method = "pearson", label.x = 16, label.y = 15)+ylim(4,17)+xlim(8,21)  

# Arrange for figure
ggarrange(#ggarrange(fig1, fig2, nrow = 2, labels = c("A", "B")), # First column with histograms
          ggarrange(fig3, labels = "A"), # First column with scatterplot
          ggarrange(fig4, labels = "B"), # Second column with scatterplot
          ncol = 2
) 

# Turn off
dev.off()

#######

# Import data. Ignore warning
dat <- read.csv("DLL3_STEAP1_LogTPM.csv",  header = TRUE, row.names = "IDENTIFIER")
dat2 <- read.csv("DLL3_FOLH1_LogTPM.csv",  header = TRUE, row.names = "IDENTIFIER")

# Convert to vectors for plotting
STEAP1 <- as.numeric(dat['STEAP1',]) # Vector with STEAP1 EXP
STEAP1 <- log10(STEAP1 + 1)
dll3 <- as.numeric(dat['DLL3',]) # Vector with DLL3 EXP
dll3 <- log10(dll3+1)
psma <- as.numeric(dat2['FOLH1',]) # Vector with PSMA EXP
psma <- log10(psma+1)

nepc <- dat['Subtype',] # Vector with whether NEPC/AdPC

# Convert NEPC/AdPC to colors for the scatterplot
ne <- vector(mode = "list", length = 119)
for (i in 1:119){
  if (nepc[i] == 'NEPC'){ne[i] = 'red'}
  else{ne[i] = 'grey'}
}

jpeg("dll3_Quigley18_data.jpg", units='in', width = 15, height = 5, res = 300)

# Plot the scatterplot of folh1 vs DLL3. Ignore the warning message. 
df <- data.frame("normalized_DLL3" = dll3, "normalized_PSMA" = psma)
fig5 <- ggscatter(df, y = "normalized_DLL3", x = "normalized_PSMA",
                  main = 'FOLH1 vs DLL3',
                  xlab = 'log10_PSMA (TPM)',
                  ylab = 'log10_DLL3 (TPM)',
                  add = "reg.line", # Add regression line
                  col = unlist(ne),
                  add.params = list(color = "blue",
                                    fill = "lightgray")
)+stat_cor(method = "pearson", label.x = 2.5, label.y = 2.2)+ylim(0,2.5)+xlim(0,4)  

# Plot the scatterplot of STEAP1 vs DLL3. Ignore the warning message. 
df <- data.frame("normalized_DLL3" = dll3, "normalized_STEAP1" = STEAP1)
fig6 <- ggscatter(df, y = "normalized_DLL3", x = "normalized_STEAP1",
                  main = 'STEAP1 vs DLL3',
                  xlab = 'log10_STEAP1 (TPM)',
                  ylab = 'log10_DLL3 (TPM)',
                  add = "reg.line", # Add regression line
                  col = unlist(ne),
                  add.params = list(color = "blue",
                                    fill = "lightgray")
)+stat_cor(method = "pearson", label.x = 2, label.y = 2.2)+ylim(0,2.5)+xlim(0,3.5)  



# Arrange for figure
ggarrange(ggarrange(fig5, labels = "A"), # First column with scatterplot
  ggarrange(fig6, labels = "B"), # Second column with scatterplot
  ncol = 2
) 

# Turn off
dev.off()


#######

# Import data. Ignore warning
dat <- read.csv("DLL3_STEAP1_kumar.csv",  header = TRUE, row.names = "IDENTIFIER")
dat2 <- read.csv("DLL3_FOLH1_kumar.csv",  header = TRUE, row.names = "IDENTIFIER")

# Convert to vectors for plotting
STEAP1 <- as.numeric(dat['STEAP1',]) # Vector with STEAP1 EXP
dll3 <- as.numeric(dat['DLL3',]) # Vector with DLL3 EXP
psma <- as.numeric(dat2['FOLH1',]) # Vector with PSMA EXP

nepc <- dat['Subtype',] # Vector with whether NEPC/AdPC

# Convert NEPC/AdPC to colors for the scatterplot
n = length(dat)
ne <- vector(mode = "list", length = n)
for (i in 1:n){
  if (nepc[i] == 'NEPC'){ne[i] = 'red'}
  else{ne[i] = 'grey'}
}

jpeg("dll3_Kumar16_data.jpg", units='in', width = 15, height = 5, res = 300)

# Plot the scatterplot of folh1 vs DLL3. Ignore the warning message. 
df <- data.frame("normalized_DLL3" = dll3, "normalized_PSMA" = psma)
fig7 <- ggscatter(df, y = "normalized_DLL3", x = "normalized_PSMA",
                  main = 'FOLH1 vs DLL3',
                  xlab = 'Normalized PSMA expression',
                  ylab = 'Normalized DLL3 expression',
                  add = "reg.line", # Add regression line
                  col = unlist(ne),
                  add.params = list(color = "blue",
                                    fill = "lightgray")
)+stat_cor(method = "pearson", label.x = 15, label.y = 15)+ylim(6,15)+xlim(8,20)  

# Plot the scatterplot of STEAP1 vs DLL3. Ignore the warning message. 
df <- data.frame("normalized_DLL3" = dll3, "normalized_STEAP1" = STEAP1)
fig8 <- ggscatter(df, y = "normalized_DLL3", x = "normalized_STEAP1",
                  main = 'STEAP1 vs DLL3',
                  xlab = 'Normalized STEAP1 expression',
                  ylab = 'Normalized DLL3 expression',
                  add = "reg.line", # Add regression line
                  col = unlist(ne),
                  add.params = list(color = "blue",
                                    fill = "lightgray")
)+stat_cor(method = "pearson", label.x = 15, label.y = 15)+ylim(6,15)+xlim(8,20)  

# Arrange for figure
ggarrange(ggarrange(fig7, labels = "A"), # First column with scatterplot
          ggarrange(fig8, labels = "B"), # Second column with scatterplot
          ncol = 2
) 

# Turn off
dev.off()



#######

# Import data. Ignore warning
dat <- read.csv("DLL3_STEAP1_beltran.csv",  header = TRUE, row.names = "IDENTIFIER")
dat2 <- read.csv("DLL3_FOLH1_beltran.csv",  header = TRUE, row.names = "IDENTIFIER")

# Convert to vectors for plotting
STEAP1 <- as.numeric(dat['STEAP1',]) # Vector with STEAP1 EXP
STEAP1 <- log2(STEAP1 + 1)
dll3 <- as.numeric(dat['DLL3',]) # Vector with DLL3 EXP
dll3 <- log2(dll3+1)
psma <- as.numeric(dat2['FOLH1',]) # Vector with PSMA EXP
psma <- log2(psma+1)

nepc <- dat['Subtype',] # Vector with whether NEPC/AdPC

# Convert NEPC/AdPC to colors for the scatterplot
n = length(dat)
ne <- vector(mode = "list", length = n)
for (i in 1:n){
  if (nepc[i] == 'NEPC'){ne[i] = 'red'}
  else{ne[i] = 'grey'}
}

jpeg("dll3_Beltran16_data.jpg", units='in', width = 15, height = 5, res = 300)

# Plot the scatterplot of folh1 vs DLL3. Ignore the warning message. 
df <- data.frame("normalized_DLL3" = dll3, "normalized_PSMA" = psma)
fig5 <- ggscatter(df, y = "normalized_DLL3", x = "normalized_PSMA",
                  main = 'FOLH1 vs DLL3',
                  xlab = 'log2(PSMA+1) (FPKM)',
                  ylab = 'log2(DLL3+1) (FPKM)',
                  add = "reg.line", # Add regression line
                  col = unlist(ne),
                  add.params = list(color = "blue",
                                    fill = "lightgray")
)+stat_cor(method = "pearson", label.x = 5, label.y = 7.5)+ylim(1.5,10.5)+xlim(1.5,8.5)  

# Plot the scatterplot of STEAP1 vs DLL3. Ignore the warning message. 
df <- data.frame("normalized_DLL3" = dll3, "normalized_STEAP1" = STEAP1)
fig6 <- ggscatter(df, y = "normalized_DLL3", x = "normalized_STEAP1",
                  main = 'STEAP1 vs DLL3',
                  xlab = 'log2(STEAP1+1) (FPKM)',
                  ylab = 'log2(DLL3+1) (FPKM)',
                  add = "reg.line", # Add regression line
                  col = unlist(ne),
                  add.params = list(color = "blue",
                                    fill = "lightgray")
)+stat_cor(method = "pearson", label.x = 2.5, label.y = 7.5)+ylim(1.5,10.5)+xlim(1.8,3)  

# Arrange for figure
ggarrange(ggarrange(fig5, labels = "A"), # First column with scatterplot
          ggarrange(fig6, labels = "B"), # Second column with scatterplot
          ncol = 2
) 

# Turn off
dev.off()


