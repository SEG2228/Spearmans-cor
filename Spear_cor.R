# Load the necessary library
library(readr)

# Load the CSV file
data <- read_csv("C:/Users/susie/OneDrive/Desktop/Sprearmans.csv")

#Define the variables of interest, including PC1 and PC2
variables <- c("PC1", "PC2", "DMS", "CH4", "CO2", "DMSP_consumed", "Shannon","Simpson","Telmatospirillum","Paludibacterium", "Paraburkholderia", "Pseudomonas")

# Calculate the Spearman correlation matrix
cor_matrix <- cor(data[variables], method = "spearman")

# Function to compute p-values for the correlation matrix
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], method = "spearman")
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  return(p.mat)
}

# Calculate the p-values matrix
p_matrix <- cor.mtest(data[variables])

# Load necessary libraries
library(corrplot)
library(RColorBrewer)

# Assuming cor_matrix and p_matrix are already calculated
# Adjust the text size if needed
corrplot(cor_matrix, method = "circle", type = "upper",
         col = colorRampPalette(brewer.pal(8, "RdBu"))(200),
         tl.col = "black", tl.cex = 1.2, # Adjusted for approximately 12-point font
         addCoef.col = "black", # Add correlation coefficients
         number.cex = 1, # Adjusted for approximately 12-point font
         p.mat = p_matrix, # p-values matrix
         sig.level = c(0.001, 0.01, 0.05), # Define significance levels
         insig = "label_sig", # Label insignificant correlations
         pch.cex = 2, # Size of the significance symbols
         pch.col = "white", # Color of the significance symbols
         diag = FALSE) # Hide the diagonal




