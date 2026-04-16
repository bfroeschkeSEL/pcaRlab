############################################################
# PCA + REGRESSION LAB
# Author: Bridgette Froeschke
# Course: Statistics
############################################################

# =========================
# 1. LOAD LIBRARIES
# =========================
# Install if needed:
# install.packages(c("tidyverse", "ggrepel"))

library(tidyverse)
library(ggrepel)

# =========================
# 2.DATA
# =========================
##fish_model


# =========================
# 3. PREPARE DATA FOR PCA
# =========================
# PCA should only include numeric predictor variables

pca_vars <- fish_model %>%
  select(Temp, Salinity, Depth, Turbidity, Chlorophyll)

# IMPORTANT:
# We standardize the data because variables are on different units

pca_scaled <- scale(pca_vars)

# =========================
# 4. RUN PCA
# =========================
pca_model <- prcomp(
  pca_scaled,   # Input data (scaled numeric variables)
  center = TRUE,  # Subtracts the mean from each variable
  scale. = TRUE   # Divides each variable by its standard deviation
)

# -------------------------
# WHAT THIS DOES:
# -------------------------
# prcomp() performs Principal Component Analysis by:
#
# 1. Taking your variables (Temp, Salinity, etc.)
# 2. Centering them (mean = 0)
# 3. Scaling them (variance = 1)
# 4. Rotating the dataset into new axes (PC1, PC2, PC3...)
#
# These new axes:
# - PC1 = direction of greatest variance
# - PC2 = second greatest variance (perpendicular to PC1)
#
# The output contains:
# - pca_model$x        → SCORES (where samples fall)
# - pca_model$rotation → LOADINGS (variable influence)
# - pca_model$sdev     → variance explained


# View summary (variance explained)
summary(pca_model)

# =========================
# 5. EXTRACT SCORES & LOADINGS
# =========================

# SCORES = positions of samples
scores <- as.data.frame(pca_model$x)

# Add grouping variable back in
scores$Habitat <- fish_model$Habitat

# LOADINGS = contribution of variables
loadings <- as.data.frame(pca_model$rotation)

# Add variable names
loadings$Variable <- rownames(loadings)

# =========================
# 6. SCALE LOADINGS FOR PLOTTING
# =========================
# Loadings are often scaled up for visualization

loadings_scaled <- loadings %>%
  mutate(PC1 = PC1 * 3,
         PC2 = PC2 * 3)

# =========================
# 7. CREATE CONVEX HULLS (POLYGONS)
# =========================
# This creates outlines around groups

hulls <- scores %>%
  group_by(Habitat) %>%
  slice(chull(PC1, PC2))

# =========================
# 8. PCA BIPLOT WITH GGPLOT
# =========================

ggplot() +
  
  # POLYGONS (group structure)
  geom_polygon(data = hulls,
               aes(x = PC1, y = PC2, fill = Habitat),
               alpha = 0.2) +
  
  # SAMPLE POINTS (scores)
  geom_point(data = scores,
             aes(x = PC1, y = PC2, color = Habitat),
             size = 3) +
  
  # LOADINGS ARROWS
  geom_segment(data = loadings_scaled,
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black") +
  
  # LABEL LOADINGS
  geom_text_repel(data = loadings_scaled,
                  aes(x = PC1, y = PC2, label = Variable),
                  size = 4) +
  
  theme_minimal() +
  labs(title = "PCA Biplot",
       x = "PC1",
       y = "PC2")

# =========================
# 9. INTERPRETATION NOTES
# =========================

#  NOTE:
# - Points = samples (scores)
# - Arrows = variables (loadings)
# - Direction of arrows = increasing value of that variable
# - Length of arrows = strength of influence
# - Polygons = grouping patterns (habitat types)

# =========================
# 10. USING PCA FOR REGRESSION
# =========================

# Approach 1:
# Use PCA SCORES as predictors

reg_data <- scores %>%
  select(PC1, PC2) %>%
  mutate(Fish_Abundance = fish_model$Fish_Abundance)

model_pca <- lm(Fish_Abundance ~ PC1 + PC2, data = reg_data)

summary(model_pca)

# =========================
# 11. USING LOADINGS TO INTERPRET REGRESSION
# =========================

# The loadings tell us WHAT PC1 and PC2 represent

loadings[, c("Variable", "PC1", "PC2")]

# Example interpretation:
#  PC1 strongly loads on Temp, Turbidity and Chlorophyll,
# then a significant PC1 in regression means:
# "Fish abundance is related to Temp + Turbidity + Chlorophyll gradient"

ggplot(reg_data, aes(x = PC1, y = PC2, color = Fish_Abundance)) +
  geom_point(size = 3) +
  scale_color_viridis_c(option = "D", end = 0.95) +
  theme_minimal() +
  labs(title = "Fish Abundance across PCA Space",
       color = "Abundance")

reg_data$Abundance_Group <- cut(reg_data$Fish_Abundance,
                                breaks = 4,
                                labels = c("Low", "Med-Low", "Med-High", "High"))

ggplot(reg_data, aes(x = PC1, y = PC2, color = Abundance_Group)) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(title = "Fish Abundance Categories across PCA Space",
       color = "Abundance Level")
# =========================
# 12. ORIGINAL VARIABLE REGRESSION
# =========================

model_original <- lm(Fish_Abundance ~ Temp + Salinity + Depth +
                       Turbidity + Chlorophyll,
                     data = fish_model)

summary(model_original)

# =========================
# 13. COMPARISON DISCUSSION
# =========================

# Compare:
# - PCA regression (PC1, PC2)
# vs.
# - Original regression (raw variables)

# Key questions:
# 1. Which model is easier to interpret?
# 2. Which avoids multicollinearity?
# 3. What do the PCs represent biologically?




############################################################
# END OF LAB
############################################################