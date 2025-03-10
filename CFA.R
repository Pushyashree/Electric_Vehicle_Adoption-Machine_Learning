#Confirmatory factor analysis

if (!require("lavaan")) install.packages("lavaan", dependencies = TRUE)
library(lavaan)

if (!require("readr")) install.packages("readr", dependencies = TRUE)
library(readr)
df <- read_csv("cfa_data.csv")

# Hypothesis and Latent Variables Definition
# Hypothesis:
# - Environmental concern is measured by five indicators (envi_concern1 to envi_concern5).
# - Knowledge about microcars is measured by three indicators (knowledge1, knowledge2, knowledge3).
# - Car usage intensity is measured by five indicators (car_usage, daily_ttd, car_usage_ld, yearly_ttd, no_cars).

# Define the CFA model with three latent variables
# Each latent variable is specified with its corresponding observed indicators.
model <- '
  # Define latent variables and their indicators
  Environmental_Concern =~ envi_concern1 + envi_concern2 + envi_concern3 + envi_concern5
  #Knowledge_Microcars =~ knowledge1 + knowledge2 + knowledge3
  Car_Usage_Intensity =~ car_usage + daily_ttd + car_usage_ld + yearly_ttd + no_cars
'
#Additional: Knowledge_Microcars =~ knowledge2

cor_matrix <- cor(df, use = "pairwise.complete.obs")
print(cor_matrix)

# Fit the CFA model to the data
# cfa() is a function in lavaan that performs Confirmatory Factor Analysis
fit <- cfa(model, data = df)

# Display a summary of the model fit
# - fit.measures = TRUE provides fit indices like CFI, TLI, RMSEA, and SRMR
# - standardized = TRUE provides standardized estimates for easier interpretation
summary(fit, fit.measures = TRUE, standardized = TRUE)

# Check fit indices
# The key indices include:
# - CFI (Comparative Fit Index): Values above 0.90 or 0.95 indicate good fit.
# - TLI (Tucker-Lewis Index): Similar to CFI, with values closer to 1 indicating a good fit.
# - RMSEA (Root Mean Square Error of Approximation): Values below 0.08 indicate acceptable fit, with below 0.05 being ideal.
# - SRMR (Standardized Root Mean Square Residual): Values below 0.08 indicate a good fit.
fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr"))

# Extract and view factor loadings for each indicator on its latent variable
# Higher loadings (close to 1) suggest that the indicator strongly represents the latent variable
standardizedSolution(fit)

# If fit indices are not satisfactory, examine modification indices
# Modification indices suggest potential adjustments to improve fit, such as correlating error terms
# Only make modifications if they are theoretically justified
#modification_indices <- modindices(fit)
#head(modification_indices[order(-modification_indices$mi), ])  # Show top modifications

# Extract factor scores for each latent variable
# These scores can be used in further analyses (e.g., regression or clustering)
factor_scores <- lavPredict(fit)


# Save factor scores to a CSV file for future use if needed
#write.csv(factor_scores, "factor_scores.csv", row.names = FALSE)

# Plot path diagram (optional) if you want to visualize the CFA model
# Install the semPlot package for creating diagrams
#if (!require("semPlot")) install.packages("semPlot", dependencies = TRUE)
library(semPlot)
semPaths(fit, "std", layout = "tree", whatLabels = "std", edge.label.cex = 0.8)

library(semPlot)
library(ggplot2)
library(readr)
# Plot the distribution of Environmental Concern
ggplot(factor_scores, aes(x = Environmental_Concern)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "blue", size = 1) +
  labs(title = "Distribution of Environmental Concern Scores", x = "Environmental Concern", y = "Density") +
  theme_minimal()

# Plot the distribution of Car Usage Intensity
ggplot(factor_scores, aes(x = Car_Usage_Intensity)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "blue", size = 1) +
  labs(title = "Distribution of Car Usage Intensity Scores", x = "Car Usage Intensity", y = "Density") +
  theme_minimal()



library(dplyr)
factor_scores_df <- as.data.frame(factor_scores)

# Separate individuals based on positive or negative scores for each latent variable
positive_car_usage <- factor_scores_df %>% filter(Car_Usage_Intensity > 0)
negative_car_usage <- factor_scores_df %>% filter(Car_Usage_Intensity < 0)

positive_envi_concern <- factor_scores_df %>% filter(Environmental_Concern > 0)
negative_envi_concern <- factor_scores_df %>% filter(Environmental_Concern < 0)

# Summary Statistics for Interpretation
summary_positive_car <- summary(positive_car_usage$Car_Usage_Intensity)
summary_negative_car <- summary(negative_car_usage$Car_Usage_Intensity)

summary_positive_envi <- summary(positive_envi_concern$Environmental_Concern)
summary_negative_envi <- summary(negative_envi_concern$Environmental_Concern)

# Print summaries for interpretation
print("Positive Car Usage Intensity Summary:")
print(summary_positive_car)
print("Negative Car Usage Intensity Summary:")
print(summary_negative_car)

print("Positive Environmental Concern Summary:")
print(summary_positive_envi)
print("Negative Environmental Concern Summary:")
print(summary_negative_envi)




library(lavaan)
library(readr)
df <- read_csv("Processed_data_final.csv")

# Define the model with covariances
model <- ' 
  Performance_EV =~ Range_EV + TopSpeed_EV
  Performance_MEV =~ Range_MEV + TopSpeed_MEV
  Range_EV ~~ TopSpeed_EV
  Range_MEV ~~ TopSpeed_MEV
'

# Fit the model
fit <- cfa(model, data = df_cfa)

# Summary of results
summary(fit, fit.measures = TRUE, standardized = TRUE)

# Fit indices
fitMeasures(fit, c("cfi", "tli", "rmsea", "srmr"))


# Extract and view factor loadings for each indicator on its latent variable
# Higher loadings (close to 1) suggest that the indicator strongly represents the latent variable
standardizedSolution(fit)
factor_scores <- lavPredict(fit)

#write.csv(factor_scores, "factor_scores.csv", row.names = FALSE)
library(semPlot)
semPaths(fit, "std", layout = "tree", whatLabels = "std", edge.label.cex = 0.8)

library(semPlot)
library(ggplot2)
library(readr)
ggplot(factor_scores, aes(x = Performance_EV)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "blue", size = 1) +
  labs(title = "Distribution of Performance_EV Scores", x = "Performance_EV", y = "Density") +
  theme_minimal()

ggplot(factor_scores, aes(x = Performance_MEV)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "blue", size = 1) +
  labs(title = "Distribution of Performance_MEV Scores", x = "Performance_MEV", y = "Density") +
  theme_minimal()
