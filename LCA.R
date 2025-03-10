library(dplyr)
if (!require("poLCA")) install.packages("poLCA")
library(poLCA)
library(ggplot2)

#Use:  C:\Users\User15\OneDrive\Desktop\Final Thesis\Model\HCM\LCM\LCA_Categorical_data.csv
data <- read.csv("LCA_Categorical_data.csv")

class_vars <- data #[, which(names(data) == "age_c"):ncol(data)]  # Modify as per your column names
class_vars <- class_vars %>% mutate(across(everything(), as.factor))  # Convert to factors

#Formula for Latent Class Analysis (LCA)
formula <- as.formula(paste("cbind(", paste(names(class_vars), collapse = ", "), ") ~ 1"))

results <- list()
aic_values <- c()
bic_values <- c()
set.seed(123)

for (k in 2:6) {
  cat("Estimating model with", k, "classes...\n")
  lca_model <- poLCA(formula, data = class_vars, nclass = k, na.rm = TRUE, maxiter = 1000, tol = 1e-5)
  results[[paste0("Class_", k)]] <- lca_model
  aic_values[k] <- lca_model$aic
  bic_values[k] <- lca_model$bic
}

#Model Fit
metrics <- data.frame(
  Classes = 2:6,  
  AIC = aic_values[2:6],
  BIC = bic_values[2:6]
)

print(metrics)

#ggplot(metrics, aes(x = Classes)) +
#  geom_line(aes(y = AIC, color = "AIC")) +
#  geom_line(aes(y = BIC, color = "BIC")) +
#  labs(title = "Model Fit for Different Classes",
#       x = "Number of Classes", y = "Criterion Value") +
#  scale_color_manual(values = c("AIC" = "blue", "BIC" = "red"))

library(ggplot2)
ggplot(metrics, aes(x = Classes)) +
  geom_line(aes(y = AIC, color = "AIC")) +
  geom_line(aes(y = BIC, color = "BIC")) +
  geom_point(aes(y = AIC, color = "AIC")) +
  geom_point(aes(y = BIC, color = "BIC")) +
  labs(title = "Model Fit for Different Classes",
       x = "Number of Classes", y = "Criterion Value") +
  theme_minimal() +
  scale_color_manual(values = c("AIC" = "blue", "BIC" = "red"))

#Entropy
entropy_results <- data.frame(
  Classes = integer(),
  Entropy = numeric()
)

for (k in names(results)) {
  chosen_model <- results[[k]]
  
  #posterior probabilities
  posterior_probs <- chosen_model$posterior
  
  # Calculate entropy
  entropy <- 1 - sum(posterior_probs * log(posterior_probs), na.rm = TRUE) / nrow(posterior_probs)
  
  # Store entropy value
  entropy_results <- rbind(entropy_results, data.frame(Classes = as.numeric(gsub("Class_", "", k)), Entropy = entropy))
}

print(entropy_results)
ggplot(entropy_results, aes(x = Classes, y = Entropy)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Entropy for Different Class Solutions",
       x = "Number of Classes",
       y = "Entropy") +
  theme_minimal()

#posterior probabilities
posterior_probs <- chosen_model$posterior

# proportion of individuals in each class
class_population <- colSums(posterior_probs) / nrow(posterior_probs)
class_percentages <- class_population * 100
class_percentages
cumulative_percentages <- cumsum(class_percentages)
cumulative_percentages


#Class-Specific Probabilities for Variables
selected_class <- 4
class_probs <- results[[paste0("Class_", selected_class)]]$probs
summary_table <- list()

for (class in 1:length(class_probs[[1]][, 1])) {  # Iterate over classes
  class_summary <- data.frame()
  
  for (var in names(class_probs)) {  # Iterate over variables
    if (ncol(class_probs[[var]]) > 1) {  # If the variable has multiple categories
      for (category in 1:ncol(class_probs[[var]])) {  # Iterate over categories
        class_summary <- rbind(
          class_summary,
          data.frame(
            Variable = var,
            Category = paste(var, "=", category - 1),  # Adjust for binary encoding
            Prob = class_probs[[var]][class, category]
          )
        )
      }
    }
  }
  summary_table[[paste0("Class_", class)]] <- class_summary
}

threshold <- 0.5
defining_vars_with_categories <- list()

for (class in 1:length(summary_table)) {
  class_data <- summary_table[[paste0("Class_", class)]]
  defining_vars_with_categories[[paste0("Class_", class)]] <- class_data %>%
    dplyr::filter(Prob > threshold) %>%  # Apply threshold to filter relevant rows
    dplyr::mutate(Category_Refined = paste0(Variable, " = ", gsub(".*= ", "", Category))) %>%  # Refine category formatting
    dplyr::select(Category_Refined)  # Keep only the 'Category_Refined' column
}

for (class in names(defining_vars_with_categories)) {
  cat("\nDefining categories for", class, ":\n")
  print(defining_vars_with_categories[[class]])
}

mapping <- list()
categorical_vars <- c("education_c", "gender_c", "employment_c", "Family_cluster_c", "age_c", "region_c", "income_c")  # List your categorical variables

for (var in categorical_vars) {
  # Ensure the variable is a factor before creating mapping
  if (!is.factor(data[[var]])) {
    data[[var]] <- as.factor(data[[var]])
  }
  
  mapping[[var]] <- data.frame(
    Code = seq_along(levels(data[[var]])),  # Generate sequential numeric codes for levels
    Category = levels(data[[var]])  # Capture the actual category names
  )
}

for (var in names(mapping)) {
  cat("\nMapping for", var, ":\n")
  print(mapping[[var]])
}


#Update Summary Table with Mappings
for (class in 1:length(summary_table)) {
  class_data <- summary_table[[paste0("Class_", class)]]
  
  for (var in categorical_vars) {
    if (var %in% class_data$Variable) {
      mapping_table <- mapping[[var]]
      class_data <- class_data %>%
        mutate(
          Category = ifelse(Variable == var,
                            mapping_table$Category[as.numeric(gsub(".*= ", "", Category)) + 1],
                            Category)
        )
    }
  }
  summary_table[[paste0("Class_", class)]] <- class_data
}

for (class in names(summary_table)) {
  cat("\nUpdated Summary for", class, ":\n")
  print(summary_table[[class]])
}

threshold <- 0.4
defining_vars_with_categories <- list()

for (class in 1:length(summary_table)) {
  class_data <- summary_table[[paste0("Class_", class)]]
  defining_vars_with_categories[[paste0("Class_", class)]] <- class_data %>%
    dplyr::filter(Prob > threshold) %>%  # Apply threshold to filter relevant rows
    dplyr::mutate(
      Category_Refined = paste0(Variable, " = ", gsub(".*= ", "", Category)),  # Refine category formatting
      Loading = Prob  # Include the probabilities (loadings)
    ) %>%
    dplyr::select(Category_Refined, Loading)  # Keep refined category and loading
}

for (class in names(defining_vars_with_categories)) {
  cat("\nDefining categories with loadings for", class, ":\n")
  print(defining_vars_with_categories[[class]])
}

# Inspect the original variables and formula for debugging
#print(head(class_vars))  # Inspect selected variables
#print(formula)  # Inspect the formula used for LCA
#any(is.null(database))


selected_class <- "Class_4"
chosen_model <- results[[selected_class]]

# Extract posterior probabilities
posterior_probs <- chosen_model$posterior
# Calculate entropy
entropy <- 1 - sum(posterior_probs * log(posterior_probs), na.rm = TRUE) / nrow(posterior_probs)

cat("Entropy for", selected_class, ":", entropy, "\n")

# Calculate Class Percentages
class_population <- colSums(posterior_probs) / nrow(posterior_probs)
class_percentages <- class_population * 100
cat("Class Percentages for", selected_class, ":\n")
print(class_percentages)
cumulative_percentages <- cumsum(class_percentages)
cat("Cumulative Percentages for", selected_class, ":\n")
print(cumulative_percentages)


library(ggplot2)
class_percentages_df <- data.frame(
  Class = seq_len(length(class_percentages)),
  Percentage = class_percentages
)

ggplot(class_percentages_df, aes(x = Class, y = Percentage)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = round(Percentage, 1)), vjust = -0.5) +
  labs(title = paste("Class Percentages"),
       x = "Class",
       y = "Percentage") +
  theme_minimal()


data$class <- apply(chosen_model$posterior, 1, which.max)
head(data$class)

# Check maximum posterior probabilities for individuals
max_probs <- apply(chosen_model$posterior, 1, max)
summary(max_probs)  # Summarize confidence in class assignment
table(data$class)

low_confidence <- data[max_probs < 0.75, ]
#print(low_confidence)
# Calculate total individuals with max_probs less than 0.75
low_confidence_count <- sum(max_probs < 0.60)
cat("Total number of individuals with max posterior probability < 0.60:", low_confidence_count, "\n")




#Check class assignment error matrix
#'posterior_probs' is a matrix of posterior probabilities P(C = j | Y) for each individual
# and 'assigned_class' is a vector of modal class assignments W for each individual

n_classes <- ncol(posterior_probs)

Q_matrix <- matrix(0, nrow = n_classes, ncol = n_classes)

# Calculate P(C = j | W = i)
P_C_given_W <- matrix(0, nrow = n_classes, ncol = n_classes)
N_i <- table(data$class) 

for (i in 1:n_classes) {
  for (j in 1:n_classes) {
    # Individuals assigned to class i
    assigned_to_i <- which(data$class == i)
    if (length(assigned_to_i) > 0) {
      # Average posterior probability of true class j among those assigned to class i
      P_C_given_W[j, i] <- mean(posterior_probs[assigned_to_i, j])
    }
  }
}

# Calculate Q matrix
for (j in 1:n_classes) {
  denominator <- sum(P_C_given_W[j, ] * N_i)
  for (i in 1:n_classes) {
    Q_matrix[j, i] <- (P_C_given_W[j, i] * N_i[i]) / denominator
  }
}

# classification error matrix (Q)
print(Q_matrix)
#row_sums <- rowSums(Q_matrix)
#print(row_sums)
#As diagonal elements are above 80% class error matrix has not been incorporated as weights for individuals.

class_column <- data.frame(Class = data$class)
#write.csv(class_column, "assigned_class_LCA.csv", row.names = FALSE)


