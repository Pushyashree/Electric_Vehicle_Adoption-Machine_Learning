#Latent vraible Model
#if (!require("lavaan")) install.packages("lavaan")
#if (!require("semPlot")) install.packages("semPlot")
#if (!require("corrplot")) install.packages("corrplot")
library(corrplot)
library(lavaan)
library(semPlot)

data <- read.csv("Processed_data_final.csv")
head(data)

# Define the CFA model
env_concern_vars <- data[, c("envi_concern1", "envi_concern2", "envi_concern3", "envi_concern5")]
cor_env_concern <- cor(env_concern_vars, use = "complete.obs")
print(cor_env_concern)
corrplot(cor_env_concern, method = "circle", type = "upper", tl.cex = 0.8, tl.col = "black")

envi_concern_model <- '
  Environmental_Concern_latent =~ envi_concern1 + envi_concern2 + envi_concern3 + envi_concern5
  envi_concern1 ~~ envi_concern5
'
fit_envi <- cfa(envi_concern_model, data = data)
summary(fit_envi, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit_envi, c("cfi", "tli", "rmsea", "srmr"))

# Knowledge of Microcar CFA
# Subset indicators for Knowledge of Microcar
#knowledge_vars <- data[, c("knowledge1.1", "knowledge2.1", "knowledge3.1")]
#cor_knowledge <- cor(knowledge_vars, use = "complete.obs")
#print(cor_knowledge)
#corrplot(cor_knowledge, method = "circle", type = "upper", tl.cex = 0.8, tl.col = "black")

#knowledge_model <- '
#  Knowledge_Microcar =~ knowledge1.1 + knowledge2.1 + knowledge3.1
#'
#fit_knowledge <- cfa(knowledge_model, data = data)
#summary(fit_knowledge, fit.measures = TRUE, standardized = TRUE)
#fitMeasures(fit_knowledge, c("cfi", "tli", "rmsea", "srmr"))

#Car Usage Intensity CFA
#Subset indicators forr Car Usage Intensity indicators
car_usage_vars <- data[, c("car_usage", "daily_ttd", "car_usage_ld", "yearly_ttd", "no_cars")]
cor_car_usage <- cor(car_usage_vars, use = "complete.obs")
print(cor_car_usage)
corrplot(cor_car_usage, method = "circle", type = "upper", tl.cex = 0.8, tl.col = "black")

car_usage_model <- '
  Car_Usage_Intensity_latent =~ car_usage + daily_ttd + car_usage_ld + yearly_ttd + no_cars
  daily_ttd ~~ car_usage
  daily_ttd ~~ yearly_ttd
  car_usage_ld ~~ car_usage
  car_usage_ld ~~ yearly_ttd
'
fit_car_usage <- cfa(car_usage_model, data = data)
summary(fit_car_usage, fit.measures = TRUE, standardized = TRUE)
fitMeasures(fit_car_usage, c("cfi", "tli", "rmsea", "srmr"))

#Continuing with Environmental concern and Car usage intensity

data$Environmental_Concern_latent <- lavPredict(fit_envi, type = "lv")[, "Environmental_Concern_latent"]
data$Car_Usage_Intensity_latent <- lavPredict(fit_car_usage, type = "lv")[, "Car_Usage_Intensity_latent"]

summary(data$Environmental_Concern_latent)
summary(data$Car_Usage_Intensity_latent)
hist(data$Environmental_Concern_latent, main = "Environmental Concern Scores", xlab = "Scores")
hist(data$Car_Usage_Intensity_latent, main = "Car Usage Intensity Scores", xlab = "Scores")

#if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

# Plot the distribution for Environmental Concern
ggplot(data, aes(x = Environmental_Concern_latent)) +
  geom_density(fill = "blue", alpha = 0.4) +
  geom_line(stat = "density", color = "blue") +
  labs(title = "Distribution of Environmental Concern Scores",
       x = "Scores", y = "Density") +
  theme_minimal()

# Plot the distribution for Car Usage Intensity
ggplot(data, aes(x = Car_Usage_Intensity_latent)) +
  geom_density(fill = "green", alpha = 0.4) +
  geom_line(stat = "density", color = "green") +
  labs(title = "Distribution of Car Usage Intensity Scores",
       x = "Scores", y = "Density") +
  theme_minimal()


# Define the SEM
sem_model <- '
  # Latent variable definitions
  Environmental_Concern_FS =~ envi_concern1 + envi_concern2 + envi_concern3 + envi_concern5
  Car_Usage_Intensity_FS =~ car_usage + daily_ttd + car_usage_ld + yearly_ttd + no_cars
  
  # Correlated residuals for Environmental Concern
  envi_concern1 ~~ envi_concern5
  
  # Correlated residuals for Car Usage Intensity
  daily_ttd ~~ car_usage
  daily_ttd ~~ yearly_ttd
  car_usage_ld ~~ car_usage
  car_usage_ld ~~ yearly_ttd

  # Structural relationships
  Environmental_Concern_FS ~    education + region_Rural +region_Sub.urban 
                                 
  Car_Usage_Intensity_FS ~ gender_Male+ Employed.full.time +
                                 Employed.marginally..11.18.hr.w.+
                                 Housewife.Househusband +
                                 Employed_part.time +
                                 Pensioners +
                                 Students + region_Rural +region_Sub.urban +region_Urban+ 
                                 age_45.59.years+age_60.64.years+age_65.years.and.more+
                                 education_No.qualification+ education_Other.degree+ education_Primary.or.secondary+
                                 income_2000.to.3000.euros+ income_2000.to.3000.euros+ income_3000.to.4000.euros+ income_4000.to.5000.euros+ income_5000.to.6000.euros
                                 + income_500.to.1500.euros+ income_6000.to.7000.euros+ income_Less.than.500.euros
'

fit_sem <- sem(sem_model, data = data)
summary(fit_sem, fit.measures = TRUE, standardized = TRUE)

# Visualize the SEM
semPaths(fit_sem, what = "std", edge.label.cex = 1, layout = "tree")
fitMeasures(fit_sem, c("cfi", "tli", "rmsea", "srmr"))

# Extract factor scores
data$Environmental_Concern <- lavPredict(fit_sem, type = "lv")[, "Environmental_Concern_FS"]
data$Car_Usage_Intensity <- lavPredict(fit_sem, type = "lv")[, "Car_Usage_Intensity_FS"]

# Check the scores
summary(data$Environmental_Concern)
summary(data$Car_Usage_Intensity)

#visualize their distributions
ggplot(data, aes(x = Environmental_Concern)) +
  geom_density(fill = "blue", alpha = 0.4) +
  labs(title = "Distribution of Environmental Concern Scores", x = "Scores", y = "Density") +
  theme_minimal()

ggplot(data, aes(x = Car_Usage_Intensity)) +
  geom_density(fill = "green", alpha = 0.4) +
  labs(title = "Distribution of Car Usage Intensity Scores", x = "Scores", y = "Density") +
  theme_minimal()

# Save the updated data with latent scores to a CSV file
#write.csv(data, "Data_with_Latent_Scores.csv", row.names = FALSE)

#Mixed Logit Model

library(dplyr)
library(apollo)

data <- read.csv("Data_with_Latent_Scores.csv")

# Replace choice labels
data$Choice <- recode(data$Choice, "Choice A" = "Microcar", "Choice B" = "Electric_car")

# Exclude "No electric"
data <- data %>% filter(Choice != "No electric")

# Create dummy variables for Family_cluster
data <- data %>% mutate(across(starts_with("Family_cluster"), as.factor)) %>% 
  model.matrix(~ Family_cluster - 1, data = .) %>%
  bind_cols(data)
#write.csv(data, "Processed_data_MLM.csv", row.names = FALSE)


#correlations = data[['PurchasePrice_MEV', 'PurchasePrice_EV', 'Range_MEV', 'Range_EV', 'TopSpeed_MEV', 'TopSpeed_EV']].corr()
#print(correlations)

#class(database$Choice)
#sum(is.na(database$Choice))
#unique(database$Choice)


#Clean further and use MLM data
#install.packages("apollo")
library(apollo)
apollo_initialise()

data <- read.csv("Processed_data_MNM.csv")
database <- data
database <- database[order(database$id), ]

# Map string values to numeric codes
database$Choice <- ifelse(database$Choice == "Microcar", 1,
                          ifelse(database$Choice == "Electric_car", 2,
                                 ifelse(database$Choice == "None of the above", 3, NA)))
#database$Choice <- as.numeric(database$Choice)

database$PurchasePrice_MEV <- database$PurchasePrice_MEV / 1000
database$PurchasePrice_EV <- database$PurchasePrice_EV / 1000
database$Range_MEV <- database$Range_MEV / 10
database$Range_EV <- database$Range_EV / 10

table(database$Choice)
prop.table(table(database$Choice))

#head(database$Choice)

# Define Apollo control settings for MLM
apollo_control <- list(
  modelName = "MLM_Microcar_EV",
  modelDescr = "Mixed Logit Model for Microcar vs Electric Car",
  indivID = "id",
  mixing = TRUE,
  nCores = 1,    
  outputDirectory = "output"
)

# Interaction terms
database$Price_Range_MEV <- database$PurchasePrice_MEV * database$Range_MEV
database$Price_speed_EV <- database$PurchasePrice_EV * database$TopSpeed_EV
database$Price_Safety_MEV <- database$PurchasePrice_MEV * database$Safety_MEV

apollo_draws <- list(
  interDrawsType = "halton", 
  interNDraws = 10,       
  interUnifDraws = c(),    
  interNormDraws = c("draws_price" , "draws_swapping")
)
# Define Random Coefficients
apollo_randCoeff <- function(apollo_beta, apollo_inputs) {
  randcoeff <- list(
    b_price_MEV = beta_price_MEV + draws_price * sigma_price_MEV,
    b_price_EV = beta_price_EV + draws_price * sigma_price_EV,
    #b_price_safety_MEV = beta_price_safety_MEV + draws_price * sigma_price_safety_MEV,
    #b_price_speed_EV = beta_price_speed_EV + draws_price * sigma_price_speed_EV
    b_swapping_MEV = beta_swapping_MEV + draws_swapping * sigma_swapping_MEV
    #b_age_MEV = beta_age_MEV + draws_age * sigma_age_MEV,
    #b_age_EV = beta_age_EV + draws_age * sigma_age_EV
  )
  return(randcoeff)
}

# Define Initial Parameters, Including Standard Deviations for Random Coefficients
apollo_beta <- c(
  asc_Microcar = 0, asc_Electric_car = 0, asc_None = 0,
  beta_price_MEV = 0, beta_price_EV = 0,
  #beta_price_safety_MEV = 0, beta_price_speed_EV = 0,
  beta_swapping_MEV = 0,
  beta_car_usage_MEV = 0,
  beta_age_MEV = 0, beta_age_EV = 0,
  sigma_price_MEV = 1, sigma_price_EV = 1,
  #sigma_price_safety_MEV = 1, sigma_price_speed_EV = 1
  sigma_swapping_MEV = 1
  #sigma_age_MEV = 1, sigma_age_EV = 1
)

# Fix "None of the Above" Alternative as a Reference
apollo_fixed <- c("asc_None")

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  
  # Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  # Get random coefficients
  randcoeff <- apollo_randCoeff(apollo_beta, apollo_inputs)
  
  # Create a list to store probabilities
  P <- list()
  
  # Define utility functions with random coefficients
  V <- list()
  V[["Microcar"]] <- asc_Microcar + 
    randcoeff$b_price_MEV * PurchasePrice_MEV +
    #randcoeff$b_price_safety_MEV * Price_Safety_MEV 
    randcoeff$b_swapping_MEV * BatterySwapping_MEV +
    beta_car_usage_MEV * Car_Usage_Intensity +
    beta_age_MEV * age
  
  V[["Electric_car"]] <- asc_Electric_car + 
    randcoeff$b_price_EV * PurchasePrice_EV +
    #randcoeff$b_price_speed_EV * Price_speed_EV 
    beta_age_EV * age
  
  V[["None of the above"]] <- asc_None
  
  # Define settings for the Mixed Logit Model
  mnl_settings <- list(
    alternatives = c("Microcar" = 1, "Electric_car" = 2, "None of the above" = 3),
    choiceVar = Choice,
    utilities = V
  )
  
  # Debugging: Print `mnl_settings`
  #print("Debugging: mnl_settings content")
  print(mnl_settings)
  
  # Calculate probabilities using the MNL model
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  
  # Multiply over observations for the same individual
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  
  P <- apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  # Prepare and return the probabilities
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


apollo_inputs <- apollo_validateInputs()

model <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(model)
summary(model)

print(unique(database$Choice))
unexpected_values <- database$Choice[!database$Choice %in% c(1, 2, 3)]
if (length(unexpected_values) > 0) {
  print("Unexpected values in Choice:")
  print(unexpected_values)
} else {
  print("All Choice values are valid.")
}
summary(database)
