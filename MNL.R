#Simple MNL
library(apollo)
apollo_initialise()

data <- read.csv("Processed_data_class_MNM.csv")
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

# Define Apollo control settings
apollo_control <- list(
  modelName = "MNL_Microcar_EV",
  modelDescr = "Multinomial Logit Model for Microcar vs Electric Car",
  indivID = "id",
  nCores          = 1,
  outputDirectory = "output"
)

# Interaction terms
database$Price_Range_MEV <- database$PurchasePrice_MEV * database$Range_MEV
database$Price_speed_EV <- database$PurchasePrice_EV * database$TopSpeed_EV
database$Price_Safety_MEV <- database$PurchasePrice_MEV * database$Safety_MEV

# Define initial parameters
apollo_beta <- c(
  asc_Microcar = 0, asc_Electric_car = 0, asc_None = 0,
  beta_price_MEV = 0, beta_price_EV = 0, 
  #beta_charging_MEV = 0, beta_charging_EV = 0, 
  #beta_top_speed_MEV = 0, 
  #beta_price_range_MEV = 0, 
  beta_price_safety_MEV = 0, 
  beta_price_speed_EV = 0, 
  #beta_safety_EV = 0, 
  #beta_range_EV = 0, #beta_comfort_MEV = 0,  beta_comfort_EV = 0,
  beta_swapping_MEV = 0, 
  #beta_swapping_EV = 0, 
  #beta_env_concern_EV = 0, beta_car_usage_EV = 0, 
  #beta_env_concern_MEV = 0, 
  beta_car_usage_MEV = 0,
  #beta_knowledge1_Fully.aware_MEV= 0,	
  beta_knowledge1_Not.aware_MEV=0,	
  #beta_knowledge1_Somewhat.aware_MEV=0,	
  beta_PuT_2to5km_MEV=0,	
  #beta_PuT_250to500m_MEV=0,	
  #beta_PuT_5km_MEV=0,
  #beta_PuT_500mto1km_MEV=0,	
  beta_PuT_250m_MEV =0,
  #beta_knowledge1_Fully.aware_EV= 0,	
  beta_knowledge1_Not.aware_EV=0,	
  #beta_knowledge1_Somewhat.aware_EV=0,	
  beta_PuT_2to5km_EV=0,	
  #beta_PuT_250to500m_EV=0,	
  #beta_PuT_5km_EV=0,
  #beta_PuT_500mto1km_EV=0,	
  beta_PuT_250m_EV =0
)

# Fix "None of the Above" alternative as a reference
apollo_fixed <- c("asc_None")


# Define the apollo_probabilities function
apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  
  # Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  # Create a list to store probabilities
  P <- list()
  
  # Define utility functions
  V <- list()
  V[["Microcar"]] <- asc_Microcar + 
    beta_price_MEV * PurchasePrice_MEV +
    #beta_charging_MEV * ChargingTime_MEV +
    #beta_top_speed_MEV * TopSpeed_MEV +
    #beta_price_range_MEV * Price_Range_MEV +
    beta_price_safety_MEV * Price_Safety_MEV +
    #beta_comfort_MEV * Comfort_MEV +
    beta_swapping_MEV * BatterySwapping_MEV +
    #beta_env_concern_MEV * Environmental_Concern +
    beta_car_usage_MEV * Car_Usage_Intensity +
    #beta_knowledge1_Fully.aware_MEV *	knowledge1_Fully.aware+
    beta_knowledge1_Not.aware_MEV *	knowledge1_Not.aware+
    #beta_knowledge1_Somewhat.aware_MEV *	knowledge1_Somewhat.aware+
    beta_PuT_2to5km_MEV *	PuT_2to5km+
    #beta_PuT_250to500m_MEV * PuT_250to500m+
    #beta_PuT_5km_MEV *PuT_5km+
    #beta_PuT_500mto1km_MEV *PuT_500mto1km+
    beta_PuT_250m_MEV *PuT_250m

    
  
  V[["Electric_car"]] <- asc_Electric_car + 
    beta_price_EV * PurchasePrice_EV +
    #beta_charging_EV * ChargingTime_EV +
    #beta_range_EV * Range_EV +
    beta_price_speed_EV * Price_speed_EV +
    #beta_comfort_EV * Comfort_EV +
    #beta_swapping_EV * BatterySwapping_EV 
    #beta_env_concern_EV * Environmental_Concern +
    #beta_car_usage_EV * Car_Usage_Intensity
    #beta_knowledge1_Fully.aware_EV *	knowledge1_Fully.aware+
    beta_knowledge1_Not.aware_EV *	knowledge1_Not.aware+
    #beta_knowledge1_Somewhat.aware_EV *	knowledge1_Somewhat.aware+
    beta_PuT_2to5km_EV *	PuT_2to5km+
    #beta_PuT_250to500m_EV * PuT_250to500m+
    #beta_PuT_5km_EV *PuT_5km+
    #beta_PuT_500mto1km_EV *PuT_500mto1km+
    beta_PuT_250m_EV *PuT_250m
  
  V[["None of the above"]] <- asc_None
  
  # Define settings for the MNL model
  mnl_settings <- list(
    alternatives = c("Microcar" = 1, "Electric_car" = 2, "None of the above" = 3),
    choiceVar = Choice,  # Choice variable
    utilities = V
  )
  
  # Calculate probabilities using the MNL model
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  
  # Take the product across observations for the same individual
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  
  # Prepare and return the probabilities
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# Validate inputs
apollo_inputs <- apollo_validateInputs()

# Estimate the model
model <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# Output the model results
apollo_modelOutput(model)

# Summarize the model results
summary(model)


#sum(is.na(database))
#head(database)
install.packages(c("ggcorrplot"))
library(ggcorrplot)
# Select relevant columns for the correlation plot
selected_columns <- database[, c("PurchasePrice_MEV", "PurchasePrice_EV", "ChargingTime_MEV", "ChargingTime_EV", 
                                 "TopSpeed_MEV", "TopSpeed_EV", "Range_MEV", "Range_EV", 
                                 "Safety_MEV", "EnvironmentalEffects_MEV", "EnvironmentalEffects_EV", 
                                 "BatterySwapping_MEV", "BatterySwapping_EV")]

# Compute the correlation matrix
cor_matrix <- cor(selected_columns, use = "pairwise.complete.obs")

# Plot the correlation matrix
cor_plot <- ggcorrplot(cor_matrix, 
                       hc.order = TRUE, 
                       type = "lower", 
                       lab = TRUE, 
                       lab_size = 3, 
                       title = "Correlation Plot", 
                       ggtheme = ggplot2::theme_minimal())

# Display the plot
print(cor_plot)
