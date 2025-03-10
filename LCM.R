#Choice Model
library(dplyr)
library(apollo)

# Initialise Apollo
apollo_initialise()

data <- read.csv("Processed_data_class_MNM.csv")
database <- data
database <- database[order(database$id), ]


database$Choice <- ifelse(database$Choice == "Microcar", 1,
                          ifelse(database$Choice == "Electric_car", 2,
                                 ifelse(database$Choice == "None of the above", 3, NA)))

database$PurchasePrice_MEV <- database$PurchasePrice_MEV / 1000
database$PurchasePrice_EV <- database$PurchasePrice_EV / 1000
database$ChargingTime_MEV <- database$ChargingTime_MEV / 10 
database$ChargingTime_EV <- database$ChargingTime_EV / 10    

database$Price_Range_MEV <- database$PurchasePrice_MEV * database$Range_MEV
database$Price_speed_EV <- database$PurchasePrice_EV * database$TopSpeed_EV
database$Price_Safety_MEV <- database$PurchasePrice_MEV * database$Safety_MEV

# Apollo Control Settings
apollo_control <- list(
  modelName = "LCM_Price_ChargingTime",
  modelDescr = "Latent Class Model for Microcar vs Electric Car (Price + Charging Time)",
  indivID = "id",
  nCores = 1,
  outputDirectory = "output"
)

# Initial Parameters
apollo_beta <- c(
  # Class 1 Utility Parameters
  asc_Microcar_class1 = 0, asc_Electric_car_class1 = 0, asc_None = 0,
  beta_price_MEV_class1 = 0, beta_price_EV_class1 = 0,
  beta_time_MEV_class1 = 0, 
  beta_Car_Usage_Intensity_MEV_class1 = 0,
  beta_Car_Usage_Intensity_EV_class1 = 0,
  beta_knowledge1_Not.aware_EV_class1 = 0,
  
  # Class 2 Utility Parameters
  asc_Microcar_class2 = 0, asc_Electric_car_class2 = 0,
  beta_price_EV_class2 = 0,
  beta_Environmental_Concern_MEV_class2 = 0, 
  beta_Car_Usage_Intensity_MEV_class2 = 0,
  beta_Environmental_Concern_EV_class2 = 0, 
  beta_knowledge1_Not.aware_MEV_class2 = 0, beta_knowledge1_Not.aware_EV_class2 = 0,
  beta_PuT_250m_EV_class2 = 0, beta_PuT_250m_MEV_class2 = 0,

  # Class 3 Utility Parameters
  asc_Microcar_class3 = 0, asc_Electric_car_class3 = 0,
  beta_price_EV_class3 = 0,
  beta_time_MEV_class3 = 0, beta_time_EV_class3 = 0,
  beta_Price_Range_MEV_class3 = 0, beta_Price_speed_EV_class3 = 0,
  #beta_Car_Usage_Intensity_MEV_class3 = 0,
  beta_PuT_250m_MEV_class3 = 0,
  
  # Class 4 Utility Parameters
  asc_Microcar_class4 = 0, asc_Electric_car_class4 = 0,
  beta_price_EV_class4 = 0,
  beta_time_EV_class4 = 0,
  beta_Price_speed_EV_class4 = 0,
  beta_Car_Usage_Intensity_MEV_class4 = 0,
	beta_knowledge1_Not.aware_EV_class4 = 0,
  beta_PuT_2to5km_MEV_class4=0, beta_PuT_2to5km_EV_class4=0
)

# Fix "None of the Above" as Reference
apollo_fixed <- c("asc_None")

# Define Apollo Probabilities Function
apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P <- list()
  
  # Define Utility Functions for Each Class
  V <- list(
    Microcar = ifelse(Class == 1,
                      asc_Microcar_class1 + beta_price_MEV_class1 * PurchasePrice_MEV +
                        beta_time_MEV_class1 * ChargingTime_MEV +
                        beta_Car_Usage_Intensity_MEV_class1 * Car_Usage_Intensity,
                      ifelse(Class == 2,
                             asc_Microcar_class2 +
                               beta_Environmental_Concern_MEV_class2 * Environmental_Concern +
                               beta_Car_Usage_Intensity_MEV_class2 * Car_Usage_Intensity +
                               beta_knowledge1_Not.aware_MEV_class2 * knowledge1_Not.aware +
                               beta_PuT_250m_MEV_class2 * PuT_250m,
                             ifelse(Class == 3,
                                    asc_Microcar_class3 + beta_time_MEV_class3 * ChargingTime_MEV +
                                      beta_Price_Range_MEV_class3 * Price_Range_MEV +
                                      #beta_Car_Usage_Intensity_MEV_class3 * Car_Usage_Intensity +
                                      beta_PuT_250m_MEV_class3 * PuT_250m,
                                    ifelse(Class == 4,
                                           asc_Microcar_class4 +
                                             beta_Car_Usage_Intensity_MEV_class4 * Car_Usage_Intensity+
                                             beta_PuT_2to5km_MEV_class4 * PuT_2to5km, 0)))),
    
    Electric_car = ifelse(Class == 1,
                          asc_Electric_car_class1 + beta_price_EV_class1 * PurchasePrice_EV +
                            beta_Car_Usage_Intensity_EV_class1 * Car_Usage_Intensity +
                            beta_knowledge1_Not.aware_EV_class1 * knowledge1_Not.aware,
                          ifelse(Class == 2,
                                 asc_Electric_car_class2 + beta_price_EV_class2 * PurchasePrice_EV +
                                   beta_Environmental_Concern_EV_class2 * Environmental_Concern +
                                   beta_knowledge1_Not.aware_EV_class2 * knowledge1_Not.aware +
                                   beta_PuT_250m_EV_class2 * PuT_250m,
                                 ifelse(Class == 3,
                                        asc_Electric_car_class3 + beta_price_EV_class3 * PurchasePrice_EV +
                                          beta_time_EV_class3 * ChargingTime_EV +
                                          beta_Price_speed_EV_class3 * Price_speed_EV,
                                        ifelse(Class == 4,
                                               asc_Electric_car_class4 + beta_price_EV_class4 * PurchasePrice_EV +
                                                 beta_time_EV_class4 * ChargingTime_EV +
                                                 beta_Price_speed_EV_class4 * Price_speed_EV +
                                                 beta_knowledge1_Not.aware_EV_class4 * knowledge1_Not.aware+
                                                 beta_PuT_2to5km_EV_class4 * PuT_2to5km, 0)))),
    
    None = asc_None  # Reference
  )
  
  # Multinomial Logit Model
  mnl_settings <- list(
    alternatives = c("Microcar" = 1, "Electric_car" = 2, "None" = 3),
    choiceVar = Choice,
    utilities = V
  )
  # Compute Probabilities
  P <- apollo_mnl(mnl_settings, functionality)
  
  # Aggregate Panel Probabilities
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  
  assign("overall_probabilities", P, envir = .GlobalEnv)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# Validate Inputs
apollo_inputs <- apollo_validateInputs()

# Estimate the Model
model <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(model)
summary(model)


#Correlation
#library(ggplot2)
#library(reshape2)

#data <- read.csv("Processed_data_class_MNM.csv")
#selected_columns <- data[, 3:18]
#selected_columns_filtered <- selected_columns[, apply(selected_columns, 2, var) != 0]


#correlation_matrix <- cor(selected_columns_filtered, use = "complete.obs")
#melted_correlation <- melt(correlation_matrix)
#ggplot(data = melted_correlation, aes(x = Var1, y = Var2, fill = value)) +
#  geom_tile(color = "white") +
#  geom_text(aes(label = sprintf("%.2f", value)), size = 3) +  # Display correlation values
#  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
#                       limit = c(-1, 1), space = "Lab", name = "Correlation") +
#  theme_minimal() +
#  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
#  coord_fixed() +
#  labs(title = "Correlation Matrix (Columns 3 to 16)", x = "", y = "")

#library(car)
#vif(lm(Choice ~ Range_MEV + TopSpeed_MEV + ChargingTime_MEV, data = database))


# Predicted Probabilities
predictions <- apollo_prediction(model, apollo_probabilities, apollo_inputs)
#str(predictions)

#write.csv(predictions$Microcar, "predicted_microcar.csv", row.names = FALSE)
#write.csv(predictions$Electric_car, "predicted_electric_car.csv", row.names = FALSE)
#write.csv(predictions$None, "predicted_none.csv", row.names = FALSE)
#write.csv(predictions$chosen, "predicted_chosen.csv", row.names = FALSE)

#Consider them seperately and map them together with Class column manually as id's are repititive!

updated_df <- read.csv("updated_choices.csv")

# Calculate class-wise predicted and observed probabilities
class_probabilities <- updated_df %>%
  group_by(Class) %>%
  summarise(
    Predicted_Microcar = mean(Predicted_Choice == "Microcar"),
    Predicted_Electric_Car = mean(Predicted_Choice == "Electric_car"),
    Predicted_None = mean(Predicted_Choice == "None"),
    Observed_Microcar = mean(Observed_Choice == "Microcar"),
    Observed_Electric_Car = mean(Observed_Choice == "Electric_car"),
    Observed_None = mean(Observed_Choice == "None")
  )

print(class_probabilities)

library(tidyr)
library(dplyr)
class_probabilities_long <- class_probabilities %>%
  pivot_longer(
    cols = starts_with("Predicted_") | starts_with("Observed_"),
    names_to = "Variable",
    values_to = "Probability"
  ) %>%
  mutate(
    Type = ifelse(grepl("^Predicted", Variable), "Predicted", "Observed"),
    Choice = gsub("Predicted_|Observed_", "", Variable)
  ) %>%
  select(-Variable)


library(ggplot2)
ggplot(class_probabilities_long, aes(x = Choice, y = Probability, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Class, scales = "free_y") +
  labs(
    title = "Class-Wise Predicted vs Observed Probabilities",
    x = "Choice",
    y = "Probability",
    fill = "Type"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Calculate overall probabilities for Predicted_Choice and Observed_Choice
overall_probabilities <- updated_df %>%
  summarise(
    Predicted_Microcar = mean(Predicted_Choice == "Microcar"),
    Predicted_Electric_Car = mean(Predicted_Choice == "Electric_car"),
    Predicted_None = mean(Predicted_Choice == "None"),
    Observed_Microcar = mean(Observed_Choice == "Microcar"),
    Observed_Electric_Car = mean(Observed_Choice == "Electric_car"),
    Observed_None = mean(Observed_Choice == "None")
  )

print(overall_probabilities)
