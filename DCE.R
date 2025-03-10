#Model Pre-processing

#install.packages("apollo")
#install.packages("ggplot2")
#install.packages("Matrix")
#install.packages("dplyr")

# Load necessary libraries
library(apollo)
library(ggplot2)
library(dplyr)

my_data <- read.csv("mode_data_eda.csv")
head(my_data)
# Define attributes for each choice set
# Choice Set 1
attributes_set_1 <- data.frame(
  PurchasePrice_MEV = 20000,
  PurchasePrice_EV = 22000,
  ChargingTime_MEV = 4,
  ChargingTime_EV = 8,
  TopSpeed_MEV = 80,
  TopSpeed_EV = 140,
  Range_MEV = 100,
  Range_EV = 230,
  Safety_MEV = 4,
  Safety_EV = 5,
  EnvironmentalEffects_MEV = 80,
  EnvironmentalEffects_EV = 60,
  Comfort_MEV = 2,
  Comfort_EV = 4,
  BatterySwapping_MEV = 1,
  BatterySwapping_EV = 0
)


# Choice Set 2
attributes_set_2 <- data.frame(
  PurchasePrice_MEV = 8000,
  PurchasePrice_EV = 18000,
  ChargingTime_MEV = 3,
  ChargingTime_EV = 4,
  TopSpeed_MEV = 45,
  TopSpeed_EV = 140,
  Range_MEV = 75,
  Range_EV = 200,
  Safety_MEV = 3,
  Safety_EV = 5,
  EnvironmentalEffects_MEV = 80,
  EnvironmentalEffects_EV = 70,
  Comfort_MEV = 2,
  Comfort_EV = 4,
  BatterySwapping_MEV = 2,
  BatterySwapping_EV = 1
)

# Choice Set 3
attributes_set_3 <- data.frame(
  PurchasePrice_MEV = 17000,
  PurchasePrice_EV = 26000,
  ChargingTime_MEV = 5,
  ChargingTime_EV = 8,
  TopSpeed_MEV = 80,
  TopSpeed_EV = 140,
  Range_MEV = 100,
  Range_EV = 230,
  Safety_MEV = 4,
  Safety_EV = 5,
  EnvironmentalEffects_MEV = 70,
  EnvironmentalEffects_EV = 50,
  Comfort_MEV = 2,
  Comfort_EV = 4,
  BatterySwapping_MEV = 2,
  BatterySwapping_EV = 0
)

# Choice Set 4
attributes_set_4 <- data.frame(
  PurchasePrice_MEV = 8000,
  PurchasePrice_EV = 34000,
  ChargingTime_MEV = 4,
  ChargingTime_EV = 8,
  TopSpeed_MEV = 45,
  TopSpeed_EV = 180,
  Range_MEV = 75,
  Range_EV = 230,
  Safety_MEV = 3,
  Safety_EV = 5,
  EnvironmentalEffects_MEV = 80,
  EnvironmentalEffects_EV = 60,
  Comfort_MEV = 2,
  Comfort_EV = 4,
  BatterySwapping_MEV = 2,
  BatterySwapping_EV = 0
)

# Choice Set 5
attributes_set_5 <- data.frame(
  PurchasePrice_MEV = 13000,
  PurchasePrice_EV = 26000,
  ChargingTime_MEV = 4,
  ChargingTime_EV = 6,
  TopSpeed_MEV = 120,
  TopSpeed_EV = 160,
  Range_MEV = 100,
  Range_EV = 270,
  Safety_MEV = 4,
  Safety_EV = 5,
  EnvironmentalEffects_MEV = 80,
  EnvironmentalEffects_EV = 50,
  Comfort_MEV = 2,
  Comfort_EV = 4,
  BatterySwapping_MEV = 0,
  BatterySwapping_EV = 2
)

# Choice Set 6
attributes_set_6 <- data.frame(
  PurchasePrice_MEV = 17000,
  PurchasePrice_EV = 22000,
  ChargingTime_MEV = 5,
  ChargingTime_EV = 8,
  TopSpeed_MEV = 80,
  TopSpeed_EV = 140,
  Range_MEV = 120,
  Range_EV = 300,
  Safety_MEV = 4,
  Safety_EV = 5,
  EnvironmentalEffects_MEV = 70,
  EnvironmentalEffects_EV = 60,
  Comfort_MEV = 2,
  Comfort_EV = 4,
  BatterySwapping_MEV = 0,
  BatterySwapping_EV = 1
)

# Choice Set 7
attributes_set_7 <- data.frame(
  PurchasePrice_MEV = 10000,
  PurchasePrice_EV = 22000,
  ChargingTime_MEV = 5,
  ChargingTime_EV = 6,
  TopSpeed_MEV = 120,
  TopSpeed_EV = 160,
  Range_MEV = 100,
  Range_EV = 300,
  Safety_MEV = 3,
  Safety_EV = 5,
  EnvironmentalEffects_MEV = 80,
  EnvironmentalEffects_EV = 60,
  Comfort_MEV = 2,
  Comfort_EV = 4,
  BatterySwapping_MEV = 1,
  BatterySwapping_EV = 0
)

# Choice Set 8
attributes_set_8 <- data.frame(
  PurchasePrice_MEV = 20000,
  PurchasePrice_EV = 34000,
  ChargingTime_MEV = 4,
  ChargingTime_EV = 8,
  TopSpeed_MEV = 80,
  TopSpeed_EV = 160,
  Range_MEV = 120,
  Range_EV = 300,
  Safety_MEV = 4,
  Safety_EV = 5,
  EnvironmentalEffects_MEV = 60,
  EnvironmentalEffects_EV = 40,
  Comfort_MEV = 2,
  Comfort_EV = 4,
  BatterySwapping_MEV = 2,
  BatterySwapping_EV = 0
)

my_data <- my_data %>% mutate(id = row_number())

# Enrich the data for each choice set
CS1 <- my_data %>%
  select(id, choice_set1) %>%
  rename(Choice = choice_set1) %>%
  mutate(
    PurchasePrice_MEV = attributes_set_1$PurchasePrice_MEV,
    PurchasePrice_EV = attributes_set_1$PurchasePrice_EV,
    ChargingTime_MEV = attributes_set_1$ChargingTime_MEV,
    ChargingTime_EV = attributes_set_1$ChargingTime_EV,
    TopSpeed_MEV = attributes_set_1$TopSpeed_MEV,
    TopSpeed_EV = attributes_set_1$TopSpeed_EV,
    Range_MEV = attributes_set_1$Range_MEV,
    Range_EV = attributes_set_1$Range_EV,
    Safety_MEV = attributes_set_1$Safety_MEV,
    Safety_EV = attributes_set_1$Safety_EV,
    EnvironmentalEffects_MEV = attributes_set_1$EnvironmentalEffects_MEV,
    EnvironmentalEffects_EV = attributes_set_1$EnvironmentalEffects_EV,
    Comfort_MEV = attributes_set_1$Comfort_MEV,
    Comfort_EV = attributes_set_1$Comfort_EV,
    BatterySwapping_MEV = attributes_set_1$BatterySwapping_MEV,
    BatterySwapping_EV = attributes_set_1$BatterySwapping_EV,
    Choice_Task = 1,
    Scenario = 1
  )

CS2 <- my_data %>%
  select(id, choice_set2) %>%
  rename(Choice = choice_set2) %>%
  mutate(
    PurchasePrice_MEV = attributes_set_2$PurchasePrice_MEV,
    PurchasePrice_EV = attributes_set_2$PurchasePrice_EV,
    ChargingTime_MEV = attributes_set_2$ChargingTime_MEV,
    ChargingTime_EV = attributes_set_2$ChargingTime_EV,
    TopSpeed_MEV = attributes_set_2$TopSpeed_MEV,
    TopSpeed_EV = attributes_set_2$TopSpeed_EV,
    Range_MEV = attributes_set_2$Range_MEV,
    Range_EV = attributes_set_2$Range_EV,
    Safety_MEV = attributes_set_2$Safety_MEV,
    Safety_EV = attributes_set_2$Safety_EV,
    EnvironmentalEffects_MEV = attributes_set_2$EnvironmentalEffects_MEV,
    EnvironmentalEffects_EV = attributes_set_2$EnvironmentalEffects_EV,
    Comfort_MEV = attributes_set_2$Comfort_MEV,
    Comfort_EV = attributes_set_2$Comfort_EV,
    BatterySwapping_MEV = attributes_set_2$BatterySwapping_MEV,
    BatterySwapping_EV = attributes_set_2$BatterySwapping_EV,
    Choice_Task = 2,
    Scenario = 1
  )

CS3 <- my_data %>%
  select(id, choice_set3) %>%
  rename(Choice = choice_set3) %>%
  mutate(
    PurchasePrice_MEV = attributes_set_3$PurchasePrice_MEV,
    PurchasePrice_EV = attributes_set_3$PurchasePrice_EV,
    ChargingTime_MEV = attributes_set_3$ChargingTime_MEV,
    ChargingTime_EV = attributes_set_3$ChargingTime_EV,
    TopSpeed_MEV = attributes_set_3$TopSpeed_MEV,
    TopSpeed_EV = attributes_set_3$TopSpeed_EV,
    Range_MEV = attributes_set_3$Range_MEV,
    Range_EV = attributes_set_3$Range_EV,
    Safety_MEV = attributes_set_3$Safety_MEV,
    Safety_EV = attributes_set_3$Safety_EV,
    EnvironmentalEffects_MEV = attributes_set_3$EnvironmentalEffects_MEV,
    EnvironmentalEffects_EV = attributes_set_3$EnvironmentalEffects_EV,
    Comfort_MEV = attributes_set_3$Comfort_MEV,
    Comfort_EV = attributes_set_3$Comfort_EV,
    BatterySwapping_MEV = attributes_set_3$BatterySwapping_MEV,
    BatterySwapping_EV = attributes_set_3$BatterySwapping_EV,
    Choice_Task = 3,
    Scenario = 1
  )

CS4 <- my_data %>%
  select(id, choice_set4) %>%
  rename(Choice = choice_set4) %>%
  mutate(
    PurchasePrice_MEV = attributes_set_4$PurchasePrice_MEV,
    PurchasePrice_EV = attributes_set_4$PurchasePrice_EV,
    ChargingTime_MEV = attributes_set_4$ChargingTime_MEV,
    ChargingTime_EV = attributes_set_4$ChargingTime_EV,
    TopSpeed_MEV = attributes_set_4$TopSpeed_MEV,
    TopSpeed_EV = attributes_set_4$TopSpeed_EV,
    Range_MEV = attributes_set_4$Range_MEV,
    Range_EV = attributes_set_4$Range_EV,
    Safety_MEV = attributes_set_4$Safety_MEV,
    Safety_EV = attributes_set_4$Safety_EV,
    EnvironmentalEffects_MEV = attributes_set_4$EnvironmentalEffects_MEV,
    EnvironmentalEffects_EV = attributes_set_4$EnvironmentalEffects_EV,
    Comfort_MEV = attributes_set_4$Comfort_MEV,
    Comfort_EV = attributes_set_4$Comfort_EV,
    BatterySwapping_MEV = attributes_set_4$BatterySwapping_MEV,
    BatterySwapping_EV = attributes_set_4$BatterySwapping_EV,
    Choice_Task = 4,
    Scenario = 1
  )

CS5 <- my_data %>%
  select(id, choice_set5) %>%
  rename(Choice = choice_set5) %>%
  mutate(
    PurchasePrice_MEV = attributes_set_5$PurchasePrice_MEV,
    PurchasePrice_EV = attributes_set_5$PurchasePrice_EV,
    ChargingTime_MEV = attributes_set_5$ChargingTime_MEV,
    ChargingTime_EV = attributes_set_5$ChargingTime_EV,
    TopSpeed_MEV = attributes_set_5$TopSpeed_MEV,
    TopSpeed_EV = attributes_set_5$TopSpeed_EV,
    Range_MEV = attributes_set_5$Range_MEV,
    Range_EV = attributes_set_5$Range_EV,
    Safety_MEV = attributes_set_5$Safety_MEV,
    Safety_EV = attributes_set_5$Safety_EV,
    EnvironmentalEffects_MEV = attributes_set_5$EnvironmentalEffects_MEV,
    EnvironmentalEffects_EV = attributes_set_5$EnvironmentalEffects_EV,
    Comfort_MEV = attributes_set_5$Comfort_MEV,
    Comfort_EV = attributes_set_5$Comfort_EV,
    BatterySwapping_MEV = attributes_set_5$BatterySwapping_MEV,
    BatterySwapping_EV = attributes_set_5$BatterySwapping_EV,
    Choice_Task = 5,
    Scenario = 1
  )

CS6 <- my_data %>%
  select(id, choice_set6) %>%
  rename(Choice = choice_set6) %>%
  mutate(
    PurchasePrice_MEV = attributes_set_6$PurchasePrice_MEV,
    PurchasePrice_EV = attributes_set_6$PurchasePrice_EV,
    ChargingTime_MEV = attributes_set_6$ChargingTime_MEV,
    ChargingTime_EV = attributes_set_6$ChargingTime_EV,
    TopSpeed_MEV = attributes_set_6$TopSpeed_MEV,
    TopSpeed_EV = attributes_set_6$TopSpeed_EV,
    Range_MEV = attributes_set_6$Range_MEV,
    Range_EV = attributes_set_6$Range_EV,
    Safety_MEV = attributes_set_6$Safety_MEV,
    Safety_EV = attributes_set_6$Safety_EV,
    EnvironmentalEffects_MEV = attributes_set_6$EnvironmentalEffects_MEV,
    EnvironmentalEffects_EV = attributes_set_6$EnvironmentalEffects_EV,
    Comfort_MEV = attributes_set_6$Comfort_MEV,
    Comfort_EV = attributes_set_6$Comfort_EV,
    BatterySwapping_MEV = attributes_set_6$BatterySwapping_MEV,
    BatterySwapping_EV = attributes_set_6$BatterySwapping_EV,
    Choice_Task = 6,
    Scenario = 1
  )

CS7 <- my_data %>%
  select(id, choice_set7) %>%
  rename(Choice = choice_set7) %>%
  mutate(
    PurchasePrice_MEV = attributes_set_7$PurchasePrice_MEV,
    PurchasePrice_EV = attributes_set_7$PurchasePrice_EV,
    ChargingTime_MEV = attributes_set_7$ChargingTime_MEV,
    ChargingTime_EV = attributes_set_7$ChargingTime_EV,
    TopSpeed_MEV = attributes_set_7$TopSpeed_MEV,
    TopSpeed_EV = attributes_set_7$TopSpeed_EV,
    Range_MEV = attributes_set_7$Range_MEV,
    Range_EV = attributes_set_7$Range_EV,
    Safety_MEV = attributes_set_7$Safety_MEV,
    Safety_EV = attributes_set_7$Safety_EV,
    EnvironmentalEffects_MEV = attributes_set_7$EnvironmentalEffects_MEV,
    EnvironmentalEffects_EV = attributes_set_7$EnvironmentalEffects_EV,
    Comfort_MEV = attributes_set_7$Comfort_MEV,
    Comfort_EV = attributes_set_7$Comfort_EV,
    BatterySwapping_MEV = attributes_set_7$BatterySwapping_MEV,
    BatterySwapping_EV = attributes_set_7$BatterySwapping_EV,
    Choice_Task = 7,
    Scenario = 1
  )

CS8 <- my_data %>%
  select(id, choice_set8) %>%
  rename(Choice = choice_set8) %>%
  mutate(
    PurchasePrice_MEV = attributes_set_8$PurchasePrice_MEV,
    PurchasePrice_EV = attributes_set_8$PurchasePrice_EV,
    ChargingTime_MEV = attributes_set_8$ChargingTime_MEV,
    ChargingTime_EV = attributes_set_8$ChargingTime_EV,
    TopSpeed_MEV = attributes_set_8$TopSpeed_MEV,
    TopSpeed_EV = attributes_set_8$TopSpeed_EV,
    Range_MEV = attributes_set_8$Range_MEV,
    Range_EV = attributes_set_8$Range_EV,
    Safety_MEV = attributes_set_8$Safety_MEV,
    Safety_EV = attributes_set_8$Safety_EV,
    EnvironmentalEffects_MEV = attributes_set_8$EnvironmentalEffects_MEV,
    EnvironmentalEffects_EV = attributes_set_8$EnvironmentalEffects_EV,
    Comfort_MEV = attributes_set_8$Comfort_MEV,
    Comfort_EV = attributes_set_8$Comfort_EV,
    BatterySwapping_MEV = attributes_set_8$BatterySwapping_MEV,
    BatterySwapping_EV = attributes_set_8$BatterySwapping_EV,
    Choice_Task = 8,
    Scenario = 1
  )

# Combine all choice sets into a single data frame
combined_data <- bind_rows(CS1, CS2, CS3, CS4, CS5, CS6, CS7, CS8)

# Add a `Choice_Set_ID` column when combining choice sets
combined_data <- bind_rows(
  CS1 %>% mutate(Choice_Set_ID = 1),
  CS2 %>% mutate(Choice_Set_ID = 2),
  CS3 %>% mutate(Choice_Set_ID = 3),
  CS4 %>% mutate(Choice_Set_ID = 4),
  CS5 %>% mutate(Choice_Set_ID = 5),
  CS6 %>% mutate(Choice_Set_ID = 6),
  CS7 %>% mutate(Choice_Set_ID = 7),
  CS8 %>% mutate(Choice_Set_ID = 8)
)

# Sort by respondent ID to ensure all choices by the same respondent are together
combined_data <- combined_data %>% arrange(id)

# Ensure that data for each respondent is replicated correctly to match with the choice sets
num_tasks_per_respondent <- 8
expanded_my_data <- my_data[rep(1:nrow(my_data), each = num_tasks_per_respondent), ]

# Verify row counts match
nrow(expanded_my_data)
nrow(combined_data)

# Dynamically include only the relevant `p.sX` column based on `Choice_Set_ID`
combined_data <- combined_data %>%
  mutate(
    # Convert Choice_Set_ID to character to match the type of p.s values
    Choice_Set_ID = as.character(Choice_Set_ID),
    knowledge1 = expanded_my_data$knowledge1,
    knowledge2 = expanded_my_data$knowledge2,
    knowledge3 = expanded_my_data$knowledge3,
    age = expanded_my_data$age,
    education = expanded_my_data$education,
    income = expanded_my_data$income,
    nearby_PuT = expanded_my_data$nearby_PuT,
    gender = expanded_my_data$gender,
    employment = expanded_my_data$employment,
    region = expanded_my_data$region,
    family_structure = expanded_my_data$family_structure,
    Environmental_Concern = expanded_my_data$Environmental_Concern,
    Car_Usage_Intensity = expanded_my_data$Car_Usage_Intensity,
    Family_cluster = expanded_my_data$Family_cluster,
    envi_concern1 = expanded_my_data$envi_concern1,
    envi_concern2 = expanded_my_data$envi_concern2,
    envi_concern3 = expanded_my_data$envi_concern3,
    envi_concern5 = expanded_my_data$envi_concern5,
    knowledge1.1 = expanded_my_data$knowledge1.1,
    knowledge2.1 = expanded_my_data$knowledge2.1,
    knowledge3.1 = expanded_my_data$knowledge3.1,
    car_usage = expanded_my_data$car_usage,
    daily_ttd = expanded_my_data$daily_ttd,
    car_usage_ld = expanded_my_data$car_usage_ld,
    yearly_ttd = expanded_my_data$yearly_ttd,
    no_cars = expanded_my_data$no_cars,
    # Dynamically add the relevant `p.sX`
    p.s = case_when(
      Choice_Set_ID == "1" ~ expanded_my_data$p.s1,
      Choice_Set_ID == "2" ~ expanded_my_data$p.s2,
      Choice_Set_ID == "3" ~ expanded_my_data$p.s3,
      Choice_Set_ID == "4" ~ expanded_my_data$p.s4,
      Choice_Set_ID == "5" ~ expanded_my_data$p.s5,
      Choice_Set_ID == "6" ~ expanded_my_data$p.s6,
      Choice_Set_ID == "7" ~ expanded_my_data$p.s7,
      Choice_Set_ID == "8" ~ expanded_my_data$p.s8,
      TRUE ~ NA_character_ # Use NA_character_ for consistency
    )
  ) %>%
  select(-Choice_Set_ID) # Drop Choice_Set_ID after using it



#combined_data <- combined_data %>%
#  filter(Choice %in% c(1, 2))

# Save the final combined data to CSV
#write.csv(combined_data, "combined_model_data_final.csv", row.names = FALSE)

# Summary of the combined data
summary(combined_data)