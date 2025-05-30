# Load libraries
library(tidyverse)
library(readr)

# Step 1: Load the datasets
final_data <- read_csv("/Users/pablovaquer/Desktop/TFG/Datasets/Final_Data_OLS.csv")%>%
  select(1, 3, 7) %>%  
  rename(
    Any = 1,
    Nom_districte = 2,
    Sup_mitjana_m2 = 3
  )

cpi <- read_delim("/Users/pablovaquer/Desktop/TFG/Datasets/CPI 2002-2024 .csv", delim = ";") %>%
  select(1, 2) %>%  # Keep only Year and General Index
  rename(
    Year = 1,
    CPI = 2
    )

euribor <- read_delim("/Users/pablovaquer/Desktop/TFG/Datasets/EURIBOR 2000-2023.csv", delim = ";") %>%
  select(1, 4) %>%  # Keep only Year and 12-month EURIBOR
  rename(
    Year = 1,
    EURIBOR = 2
  )

gdp <- read_delim("/Users/pablovaquer/Desktop/TFG/Datasets/GDP BCN 2011-2023.csv", delim = ";") %>%
  select(1, ncol(.)) %>%  # Keep only Year and last column (GDP)
  rename(
    Year = 1,
    GDP = 2
  )

# Rename first column to 'Year' explicitly
colnames(final_data)[1] <- "Year"
colnames(cpi)[1] <- "Year"
colnames(euribor)[1] <- "Year"
colnames(gdp)[1] <- "Year"


library(dplyr)
library(readr)

# Step 2: Join datasets by Year
full_data <- final_data %>%
  left_join(cpi, by = "Year") %>%
  left_join(euribor, by = "Year") %>%
  left_join(gdp, by = "Year")

# Keep only observations from 2011 to 2023
full_data <- full_data %>%
  filter(Year >= 2011, Year <= 2023)

summary(full_data)
any(is.na(full_data))



##OLS ANALYSIS#
model <- lm(Sup_mitjana_m2 ~ CPI + EURIBOR + GDP + factor(Nom_districte), data = full_data)
summary(model)

# Add predictions to the dataset
full_data$Predicted <- predict(model)

# Plot actual vs predicted
library(ggplot2)

ggplot(full_data, aes(x = Sup_mitjana_m2, y = Predicted)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Rental Prices (€/m²)",
       x = "Actual Price",
       y = "Predicted Price") +
  theme_minimal()

# Set plotting layout to 2 rows × 2 columns
par(mfrow = c(2, 2))

# Plot all 4 diagnostic plots
plot(model)

# Optional: reset plotting layout to default
par(mfrow = c(1, 1))



model_macro <- lm(Sup_mitjana_m2 ~ CPI + EURIBOR + GDP, data = full_data)
summary(model_macro)

model_district <- lm(Sup_mitjana_m2 ~ factor(Nom_districte), data = full_data)
summary(model_district)

model_full <- lm(Sup_mitjana_m2 ~ CPI + EURIBOR + GDP + factor(Nom_districte) + factor(Year), data = full_data)
summary(model_full)


View(full_data)


### TOTAL SPENDING PER BOROUGH####

library(tibble)

spending_data <- tribble(
  ~Borough,         ~Period,   ~Spending,
  "Manhattan",      "Pre-LL18",   928,
  "Brooklyn",       "Pre-LL18",  1100,
  "Queens",         "Pre-LL18",   488,
  "Bronx",          "Pre-LL18",   115,
  "Staten Island",  "Pre-LL18",    41,
  "Manhattan",      "Post-LL18",   39,
  "Brooklyn",       "Post-LL18",  123,
  "Queens",         "Post-LL18",   50,
  "Bronx",          "Post-LL18",    7,
  "Staten Island",  "Post-LL18",    4
)

ggplot(spending_data, aes(x = Borough, y = Spending, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Impact of Local Law 18 on Airbnb Spending by Borough",
    y = "Spending (Million USD)",
    x = "Borough",
    fill = "Period"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Pre-LL18" = "#1f77b4", "Post-LL18" = "#ff7f0e")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####GLM ANALYSIS###

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Create the dataframe
district_data <- data.frame(
  District = c("Eixample", "Gràcia", "Horta-Guinardó", "L'Eixample", "Les Corts", 
               "Nou Barris", "Sant Andreu", "Sant Martí", "Sants-Montjuïc", "Sarrià-Sant Gervasi"),
  Estimate = c(0.312220, 0.123332, 0.023569, 0.098966, 0.297750, 
               -0.007839, 0.077768, 0.118879, 0.056249, 0.508594),
  Std_Error = c(0.051759, 0.056073, 0.052837, 0.076683, 0.061465, 
                0.053249, 0.055162, 0.051505, 0.053249, 0.054855),
  P_value = c(1.71e-09, 0.0279, 0.6556, 0.1969, 1.30e-06, 
              0.8830, 0.1586, 0.0210, 0.2909, 2e-16)
)

#### Correlation Matrix###

# Load necessary libraries
library(ggplot2)
library(corrplot)
library(dplyr)





