# ==========================
# Libraries
# ==========================
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(viridis)
library(tibble)
library(corrplot)
library(sf)

# ==========================
# Data Loading
# ==========================
# Load property prices
final_data <- read_csv("/Users/pablovaquer/Desktop/TFG/Datasets/Final_Data_OLS.csv") %>%
  select(1, 3, 7) %>%
  rename(Year = 1, Nom_districte = 2, Sup_mitjana_m2 = 3)

# Load macroeconomic datasets
cpi <- read_delim("/Users/pablovaquer/Desktop/TFG/Datasets/CPI 2002-2024 .csv", delim = ";") %>%
  select(1, 2) %>%
  rename(Year = 1, CPI = 2)

euribor <- read_delim("/Users/pablovaquer/Desktop/TFG/Datasets/EURIBOR 2000-2023.csv", delim = ";") %>%
  select(1, 4) %>%
  rename(Year = 1, EURIBOR = 2)

gdp <- read_delim("/Users/pablovaquer/Desktop/TFG/Datasets/GDP BCN 2011-2023.csv", delim = ";") %>%
  select(1, ncol(.)) %>%
  rename(Year = 1, GDP = 2)

# ==========================
# Data Merging
# ==========================
full_data <- final_data %>%
  left_join(cpi, by = "Year") %>%
  left_join(euribor, by = "Year") %>%
  left_join(gdp, by = "Year") %>%
  filter(Year >= 2011, Year <= 2023)

summary(full_data)
any(is.na(full_data))

# ==========================
# OLS Analysis
# ==========================
model <- lm(Sup_mitjana_m2 ~ CPI + EURIBOR + GDP + factor(Nom_districte), data = full_data)
summary(model)

full_data$Predicted <- predict(model)

# Actual vs Predicted Plot
ggplot(full_data, aes(x = Sup_mitjana_m2, y = Predicted)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Rental Prices (€/m²)",
       x = "Actual Price", y = "Predicted Price") +
  theme_minimal()

# Model diagnostics
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))

# Macroeconomic-only model
model_macro <- lm(Sup_mitjana_m2 ~ CPI + EURIBOR + GDP, data = full_data)
summary(model_macro)

# District-only model
model_district <- lm(Sup_mitjana_m2 ~ factor(Nom_districte), data = full_data)
summary(model_district)

# Full model with Year fixed effects
model_full <- lm(Sup_mitjana_m2 ~ CPI + EURIBOR + GDP + factor(Nom_districte) + factor(Year), data = full_data)
summary(model_full)

# ==========================
# GLM Coefficients Table (for visualization)
# ==========================
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

# ==========================
# Density Plot (Key Variables)
# ==========================
ggplot(full_data %>%
         pivot_longer(cols = c(Sup_mitjana_m2, CPI, EURIBOR, GDP), names_to = "Variable", values_to = "Value"),
       aes(x = Value, fill = Variable)) +
  geom_density(alpha = 0.6, color = "black") +
  facet_wrap(~ Variable, scales = "free") +
  scale_fill_manual(values = c("#2c3e50", "#4e5d6c", "#607d8b", "#37474f")) +
  labs(title = "Density Plots of Key Numeric Variables", x = "Value", y = "Density") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none", strip.text = element_text(face = "bold", color = "#2c3e50"),
        plot.title = element_text(hjust = 0.5, face = "bold"))

# ==========================
# Boxplot: Rental Prices by District
# ==========================
ggplot(full_data %>% filter(Sup_mitjana_m2 >= 500 & Sup_mitjana_m2 <= 1500), 
       aes(x = reorder(Nom_districte, Sup_mitjana_m2, median), y = Sup_mitjana_m2)) +
  geom_boxplot(fill = "#90a4ae", outlier.color = "red", outlier.shape = 1) +
  coord_flip() +
  labs(title = "Boxplot of €/m² Prices by District (2018–2023)", x = "District", y = "€/m²") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), axis.text.y = element_text(face = "bold"))

# ==========================
# Correlation Matrix
# ==========================
cor_matrix <- full_data %>%
  select(Sup_mitjana_m2, CPI, EURIBOR, GDP) %>%
  cor(use = "complete.obs")

corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black",
         number.cex = 0.7, col = colorRampPalette(c("lightblue", "white", "darkred"))(200))

# ==========================
# Tourism Density Map (Barcelona)
# ==========================
dt <- st_read("/Users/pablovaquer/Desktop/TFG/Datasets/2019_turisme_allotjament.gpkg", layer = "2019_turisme_allotjament")

ggplot() +
  geom_sf(data = dt, aes(fill = DN), color = NA) +
  scale_fill_viridis(name = "DN", option = "viridis", direction = -1) +
  theme_minimal() +
  labs(title = "Density of Tourists across Barcelona", fill = "DN") +
  theme(legend.position = "right")

# ==========================
# Airbnb Spending in NYC Boroughs (LL18 Impact)
# ==========================
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
  labs(title = "Impact of Local Law 18 on Airbnb Spending by Borough",
       y = "Spending (Million USD)", x = "Borough", fill = "Period") +
  theme_minimal() +
  scale_fill_manual(values = c("Pre-LL18" = "#1f77b4", "Post-LL18" = "#ff7f0e")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

