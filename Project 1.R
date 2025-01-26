#Project 1 Code - Louis Williams 
# Part 2
# Load necessary libraries
library(dplyr)

# Read the long-format data
data <- read.csv("data_long_format.csv")

# Filter data for GDP per capita only
gdp_data <- data %>%
  dplyr::filter(Indicator == "GDP per capita (constant 2015 US$)") %>%
  mutate(Year = as.numeric(Year)) %>%
  arrange(Country, Year)

# Compute annual GDP per capita growth rates
gdp_growth <- gdp_data %>%
  group_by(Country) %>%
  mutate(Growth_Rate = (Value - lag(Value)) / lag(Value) * 100) %>%
  ungroup()

# View the results
print(head(gdp_growth))


#2.2 Visualize growth

# Load necessary library for visualization
library(ggplot2)

# Create a line plot comparing GDP growth trajectories
gdp_growth_plot <- ggplot(gdp_growth, aes(x = Year, y = Growth_Rate, color = Country, group = Country)) +
  geom_line(size = 1) +  # Line for each country
  geom_point(size = 2) +  # Add points for better visualization
  labs(
    title = "GDP Per Capita Growth Trajectories",
    subtitle = "Annual Growth Rates Across Countries",
    x = "Year",
    y = "GDP Growth Rate (%)",
    color = "Country"
  ) +
  theme_minimal(base_size = 14) +  # Clean, modern theme
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Print the plot
print(gdp_growth_plot)

# Optionally save the plot to a file
ggsave("GDP_Growth_Trajectories.png", plot = gdp_growth_plot, width = 10, height = 6)

# 2.3 Analysis
# Calculate cumulative growth for each country
cumulative_growth <- gdp_growth %>%
  group_by(Country) %>%
  summarise(
    Start_GDP = first(Value),
    End_GDP = last(Value),
    Cumulative_Growth = (End_GDP - Start_GDP) / Start_GDP * 100
  )

# View cumulative growth
print(cumulative_growth)

# Vietnam expereinced the fastest cumlative growth at 253.162. India is in second with 250% and Australia the developed country at 40.1%
#Reasons
#  Vietnam's young and growing population has contributed to an expanding labor force.
# Export oriented economy and entrence into WTO (Data point Exports as % of GDP)
#Significant government investment in infrastructure, such as roads, ports, and telecommunications, has improved connectivity and boosted trade.
# Eg (Gross capital formation % of GDP)


# Growth volatility
# Calculate the standard deviation of growth rates for each country
growth_volatility <- gdp_growth %>%
  group_by(Country) %>%
  summarise(
    Mean_Growth = mean(Growth_Rate, na.rm = TRUE),
    Std_Dev_Growth = sd(Growth_Rate, na.rm = TRUE)  # Standard deviation
  )

# View the volatility analysis
print(growth_volatility)

# Based on SD Australia had the most volatile growth

# Filter exports and capital formation from the `data` DataFrame
exports_data <- data %>%
  filter(Indicator == "Exports of goods and services (% of GDP)") %>%
  select(Country, Year, Exports = Value)

capital_data <- data %>%
  filter(Indicator == "Gross capital formation (% of GDP)") %>%
  select(Country, Year, Capital_Formation = Value)

# Merge the data frames (gdp_growth, exports_data, capital_data) by Country and Year
combined_data <- gdp_growth %>%
  select(Country, Year, Growth_Rate) %>%
  left_join(exports_data, by = c("Country", "Year")) %>%
  left_join(capital_data, by = c("Country", "Year"))

# View the combined dataset
print(head(combined_data))

library(ggplot2)

# Plot GDP growth against exports
ggplot(combined_data, aes(x = Exports, y = Growth_Rate, color = Country)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "GDP Growth vs. Exports",
    x = "Exports of Goods and Services (% of GDP)",
    y = "GDP Growth Rate (%)"
  ) +
  theme_minimal()

#Analysis
#The scatterplot shows no clear linear relationship between exports and GDP growth. This aligns with the regression results where the exports variable was not statistically significant.
#For Vietnam, the exports share is consistently higher than for India or Australia, but its relationship with growth appears weak.

# Plot GDP growth against capital formation
ggplot(combined_data, aes(x = Capital_Formation, y = Growth_Rate, color = Country)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "GDP Growth vs. Gross Capital Formation",
    x = "Gross Capital Formation (% of GDP)",
    y = "GDP Growth Rate (%)"
  ) +
  theme_minimal()

#Anlaysis 
#There is a visible positive trend between gross capital formation and GDP growth, especially for Vietnam and India. 
#This supports the regression result where capital formation had a positive and marginally significant impact on growth.



# Linear regression model
model <- lm(Growth_Rate ~ Exports + Capital_Formation, data = combined_data)

# View the regression summary
summary(model)

#Limitations. It is possible using variables such as % of GDP for exports and capital formation are not appropriate in this analysis
#for comparing GDP per Capita growth rate, many other macroeconomic variables likely play a roll.



#-----------------------------------------------------------------------------------------------------------------------------
#PART 3
# Filter GDP per capita data specifically for Vietnam
gdp_per_capita_data <- data %>%
  filter(Country == "Viet Nam", Indicator == "GDP per capita (constant 2015 US$)") %>%
  select(Country, Year, GDP_per_Capita = Value)

# Initialize vietnam_data using GDP growth data for Vietnam
vietnam_data <- gdp_growth %>%
  filter(Country == "Viet Nam")

# Merge GDP per capita data into vietnam_data
vietnam_data <- vietnam_data %>%
  left_join(gdp_per_capita_data, by = c("Country", "Year"))

# View the updated Vietnam-specific data
head(vietnam_data)

#3.1
# Country chose to anlyze is Vietnam
# Filter GDP per capita data for Vietnam
# Filter GDP per capita data specifically for Vietnam
gdp_per_capita_data <- data %>%
  filter(Country == "Viet Nam", Indicator == "GDP per capita (constant 2015 US$)") %>%
  select(Country, Year, GDP_per_Capita = Value)

# Initialize vietnam_data using GDP growth data for Vietnam
vietnam_data <- gdp_growth %>%
  filter(Country == "Viet Nam")

# Merge GDP per capita data into vietnam_data
vietnam_data <- vietnam_data %>%
  left_join(gdp_per_capita_data, by = c("Country", "Year"))

# View the updated Vietnam-specific data
head(vietnam_data)

# Add a period column based on year ranges
vietnam_data <- vietnam_data %>%
  mutate(Period = case_when(
    Year >= 1995 & Year <= 2007 ~ "1995-2007",
    Year >= 2008 & Year <= 2012 ~ "2008-2012",
    Year >= 2013 ~ "2013-Present"
  ))

# View the updated data
head(vietnam_data)

# Summarize growth rates and GDP per capita by period
growth_trends <- vietnam_data %>%
  group_by(Period) %>%
  summarise(
    Avg_Growth_Rate = mean(Growth_Rate, na.rm = TRUE),
    Avg_GDP_per_Capita = mean(GDP_per_Capita, na.rm = TRUE),
    Std_Dev_Growth = sd(Growth_Rate, na.rm = TRUE)  # Optional: Volatility measure
  )

# View the growth trends summary
print(growth_trends)

#Plot growht trends
library(ggplot2)

# Plot GDP Growth Trends for Vietnam with Periods
growth_trends_plot <- ggplot(vietnam_data, aes(x = Year, y = Growth_Rate, color = Period)) +
  geom_line(size = 1.2) +  # Line plot for growth rates
  geom_point(size = 2, aes(shape = Period)) +  # Points for individual data points
  labs(
    title = "Vietnam's GDP Growth Trends Over Time",
    subtitle = "Growth Rates Divided into Key Periods",
    x = "Year",
    y = "GDP Growth Rate (%)",
    color = "Period",
    shape = "Period"
  ) +
  theme_minimal(base_size = 14) +  # Minimal theme for a clean look
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Print the plot
print(growth_trends_plot)

# 1995–2007:

#Economic Reforms: Continued benefits from Đổi Mới reform
#Global Trade Integration: Vietnam joined the WTO in 2007, boosting exports and FDI.
#Exports Growth: Driven by textiles, agriculture, and light manufacturing.

#2008–2012:

#Global Financial Crisis: Slowed global demand impacted Vietnam’s export-heavy economy.
#Policy Adjustments: Fiscal stimulus and monetary policies stabilized the economy.

#2013–Present:
#2019 COVID-19 grinded global economiy to a hault, fast rebound


#Discussion
#High average GDP growth driven by export-led development and strong FDI inflows.
#Vietnam capitalized on its competitive labor force and integration into global trade networks.

# --------------------------------------------------------------------------------------------------------------------------

# PART 4

# Filter GDP per capita data
gdp_per_capita_data <- data %>%
  filter(Indicator == "GDP per capita (constant 2015 US$)") %>%
  select(Country, Year, GDP_per_Capita = Value)

# Merge GDP per capita into combined_data
combined_data <- combined_data %>%
  left_join(gdp_per_capita_data, by = c("Country", "Year"))

# View the updated combined_data
head(combined_data)


# GDP Growth vs. GDP Per Capita
# Scatterplot: Growth_Rate vs. GDP per Capita
ggplot(combined_data, aes(x = GDP_per_Capita, y = Growth_Rate, color = Country)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Growth Rate vs. GDP per Capita",
    x = "GDP per Capita (Constant 2015 US$)",
    y = "Growth Rate (%)"
  ) +
  theme_minimal()


# Growth vs Expots
# Scatterplot: Growth_Rate vs. Exports
ggplot(combined_data, aes(x = Exports, y = Growth_Rate, color = Country)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Growth Rate vs. Exports",
    x = "Exports (% of GDP)",
    y = "Growth Rate (%)"
  ) +
  theme_minimal()

#Growth_Rate vs. Capital Formation

# Scatterplot: Growth_Rate vs. Capital Formation
ggplot(combined_data, aes(x = Capital_Formation, y = Growth_Rate, color = Country)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Growth Rate vs. Capital Formation",
    x = "Capital Formation (% of GDP)",
    y = "Growth Rate (%)"
  ) +
  theme_minimal()


# Correlation Matix
# Compute correlations for each country
correlations <- combined_data %>%
  group_by(Country) %>%
  summarise(
    Growth_vs_Exports = cor(Growth_Rate, Exports, use = "complete.obs"),
    Growth_vs_Capital_Formation = cor(Growth_Rate, Capital_Formation, use = "complete.obs"),
    Growth_vs_GDP_per_Capita = cor(Growth_Rate, GDP_per_Capita, use = "complete.obs")
  )

# View correlation results
print(correlations)


# Regression Anlysis
# Regression model: Growth_Rate ~ Exports + Capital Formation + GDP per Capita
growth_model <- lm(Growth_Rate ~ Exports + Capital_Formation + GDP_per_Capita, data = combined_data)

# Summary of the regression model
summary(growth_model)


