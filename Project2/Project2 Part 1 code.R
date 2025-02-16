# Macro Project 2 Part 1


#a 
library(dplyr)

library(ggplot2)

data <- read.csv("France Data.csv")

data <- data %>%
  mutate(Yt = as.numeric(gsub("[$,]", "", Yt)),
         Kt = as.numeric(gsub("[$,]", "", Kt)),
         Lt = as.numeric(gsub("[$,]", "", Lt)))
data <- data %>%
  select(-c(X,X.1,X.2))

# Base year is 2017

alpha <- 0.33

# Compute Solow Residual (logA_t)
data <- data %>%
  mutate(log_At = log(Yt) - alpha * log(Kt) - (1 - alpha) * log(Lt),
         At = exp(log_At))  # Convert back to levels

head(data)



# Choose 2017 as base year
base_year <- 2017
A_base <- data %>% filter(Year == base_year) %>% pull(At)

# Normalize A_t relative to base year
data <- data %>%
  mutate(At_norm = At / A_base)

# View normalized values
head(data)


# Question 3
#Can you run a regression?

# No, cannot directly run an OLS regression because of multicolniarity or biased estimate



ggplot(data, aes(x = Year, y = At_norm)) +
  geom_line(color = "blue") +
  theme_minimal() +
  labs(title = "Solow Residual (TFP) for France",
       x = "Year", y = "Normalized Total Factor Productivity (A_t)")


# Question 4 

#analyzing the trend of At we see high growth in productive after 1995 which continues upward trajectory to 2008 financial crisis and then
# rebounds to new highs during the recovery. 
# The introduction of computers, and internet technology is a likely explanation of the rise of productivity in France 

# Load required library
library(dplyr)
library(ggplot2)

# Ensure Year is numeric
data <- data %>%
  mutate(Year = as.numeric(Year))

# Define parameters
alpha <- 0.33   # Capital elasticity
s <- 0.22       # Savings rate
delta <- 0.06   # Depreciation rate

# Compute initial capital per worker (k0) for the first year in the dataset
initial_year <- min(data$Year, na.rm = TRUE)  # Start from earliest year
k0 <- data %>% filter(Year == initial_year) %>% pull(Kt) / data %>% filter(Year == initial_year) %>% pull(Lt)

# Check if k0 is valid
if (is.na(k0) | length(k0) == 0) {
  stop("Error: k0 is missing. Check that the dataset contains valid values for Kt and Lt.")
}

# Initialize simulation for all years
num_years <- nrow(data)
k_t <- k0
k_path <- numeric(num_years)
k_path[1] <- k0  # Set initial value

# Simulate the path of k_t year by year
for (t in 2:num_years) {
  A_t <- data$At[t]  # Use actual A_t from data for each year
  k_t <- s * A_t * k_t^alpha + (1 - delta) * k_t  # Capital accumulation equation
  k_path[t] <- k_t
}

# Add k_t values to dataset
data <- data %>%
  mutate(kt = k_path)

# Compute aggregate output Y_t using the Solow production function
data <- data %>%
  mutate(Yt_simulated = At * kt^alpha * Lt)

# Compare simulated Y_t with actual Y_t
comparison <- data %>%
  select(Year, Yt, Yt_simulated)

# Print the updated comparison results
print(comparison)

# Plot actual vs simulated Y_t
ggplot(comparison, aes(x = Year)) +
  geom_line(aes(y = Yt, color = "Actual")) +
  geom_line(aes(y = Yt_simulated, color = "Simulated")) +
  theme_minimal() +
  labs(title = "Comparison of Actual and Simulated Aggregate Output",
       x = "Year", y = "Aggregate Output (Y_t)") +
  scale_color_manual(name = "Legend", values = c("Actual" = "red", "Simulated" = "blue"))




# --------------------------------------------------------------------------------------------------------

