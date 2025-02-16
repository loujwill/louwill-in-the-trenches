# Project 2 Part 2

# Calibration

#Base Year 2017
#France’s historical savings rate(s) is around 20%-25% (0.20 - 0.25)

#Depreciation Rate (δ)
#Typically assumed 5% to 10% (0.05 - 0.10) for capital stock depreciation.

#Capital Elasticity (α) = 0.33
#α is the capital share of income.
#For developed economies, 
#𝛼= 0.33 α=0.33 (1/3 of output comes from capital, 2/3 from labor).

# Initial capital stock per worker
# k0 = K0/L0


alpha <- 0.33  # Capital elasticity
s <- 0.22      # Savings rate 
delta <- 0.06  # Depreciation rate (Common assumption)

# Compute k_t (capital per worker)
data <- data %>%
  mutate(kt = Kt / Lt)

# Choose base year (2017)
base_year <- 2017
A_base <- data %>% filter(Year == base_year) %>% pull(At)
k_base <- data %>% filter(Year == base_year) %>% pull(kt)


k0 <- data %>%
  filter(Year == base_year) %>%
  summarize(k0 = Kt / Lt) %>%
  pull(k0)

# Print k_0
print(paste("Initial capital stock per worker (k0) in", base_year, ":", k0))

#"Initial capital stock per worker (k0) in 2017 : 0.534078215420031"


# ---------------------------------------------------------------------------------
#Simulation


# Initialize the simulation
years <- 100  # Simulate for 100 periods
k_path <- numeric(years)
k_path[1] <- k_base

# Simulate the path of k_t
for (t in 2:years) {
  k_path[t] <- s * A_base * k_path[t-1]^alpha + (1 - delta) * k_path[t-1]
}

# Convert to a data frame for plotting
sim_data <- data.frame(Year = base_year:(base_year + years - 1), kt = k_path)

# Plot the simulation of k_t
ggplot(sim_data, aes(x = Year, y = kt)) +
  geom_line(color = "blue") +
  theme_minimal() +
  labs(title = "Path of Capital per Worker (k_t) in France",
       x = "Year", y = "Capital per Worker")




# Verification 
# Compute steady-state k*
k_star <- (s * A_base / delta)^(1 / (1 - alpha))

# Print k*
print(paste("Steady-state capital per worker (k*):", k_star))

# k0 > k_star
# 0.534 > 0.280 This concurs with chart



# An

# Define parameters
alpha <- 0.33   # Capital elasticity
s <- 0.22       # Savings rate
delta <- 0.06   # Depreciation rate

# Compute initial capital per worker (k0) and steady-state (k*)
k0 <- data %>% filter(Year == 2017) %>% summarize(k0 = Kt / Lt) %>% pull(k0)
A_base <- data %>% filter(Year == 2017) %>% pull(At)
k_star <- (s * A_base / delta)^(1 / (1 - alpha))

# Convergence threshold
epsilon <- 1e-4  

# Initialize simulation
k_t <- k0
k_path <- c(k0)
years <- 0

# Iterate until k_t converges to k*
while (abs(k_t - k_star) > epsilon) {
  k_t <- s * A_base * k_t^alpha + (1 - delta) * k_t  # Capital accumulation equation
  k_path <- c(k_path, k_t)
  years <- years + 1
}

# Create a dataframe for plotting
sim_data <- data.frame(Year = 2017:(2017 + years), kt = k_path)

# Print number of years until convergence
print(paste("Years until convergence:", years))

# Plot the path of k_t
library(ggplot2)
ggplot(sim_data, aes(x = Year, y = kt)) +
  geom_line(color = "blue") +
  theme_minimal() +
  labs(title = "Path of Capital per Worker (k_t) Until Convergence",
       x = "Year", y = "Capital per Worker")



# "Years until convergence: 189"





# ----------------------------------------------------------------------------
#Altering parameters

# Define parameters
alpha <- 0.35   # Capital elasticity
s <- 0.25       # Savings rate
delta <- 0.05   # Depreciation rate

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




# ---------------------------------------------------------------------------------------------
# Define parameters
alpha <- 0.4   # Capital elasticity
s <- 0.3       # Savings rate
delta <- 0.04   # Depreciation rate

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



#Calculate the path of aggregate output Yt based on the model using the Solow residual
#(At) and labor supply (Lt) in previous question and compare it with the data. Discuss
#your findings.


data$Year = as.numeric(data$Year)

