# Project 2 Part 3

#a

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Define parameters
alpha <- 0.40   # More capital-intensive economy
s <- 0.30       # Higher savings rate
delta <- 0.04   # Lower depreciation rate

# Compute initial capital per worker (k0) and steady-state (k*)
k0 <- data %>% filter(Year == 2017) %>% summarize(k0 = Kt / Lt) %>% pull(k0)
A_base <- data %>% filter(Year == 2017) %>% pull(At)
k_star <- (s * A_base / delta)^(1 / (1 - alpha))

# Convergence threshold
epsilon <- 1e-5  

# Initialize simulation
k_t <- k0
k_path <- numeric(length(data$Year))
k_path[1] <- k0  # Set initial value

# Simulate the path of k_t year by year
for (t in 2:length(data$Year)) {
  A_t <- data$At[t]  # Use actual A_t from data for each year
  k_t <- s * A_t * k_t^alpha + (1 - delta) * k_t  # Capital accumulation equation
  k_path[t] <- k_t
}

# Add k_t values to dataset
data <- data %>%
  mutate(kt = k_path)

# Compute aggregate output Y_t using Solow model equation
data <- data %>%
  mutate(Yt_simulated = At * kt^alpha * Lt)

# Compute steady-state capital per worker for each year
data <- data %>%
  mutate(kt_steady = (s * At / delta)^(1 / (1 - alpha)))

# Compute steady-state output for each year
data <- data %>%
  mutate(Yt_steady = At * (kt_steady^alpha) * Lt)

# Compare simulated Y_t, steady-state Y_t, and actual Y_t
comparison <- data %>%
  select(Year, Yt, Yt_simulated, Yt_steady)

# Print comparison results
print(comparison)

# Plot actual vs simulated vs steady-state Y_t
ggplot(comparison, aes(x = Year)) +
  geom_line(aes(y = Yt, color = "Actual")) +
  geom_line(aes(y = Yt_simulated, color = "Simulated")) +
  geom_line(aes(y = Yt_steady, color = "Steady-State"), linetype = "dashed") +
  theme_minimal() +
  labs(title = "Comparison of Actual, Simulated, and Steady-State Aggregate Output",
       x = "Year", y = "Aggregate Output (Y_t)") +
  scale_color_manual(name = "Legend", values = c("Actual" = "red", "Simulated" = "blue", "Steady-State" = "black"))

