import numpy as np
import pandas as pd
from scipy.optimize import fsolve

# Parameters
beta = 0.95  # Discount factor
r = 0.05     # Interest rate
w1 = 100     # Initial wealth

# Function to solve for c1 and c2 with input validation
def equations(vars, w1, w2, beta, r):
    if w1 < 0 or w2 < 0 or r < 0:
        raise ValueError("w1, w2, and r must be non-negative.")
    c1, c2 = vars
    if c2 <= 0:
        return [np.inf, np.inf]  # Prevent division errors
    eq1 = c1 + (c2 / (1 + r)) - w1  # Budget constraint
    eq2 = beta * (1 + r) / c2 - 1 / c1  # First-order condition (FOC)
    return [eq1, eq2]

# Solving for c1 and c2 when w2 = 0
w2_values = [0, 100, 10000]
results = []

for w2 in w2_values:
    initial_guess = [w1 / 2, max((w1 * (1 + r)) / 2, 1e-5)]  # Ensure c2 is positive
    c1, c2 = fsolve(equations, initial_guess, args=(w1, w2, beta, r))
    results.append([w2, c1, c2])

# Convert results to DataFrame
results_df = pd.DataFrame(results, columns=["w2", "c1", "c2"])
print(results_df)

# Sensitivity analysis on beta and r
sensitivity_results = []

beta_values = [0.90, 0.95, 1.00]  # Different discount factors
r_values = [0.03, 0.05, 0.08]      # Different interest rates

for beta in beta_values:
    for r in r_values:
        initial_guess = [w1 / 2, max((w1 * (1 + r)) / 2, 1e-5)]  # Ensure c2 is positive
        c1, c2 = fsolve(equations, initial_guess, args=(w1, 0, beta, r))
        sensitivity_results.append([beta, r, c1, c2])

# Convert sensitivity results to DataFrame
sensitivity_df = pd.DataFrame(sensitivity_results, columns=["beta", "r", "c1", "c2"])
print(sensitivity_df)
