

source("scripts/simulation/simulate_pull.R")

cat("\n========== Running simulate_pull() ==========\n")
out <- simulate_pull(N_SIM = 500, min_surplus = 0.005)

cat("\n========== Running stress_test_weights() ==========\n")
sens <- stress_test_weights(out)

cat("\n========== Surplus threshold sensitivity summary ==========\n")
print(sens$surplus_sens)

cat("\n========== Weight scheme summary ==========\n")
print(sens$summary)

cat("\nDone.\n")
