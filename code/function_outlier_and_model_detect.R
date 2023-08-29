set.seed(123)
n <- 100
x <- rnorm(n)
y <- 2 * x + rnorm(n)
data <- data.frame(x, y) %>%
  rbind(
    c(0, 100)
    ,c(6, 2))

refine_lm_with_outliers <- function(data, max_iterations = 30, max_aic_decrease = 2, external_model = NULL) {
  iteration <- 1
  prev_aic <- 5e9
  initial_data <- data  # Store initial data

  # Data frames to store removed outliers and AIC values
  removed_outliers <- data.frame(iteration = integer(), index = integer())
  aic_values <- data.frame(iteration = integer(), aic = double())

  while (iteration <= max_iterations) {
    if (is.null(external_model)) {
      lm_model <- lm(y ~ x, data = data)
    } else {
      lm_formula <- as.formula(external_model)
      lm_model <- lm(lm_formula, data = data)
    }

    curr_aic <- AIC(lm_model)

    # Calculate the percent change in AIC
    aic_change_percent <- 100 * (prev_aic - curr_aic) / prev_aic

    message("Iteration", iteration, ": AIC =", dgt2(curr_aic), " (AIC %change = ", dgt2(aic_change_percent), "%)")

    # Store the AIC value for this iteration
    aic_values <- rbind(aic_values, data.frame(iteration = iteration, aic = curr_aic))

    # Check for negative AIC change and revert
    if (aic_change_percent < 0) {
      message("Negative AIC change detected. Reverting to previous iteration.")
      data <- initial_data  # Revert to previous iteration's data
      break
    }

    # Identify and remove high Cook's distance outliers
    cooks <- cooks.distance(lm_model)
    outlier_index <- which.max(cooks)
    if (length(outlier_index) == 0) {
      message("No outliers detected in iteration", iteration)
      break
    } else {
      message("Removing outlier in iteration", iteration)
      removed_outliers <- rbind(removed_outliers, data.frame(iteration = iteration, index = outlier_index))
      data <- data[-outlier_index, ]
    }

    prev_aic <- curr_aic
    iteration <- iteration + 1
  }

  return(
    list(
      lm_model = lm_model
      ,removed_outliers = removed_outliers
      ,aic_values = aic_values))
}

# Fit and refine the model
result <- refine_lm_with_outliers(
  data, max_iterations = 30
  ,external_model = "y~x")

# Print the final model summary
summary(result$lm_model)

# Print removed outliers
cat("Removed Outliers:\n")
print(result$removed_outliers)

# Print AIC values
cat("AIC Values:\n")
print(result$aic_values)














