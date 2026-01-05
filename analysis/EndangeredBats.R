# Load data
bats <- read.csv("texas_bats_final.csv", stringsAsFactors = FALSE)

# Filter only focal species
bats <- bats[bats$keep == "yes", ]

# Check
nrow(bats)
table(bats$keep)

# Create clean table for modeling
bats_model <- bats[, c(
  "high_risk",
  "populationTrend",
  "assessmentYear",
  "BodyMass_Value",
  "DietBreadth_proxy",
  "ForStrat_Value"
)]

# Check
str(bats_model)
summary(bats_model)

# Convert types
bats_model$high_risk        <- as.factor(bats_model$high_risk)
bats_model$populationTrend <- as.factor(bats_model$populationTrend)
bats_model$ForStrat_Value  <- as.factor(bats_model$ForStrat_Value)

# Check missing
colSums(is.na(bats_model))

# Imputation
bats_model$BodyMass_Value[is.na(bats_model$BodyMass_Value)] <-
  median(bats_model$BodyMass_Value, na.rm = TRUE)

bats_model$DietBreadth_proxy[is.na(bats_model$DietBreadth_proxy)] <-
  median(bats_model$DietBreadth_proxy, na.rm = TRUE)

# Handle categorical missing values
levels(bats_model$ForStrat_Value) <-
  c(levels(bats_model$ForStrat_Value), "Unknown")

bats_model$ForStrat_Value[is.na(bats_model$ForStrat_Value)] <- "Unknown"

# Check
colSums(is.na(bats_model))
summary(bats_model)

# Initial logistic regression (small sample separation issue)
m1 <- glm(high_risk ~ populationTrend + assessmentYear + BodyMass_Value + DietBreadth_proxy + ForStrat_Value,
          data = bats_model, family = binomial)

summary(m1)

## Simplified logistic model
# Fit the simplified model
m_simple <- glm(high_risk ~ BodyMass_Value + DietBreadth_proxy,
                data = bats_model, family = binomial)

# Generate predicted probabilities
bats_model$pred_prob <- predict(m_simple, type = "response")

# Create a results table
results <- data.frame(
  scientificName = bats$scientificName,
  high_risk = bats_model$high_risk,
  pred_prob = bats_model$pred_prob
)

# View
results[order(-results$pred_prob), ]

summary(m_simple)

## LOOCV
form <- high_risk ~ BodyMass_Value + DietBreadth_proxy

n <- nrow(bats_model)

# Store predicted probabilities and true labels
loocv_pred <- numeric(n)
loocv_true <- bats_model$high_risk

for (i in 1:n) {
  # Training data: all but i
  train_data <- bats_model[-i, ]

  # Test data: row i
  test_data <- bats_model[i, , drop = FALSE]

  # Fit model on training data
  m_loocv <- glm(form, data = train_data, family = binomial)

  # Predict probability for held-out observation
  loocv_pred[i] <- predict(m_loocv, newdata = test_data, type = "response")
}

# Combine results
loocv_results <- data.frame(
  scientificName = bats$scientificName,
  true_high_risk = loocv_true,
  pred_prob = loocv_pred
)

# View results sorted by predicted risk
loocv_results[order(-loocv_results$pred_prob), ]

loocv_results$pred_class <- ifelse(loocv_results$pred_prob >= 0.5, 1, 0)

# Confusion matrix w/ threshold = 0.5
table(
  Predicted = loocv_results$pred_class,
  Actual = loocv_results$true_high_risk
)

mean(loocv_results$pred_class == loocv_results$true_high_risk)

TP <- sum(loocv_results$pred_class == 1 & loocv_results$true_high_risk == 1)
TN <- sum(loocv_results$pred_class == 0 & loocv_results$true_high_risk == 0)
FP <- sum(loocv_results$pred_class == 1 & loocv_results$true_high_risk == 0)
FN <- sum(loocv_results$pred_class == 0 & loocv_results$true_high_risk == 1)

sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)

sensitivity
specificity

# Confusion matrix w/ threshold = 0.25
loocv_results$pred_class_025 <- ifelse(loocv_results$pred_prob >= 0.25, 1, 0)

table(
  Predicted = loocv_results$pred_class_025,
  Actual = loocv_results$true_high_risk
)

mean(loocv_results$pred_class_025 == loocv_results$true_high_risk)

## Summary table
# Threshold = 0.5
TP_05 <- sum(loocv_results$pred_class == 1 & loocv_results$true_high_risk == 1)
TN_05 <- sum(loocv_results$pred_class == 0 & loocv_results$true_high_risk == 0)
FP_05 <- sum(loocv_results$pred_class == 1 & loocv_results$true_high_risk == 0)
FN_05 <- sum(loocv_results$pred_class == 0 & loocv_results$true_high_risk == 1)

acc_05 <- (TP_05 + TN_05) / nrow(loocv_results)
sens_05 <- TP_05 / (TP_05 + FN_05)
spec_05 <- TN_05 / (TN_05 + FP_05)

# Threshold = 0.25
TP_025 <- sum(loocv_results$pred_class_025 == 1 & loocv_results$true_high_risk == 1)
TN_025 <- sum(loocv_results$pred_class_025 == 0 & loocv_results$true_high_risk == 0)
FP_025 <- sum(loocv_results$pred_class_025 == 1 & loocv_results$true_high_risk == 0)
FN_025 <- sum(loocv_results$pred_class_025 == 0 & loocv_results$true_high_risk == 1)

acc_025 <- (TP_025 + TN_025) / nrow(loocv_results)
sens_025 <- TP_025 / (TP_025 + FN_025)
spec_025 <- TN_025 / (TN_025 + FP_025)

# Create summary table
results_summary <- data.frame(
  Model = c("Logistic regression", "Logistic regression"),
  Validation = c("LOOCV", "LOOCV"),
  Threshold = c(0.50, 0.25),
  Accuracy = round(c(acc_05, acc_025), 3),
  Sensitivity = round(c(sens_05, sens_025), 3),
  Specificity = round(c(spec_05, spec_025), 3),
  Notes = c("Conservative threshold", "Precautionary threshold")
)

results_summary


## Data plot
install.packages("ggplot2")
library(ggplot2)

plot_df <- loocv_results
plot_df$true_high_risk <- as.factor(plot_df$true_high_risk)

# Order species by predicted risk (low -> high)
plot_df$scientificName <- factor(
  plot_df$scientificName,
  levels = plot_df$scientificName[order(plot_df$pred_prob)]
)

p2 <- ggplot(plot_df, aes(x = scientificName, y = pred_prob)) +
  geom_col() +
  geom_point(aes(shape = true_high_risk), size = 2) +
  coord_flip() +
  geom_hline(yintercept = 0.25, linetype = "dashed") +
  geom_hline(yintercept = 0.50, linetype = "dotted") +
  labs(
    title = "LOOCV Predicted Extinction Risk (Texas Bats)",
    subtitle = "Point shape indicates true high-risk label; dashed=0.25, dotted=0.50",
    x = "Species (sorted by predicted risk)",
    y = "Predicted P(high_risk)",
    shape = "True high_risk"
  ) +
  theme_minimal()

p2
# Save as png
ggsave("loocv_risk_by_species_singlepanel.png", plot = p2, width = 8.5, height = 6, dpi = 300)
