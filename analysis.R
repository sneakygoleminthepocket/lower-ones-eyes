# Read the original CSV. Treat ? as a missing value (NA).
ads <- read.csv(
    "add.csv",
    check.names = FALSE,
    strip.white = TRUE,
    na.strings = "?"
)

stopifnot(
    all(ads[["1558"]] %in% c("ad.", "nonad."))
)

# Identify exact duplicate records using all original predictors and the label.
duplicate_cols <- c(as.character(0:1557), "1558")
duplicate_key <- do.call(paste, c(ads[duplicate_cols], sep = "\r"))
duplicate_group <- match(duplicate_key, duplicate_key)

# Select and rename the five variables.
ads <- data.frame(
    height = as.numeric(ads[["0"]]),
    width = as.numeric(ads[["1"]]),
    ratio = as.numeric(ads[["2"]]),
    local = as.numeric(ads[["3"]]),
    target = ifelse(ads[["1558"]] == "ad.", 1L, 0L),
    group = duplicate_group
)

# Keep rows with at least one image dimension and a known local value.
ads <- ads[
    (!is.na(ads$height) | !is.na(ads$width)) & !is.na(ads$local),
]

# Group exact duplicates and identify their class.
groups <- split(seq_len(nrow(ads)), ads$group)
group_sizes <- lengths(groups)
group_target <- vapply(groups, function(i) ads$target[i[1]], integer(1))

# Select whole duplicate groups for an approximately 20% testing set.
select_test_groups <- function(ids, target_size) {
    ids <- sample(ids)
    sizes <- group_sizes[ids]
    n <- which.min(abs(c(0, cumsum(sizes)) - target_size)) - 1L
    ids[seq_len(n)]
}

# Perform a reproducible, stratified 80:20 split.
set.seed(2026)

test_groups <- c(
    select_test_groups(
        names(groups)[group_target == 1L],
        round(0.20 * sum(ads$target == 1L))
    ),
    select_test_groups(
        names(groups)[group_target == 0L],
        round(0.20 * sum(ads$target == 0L))
    )
)

test_index <- sort(unlist(groups[test_groups], use.names = FALSE))
train_index <- setdiff(seq_len(nrow(ads)), test_index)

train <- ads[train_index, ]
test <- ads[test_index, ]

# Calculate imputation means using training data only.
mean_height <- mean(train$height, na.rm = TRUE)
mean_width <- mean(train$width, na.rm = TRUE)

# Impute missing dimensions and recalculate affected aspect ratios.
impute <- function(data) {
    missing_height <- is.na(data$height)
    missing_width <- is.na(data$width)

    data$height[missing_height] <- mean_height
    data$width[missing_width] <- mean_width

    filled <- missing_height | missing_width
    data$ratio[filled] <- data$width[filled] / data$height[filled]

    data
}

# Prepare the complete dataset and both partitions.
cleaned <- impute(ads)
train <- impute(train)
test <- impute(test)

# Save the output CSV files.
columns <- c("height", "width", "ratio", "local", "target")

write.csv(cleaned[columns], "cleaned_dataset.csv", row.names = FALSE)
write.csv(train[columns], "training_dataset.csv", row.names = FALSE)
write.csv(test[columns], "testing_dataset.csv", row.names = FALSE)

# Read the complete cleaned dataset.
data <- read.csv("cleaned_dataset.csv")

# Check the required variables and values.
stopifnot(
    all(c("height", "width", "ratio", "local", "target") %in% names(data)),
    nrow(data) == 2375,
    !anyNA(data[c("height", "width", "ratio", "local", "target")]),
    all(data$target %in% c(0, 1)),
    all(data$local %in% c(0, 1))
)

# Prepare class labels and output folder.
data$Class <- factor(
    data$target,
    levels = c(1, 0),
    labels = c("Advertisement", "Non-advertisement")
)

out_dir <- "section4_outputs"
dir.create(out_dir, showWarnings = FALSE)

# 4.1. Class distribution.
class_counts <- table(data$Class)
class_table <- data.frame(
    Class = names(class_counts),
    Observations = as.integer(class_counts),
    Percentage = round(100 * as.numeric(prop.table(class_counts)), 2)
)

write.csv(
    class_table,
    file.path(out_dir, "class_distribution.csv"),
    row.names = FALSE
)

png(file.path(out_dir, "Figure_4_1_Class_Distribution.png"),
    width = 1600, height = 1000, res = 180)
bar_positions <- barplot(
    class_counts,
    col = "grey75",
    border = "grey25",
    ylim = c(0, max(class_counts) * 1.15),
    ylab = "Number of observations",
    xlab = "Class",
    main = "Distribution of Advertisement Classes"
)
text(bar_positions, as.numeric(class_counts),
     labels = format(as.numeric(class_counts), big.mark = ","),
     pos = 3)
dev.off()

# 4.2. Summary statistics for numerical variables.
numerical_variables <- c("height", "width", "ratio")
variable_labels <- c(
    height = "Height (pixels)",
    width = "Width (pixels)",
    ratio = "Aspect ratio"
)

summary_table <- do.call(rbind, lapply(numerical_variables, function(v) {
    x <- data[[v]]
    q <- quantile(x, probs = c(0.25, 0.75), names = FALSE)

    data.frame(
        Variable = unname(variable_labels[v]),
        Mean = mean(x),
        Median = median(x),
        SD = sd(x),
        Minimum = min(x),
        Q1 = q[1],
        Q3 = q[2],
        Maximum = max(x)
    )
}))

summary_table[-1] <- lapply(summary_table[-1], round, digits = 2)

ratio_row <- summary_table$Variable == "Aspect ratio"
summary_table$Minimum[ratio_row] <- round(min(data$ratio), 4)

write.csv(
    summary_table,
    file.path(out_dir, "numerical_summary.csv"),
    row.names = FALSE
)

# Histograms of height, width, and aspect ratio.
png(file.path(out_dir, "Figure_4_2_Numerical_Histograms.png"),
    width = 2400, height = 850, res = 180)
par(mfrow = c(1, 3), mar = c(4.5, 4.2, 3, 1))

hist(data$height, breaks = "FD", col = "grey80", border = "white",
     main = "(a) Image height", xlab = "Height (pixels)",
     ylab = "Number of observations")
hist(data$width, breaks = "FD", col = "grey80", border = "white",
     main = "(b) Image width", xlab = "Width (pixels)",
     ylab = "Number of observations")
hist(data$ratio, breaks = "FD", col = "grey80", border = "white",
     main = "(c) Aspect ratio", xlab = "Width-to-height ratio",
     ylab = "Number of observations")

dev.off()

# 4.3. Compare numerical variables between classes.
comparison_table <- do.call(rbind, lapply(numerical_variables, function(v) {
    ad <- data[data$target == 1, v]
    nonad <- data[data$target == 0, v]

    ad_q <- quantile(ad, probs = c(0.25, 0.75), names = FALSE)
    nonad_q <- quantile(nonad, probs = c(0.25, 0.75), names = FALSE)

    data.frame(
        Variable = unname(variable_labels[v]),
        Advertisement = sprintf(
            "%.2f (%.2f-%.2f)", median(ad), ad_q[1], ad_q[2]
        ),
        Non_advertisement = sprintf(
            "%.2f (%.2f-%.2f)", median(nonad), nonad_q[1], nonad_q[2]
        )
    )
}))

write.csv(
    comparison_table,
    file.path(out_dir, "numerical_comparison_by_class.csv"),
    row.names = FALSE
)

# Boxplots of height, width, and aspect ratio by class.
png(file.path(out_dir, "Figure_4_3_Numerical_Boxplots.png"),
    width = 2400, height = 850, res = 180)
par(mfrow = c(1, 3), mar = c(5, 4.2, 3, 1))

boxplot(height ~ Class, data = data, col = "grey80",
        main = "(a) Image height", xlab = "Class",
        ylab = "Height (pixels)")
boxplot(width ~ Class, data = data, col = "grey80",
        main = "(b) Image width", xlab = "Class",
        ylab = "Width (pixels)")
boxplot(ratio ~ Class, data = data, col = "grey80",
        main = "(c) Aspect ratio", xlab = "Class",
        ylab = "Width-to-height ratio")

dev.off()

# 4.4. Locality distribution by class.
locality_counts <- table(
    data$Class,
    factor(data$local, levels = c(0, 1), labels = c("local = 0", "local = 1"))
)
locality_percentages <- prop.table(locality_counts, margin = 1) * 100

locality_table <- data.frame(
    Class = rownames(locality_counts),
    Local_0_Count = as.integer(locality_counts[, 1]),
    Local_0_Percentage = round(locality_percentages[, 1], 2),
    Local_1_Count = as.integer(locality_counts[, 2]),
    Local_1_Percentage = round(locality_percentages[, 2], 2)
)

write.csv(
    locality_table,
    file.path(out_dir, "locality_distribution_by_class.csv"),
    row.names = FALSE
)


png(file.path(out_dir, "Figure_4_4_Locality_by_Class.png"),
    width = 1900, height = 1000, res = 180)
par(mar = c(5, 4.5, 4, 2))

bar_positions <- barplot(
    t(locality_percentages),
    beside = FALSE,
    col = c("grey80", "grey40"),
    ylim = c(0, 100),
    xlim = c(0, 3.8),
    ylab = "Percentage of observations",
    xlab = "Class",
    main = "Locality Distribution by Advertisement Class"
)

legend(
    x = max(bar_positions) + 0.55,
    y = 100,
    legend = rownames(t(locality_percentages)),
    fill = c("grey80", "grey40"),
    xjust = 0,
    yjust = 1,
    bty = "n"
)

dev.off()

# Section 5: Logistic regression and held-out prediction
data_dir <- "."
train <- read.csv(file.path(data_dir, "training_dataset.csv"))
test  <- read.csv(file.path(data_dir, "testing_dataset.csv"))

required <- c("height", "width", "ratio", "local", "target")
stopifnot(
  all(required %in% names(train)),
  all(required %in% names(test)),
  nrow(train) == 1900L,
  nrow(test) == 475L,
  !anyNA(train[required]),
  !anyNA(test[required]),
  all(train$target %in% c(0, 1)),
  all(test$target %in% c(0, 1)),
  all(train$local %in% c(0, 1)),
  all(test$local %in% c(0, 1)),
  sum(train$target == 1) == 305L,
  sum(train$target == 0) == 1595L,
  sum(test$target == 1) == 76L,
  sum(test$target == 0) == 399L
)

out_dir <- "section5_outputs"
dir.create(out_dir, showWarnings = FALSE)

# 5.1 Fit the multivariable logistic regression model.
# Aspect ratio is omitted because it is derived from height and width.
model <- glm(
  target ~ height + width + local,
  data = train,
  family = binomial(link = "logit")
)

if (!isTRUE(model$converged)) {
  stop("The logistic regression model did not converge.")
}

coef_table <- summary(model)$coefficients
ci <- suppressMessages(confint.default(model))

# Convert coefficients to odds ratios for a 10-pixel increase in dimensions.
# The locality odds ratio compares local = 1 with local = 0.
scales <- c("(Intercept)" = NA_real_, height = 10, width = 10, local = 1)
coefficient_output <- data.frame(
  Predictor = rownames(coef_table),
  Estimate = coef_table[, "Estimate"],
  Std_Error = coef_table[, "Std. Error"],
  z_value = coef_table[, "z value"],
  p_value = coef_table[, "Pr(>|z|)"],
  Adjusted_OR = NA_real_,
  OR_CI_Lower = NA_real_,
  OR_CI_Upper = NA_real_,
  row.names = NULL
)

for (i in seq_len(nrow(coefficient_output))) {
  term <- coefficient_output$Predictor[i]
  scale <- scales[term]
  if (!is.na(scale)) {
    b <- coef_table[term, "Estimate"]
    s <- coef_table[term, "Std. Error"]
    coefficient_output$Adjusted_OR[i] <- exp(scale * b)
    coefficient_output$OR_CI_Lower[i] <- exp(scale * (b - 1.96 * s))
    coefficient_output$OR_CI_Upper[i] <- exp(scale * (b + 1.96 * s))
  }
}

write.csv(
  coefficient_output,
  file.path(out_dir, "Table_5_1_Logistic_Coefficients.csv"),
  row.names = FALSE
)

# Global likelihood-ratio test against the intercept-only model.
null_model <- glm(target ~ 1, data = train, family = binomial(link = "logit"))
lr_stat <- 2 * (as.numeric(logLik(model)) - as.numeric(logLik(null_model)))
lr_df <- attr(logLik(model), "df") - attr(logLik(null_model), "df")
lr_p <- pchisq(lr_stat, df = lr_df, lower.tail = FALSE)

# 5.2 Predict the held-out observations at the single selected cutoff.
cutoff <- 0.5
prob <- predict(model, newdata = test, type = "response")
pred <- as.integer(prob >= cutoff)

cm <- table(
  Actual = factor(test$target, levels = c(0, 1),
                  labels = c("Non-advertisement", "Advertisement")),
  Predicted = factor(pred, levels = c(0, 1),
                     labels = c("Non-advertisement", "Advertisement"))
)
write.csv(as.data.frame.matrix(cm),
          file.path(out_dir, "Confusion_Matrix.csv"))

tn <- unname(cm[1, 1])
fp <- unname(cm[1, 2])
fn <- unname(cm[2, 1])
tp <- unname(cm[2, 2])

accuracy <- (tp + tn) / (tp + tn + fp + fn)
precision <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
recall <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
specificity <- if ((tn + fp) > 0) tn / (tn + fp) else NA_real_
f1 <- if (is.finite(precision) && is.finite(recall) &&
           (precision + recall) > 0) {
  2 * precision * recall / (precision + recall)
} else {
  NA_real_
}
balanced_accuracy <- (recall + specificity) / 2

# AUC calculated from ranks (equivalent to the trapezoidal ROC area).
rank_sum <- rank(prob, ties.method = "average")
n1 <- sum(test$target == 1)
n0 <- sum(test$target == 0)
auc <- (sum(rank_sum[test$target == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)

baseline_accuracy <- mean(test$target == 0)
performance_output <- data.frame(
  Classifier = c("Majority-class baseline", "Logistic regression"),
  TN = c(sum(test$target == 0), tn),
  FP = c(0, fp),
  FN = c(sum(test$target == 1), fn),
  TP = c(0, tp),
  Accuracy = c(baseline_accuracy, accuracy),
  Precision = c(NA_real_, precision),
  Recall = c(0, recall),
  Specificity = c(1, specificity),
  F1 = c(0, f1),
  Balanced_Accuracy = c(0.5, balanced_accuracy),
  AUC = c(NA_real_, auc)
)
write.csv(
  performance_output,
  file.path(out_dir, "Table_5_2_Test_Performance.csv"),
  row.names = FALSE
)

# Construct ROC coordinates over all unique probability thresholds.
thresholds <- c(Inf, sort(unique(prob), decreasing = TRUE))
roc_points <- data.frame(
  Threshold = thresholds,
  FPR = vapply(thresholds, function(t) {
    mean(prob[test$target == 0] >= t)
  }, numeric(1)),
  TPR = vapply(thresholds, function(t) {
    mean(prob[test$target == 1] >= t)
  }, numeric(1))
)
write.csv(roc_points, file.path(out_dir, "ROC_Curve_Coordinates.csv"),
          row.names = FALSE)

# ROC operating point for the 0.5 cutoff.
point_fpr <- fp / (fp + tn)
point_tpr <- tp / (tp + fn)

png(file.path(out_dir, "Figure_5_1_ROC_Curve.png"),
    width = 1800, height = 1400, res = 220)
par(mar = c(5, 5, 4, 2))
plot(roc_points$FPR, roc_points$TPR,
     type = "l", lwd = 2,
     xlim = c(0, 1), ylim = c(0, 1),
     xaxs = "i", yaxs = "i",
     xlab = "False positive rate (1 - specificity)",
     ylab = "True positive rate (recall)",
     main = sprintf("ROC Curve for Advertisement Classification (AUC = %.3f)", auc))
abline(0, 1, lty = 2)
points(point_fpr, point_tpr, pch = 19, cex = 1.2)
text(point_fpr, point_tpr,
     labels = "0.5 cutoff", pos = 4, offset = 0.8, cex = 0.9)
legend("bottomright",
       legend = c("Logistic regression", "No-discrimination reference", "0.5 cutoff"),
       lty = c(1, 2, NA), pch = c(NA, NA, 19), bty = "n")
dev.off()

# Save predicted probabilities for reproducibility and checking.
prediction_output <- data.frame(
  Actual = test$target,
  Predicted_Probability = prob,
  Predicted_Class = pred
)
write.csv(prediction_output,
          file.path(out_dir, "Test_Set_Predictions.csv"),
          row.names = FALSE)

cat("Training observations:", nrow(train), "\n")
cat("Testing observations:", nrow(test), "\n")
cat("Model likelihood-ratio test: chi-square =", round(lr_stat, 2),
    ", df =", lr_df, ", p-value =", format.pval(lr_p), "\n")
cat("Confusion matrix: TN =", tn, "FP =", fp, "FN =", fn, "TP =", tp, "\n")
cat("Accuracy =", round(accuracy, 4),
    "Precision =", round(precision, 4),
    "Recall =", round(recall, 4),
    "Specificity =", round(specificity, 4),
    "F1 =", round(f1, 4),
    "Balanced accuracy =", round(balanced_accuracy, 4),
    "AUC =", round(auc, 4), "\n")
cat("Outputs written to:", out_dir, "\n")
