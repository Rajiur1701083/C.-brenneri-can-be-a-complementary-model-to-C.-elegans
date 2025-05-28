# Install necessary packages if not already installed
install.packages(c("survival", "survminer"))

# Load libraries
library(survival)
library(survminer)

# Reconstruct the dataset (assuming 100 individuals per species at start)
time_points <- c(2, 4, 6, 8)
ce_percent <- c(91.0633, 83.3967, 22.233333, 0)
cb_percent <- c(97.65, 94.85, 49.64, 0)

make_surv_data <- function(percent, species) {
  df <- data.frame()
  for (i in seq_along(time_points)) {
    n_alive <- round(percent[i])
    n_dead <- 100 - n_alive
    if (n_alive > 0) {
      alive <- data.frame(time = time_points[i], status = 0, species = species)
      alive <- alive[rep(1, n_alive), ]
      df <- rbind(df, alive)
    }
    if (n_dead > 0) {
      dead <- data.frame(time = time_points[i], status = 1, species = species)
      dead <- dead[rep(1, n_dead), ]
      df <- rbind(df, dead)
    }
  }
  return(df)
}

ce_data <- make_surv_data(ce_percent, "C. elegans")
cb_data <- make_surv_data(cb_percent, "C. brenneri")
surv_data <- rbind(ce_data, cb_data)
# Fit survival object
surv_obj <- Surv(time = surv_data$time, event = surv_data$status)

# Kaplan-Meier fit
km_fit <- survfit(surv_obj ~ species, data = surv_data)

# Plot survival curves
ggsurvplot(
  km_fit, data = surv_data, pval = TRUE, 
  risk.table = TRUE, conf.int = TRUE,
  title = "Kaplan-Meier Survival under Heat Stress",
  xlab = "Time (hours)", ylab = "Survival Probability",
  legend.title = "Species", legend.labs = c("C. elegans N2", "C. brenneri isolate")
)
# Log-rank test to compare survival curves
survdiff(Surv(time, status) ~ species, data = surv_data)

# Cox regression model
cox_model <- coxph(Surv(time, status) ~ species, data = surv_data)
summary(cox_model)





