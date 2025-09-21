# Load required libraries
library(survival)
library(survminer)
library(dplyr)
library(tidyr)

# Step 1: Input corrected survival data (both species)
ce <- c(100,100,98,97.5,97,89,85,82,72,67,61,56,51,44,40,36,31,25,21,15,14,13,12,11,10,9,6,4,3,1,0)  # 31 values
cb <- c(100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,97,96,94,93,87,75,71,67,61,57,42,
        47,45,44,40,38,27,24,19,17,15,14,13,11,7,7,4,1,1,0)  # 45 values

# Step 2: Create species-specific dataframes
ce_data <- data.frame(day = 1:31, percent_alive = ce, species = "C. elegans N2")
cb_data <- data.frame(day = 1:45, percent_alive = cb, species = "C. brenneri isolate")

# Step 3: Combine datasets
combined <- bind_rows(ce_data, cb_data) %>%
  mutate(n_alive = round(percent_alive),
         n_dead = 100 - n_alive,
         status = 1)

# Step 4: Expand to individual-level data
expand_individuals <- function(df) {
  do.call(rbind, lapply(1:nrow(df), function(i) {
    row <- df[i, ]
    alive <- if (row$n_alive > 0) {
      data.frame(time = row$day, status = 0, species = row$species)[rep(1, row$n_alive), ]
    } else NULL
    dead <- if (row$n_dead > 0) {
      data.frame(time = row$day, status = 1, species = row$species)[rep(1, row$n_dead), ]
    } else NULL
    rbind(alive, dead)
  }))
}

surv_data <- expand_individuals(combined)

# Step 5: Fit survival curves
fit <- survfit(Surv(time, status) ~ species, data = surv_data)

# Step 6: Plot Kaplan-Meier curves (no shaded CI)
ggsurvplot(fit, data = surv_data, 
           pval = TRUE, conf.int = FALSE,
           risk.table = TRUE,
           break.time.by = 5,
           xlim = c(1, 50),
           xlab = "Day of Survival", 
           ylab = "Survival Probability",
           palette = c("black", "grey50"),
           title = "Longevity: C. elegans N2 vs. C. brenneri isolate")
