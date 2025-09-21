library(survival)
library(survminer)
library(dplyr)
raw_data <- data.frame(
  time = c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,10,10,10,15,15,15,20,20,20,24,24,24),
  ce = c(100,100,100,100,100,100,100,95,96.5,81,79,85,32,41,39,33,35,37,24,21,19,11,9,6,0,0,0),
  cb = c(100,100,100,100,100,100,100,100,100,95,89,93,79,77,72,67,64,63,57,52,56,44,49,50,38,45,40)
)

llong_data <- raw_data %>%
  mutate(replicate = rep(1:3, length.out = n())) %>%
  tidyr::pivot_longer(cols = c(ce, cb), names_to = "species", values_to = "percent_alive") %>%
  mutate(
    n_alive = round(percent_alive),
    n_dead = 100 - n_alive,
    status = 1,
    time = as.numeric(time)
  )

expand_individuals <- function(df) {
  do.call(rbind, lapply(1:nrow(df), function(i) {
    row <- df[i, ]
    alive_rows <- if (row$n_alive > 0) {
      data.frame(time = row$time, status = 0, species = row$species, replicate = row$replicate)[rep(1, row$n_alive), ]
    } else NULL
    dead_rows <- if (row$n_dead > 0) {
      data.frame(time = row$time, status = 1, species = row$species, replicate = row$replicate)[rep(1, row$n_dead), ]
    } else NULL
    rbind(alive_rows, dead_rows)
  }))
}

surv_data <- expand_individuals(long_data)
surv_obj <- Surv(time = surv_data$time, event = surv_data$status)
km_fit <- survfit(surv_obj ~ species, data = surv_data)

ggsurvplot(
  km_fit, data = surv_data,
  conf.int = FALSE, pval = TRUE,
  risk.table = TRUE,
  title = "Bacterial killing assay Under P. aeruginosa infection",
  xlab = "Time (hours)",
  ylab = "Survival Probability",
  legend.title = "Species",
  legend.labs = c("C. brenneri isolate", "C. elegans N2")
)
# Log rank test
survdiff(Surv(time, status) ~ species, data = surv_data)
#cox test
cox_model <- coxph(Surv(time, status) ~ species, data = surv_data)
summary(cox_model)
