

#Ivermectin
# Step 1: Create the wide-format dataset
df <- tribble(
  ~Time, ~CB_0.05, ~CE_0.05, ~CB_0.1, ~CE_0.1, ~CB_0.25, ~CE_0.25, ~CB_0.5, ~CE_0.5, ~CB_1, ~CE_2,
  0,     1,        1,        1,       1,       1,        1,        1,       1,      1,     1,
  1,     0.902666667, 1,     0.717666667, 1,   0.72,     0.8367,   0.191667, 0.6567, 0,     0,
  3,     0.83,     0.66,     0.633333333, 0.4867, 0.603333333, 0.4967, 0.116666667, 0.2333, 0, 0,
  6,     0.166666667, 0.1267, 0.076666667, 0.0833, 0.040666667, 0.04, 0, 0.03, 0, 0,
  12,    0.060666667, 0.0367, 0.005333333, 0, 0, 0, 0, 0, 0, 0,
  24,    0.000366667, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  48,    0, 0, 0, 0, 0, 0, 0, 0, 0, 0
)

# Step 2: Reshape the data into long format
df_long <- df %>%
  pivot_longer(-Time, names_to = "Condition", values_to = "Survival") %>%
  mutate(
    Species = case_when(
      str_starts(Condition, "CB") ~ "C. brenneri (Syt11)",
      str_starts(Condition, "CE") ~ "C. elegans (N2)"
    ),
    Concentration = str_extract(Condition, "[0-9.]+") %>% as.numeric()
  ) %>%
  select(Time, Concentration, Species, Survival)

ggplot(df_filtered, aes(x = Time, y = Survival, color = Strain, shape = Strain)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  facet_wrap(~ Concentration, labeller = label_value) +
  labs(
    x = "Time (hours)",
    y = "Survival (%)",
    color = "Strain",
    shape = "Strain"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(face = "bold")
  ) +
  scale_color_manual(values = c("C. brenneri (Syt11)" = "#E69F00", "C. elegans (N2)" = "#0072B2"))




ggplot(df_filtered, aes(x = Time, y = Survival, color = Strain, shape = Strain)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  facet_wrap(~ Concentration, labeller = label_value) +
  labs(
    title = "(A) Ivermectin",
    x = "Time (hours)",
    y = "Survival (%)",
    color = "Strain",
    shape = "Strain"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(color = "grey90", size = 0.3),
    panel.grid.minor = element_line(color = "grey95", size = 0.2),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  ) +
  scale_color_manual(values = c("C. brenneri (Syt11)" = "#E69F00", "C. elegans (N2)" = "#0072B2"))



#Levamisole

# Load required libraries
library(tidyverse)

# Manually input your data
df <- tribble(
  ~Time, ~`C. brenneri (Syt11)_0.05 mg/ml`, ~`C. elegans (N2)_0.05 mg/ml`, ~`C. brenneri (Syt11)_0.1 mg/ml`, ~`C. elegans (N2)_0.1 mg/ml`,
  ~`C. brenneri (Syt11)_0.25 mg/ml`, ~`C. elegans (N2)_0.25 mg/ml`, ~`C. brenneri (Syt11)_0.5 mg/ml`, ~`C. elegans (N2)_0.5 mg/ml`,
  0,     1, 1, 1, 1, 1, 1, 1, 1,
  1,     0.987, 1, 0.897, 1, 0.57, 0.87, 0.21, 0.67,
  3,     0.851, 0.69, 0.71, 0.51, 0.29, 0.52, 0.1, 0.25,
  6,     0.55, 0.17, 0.58, 0.12, 0.11, 0.09, 0, 0.07,
  12,    0.49, 0.05, 0.42, 0, 0, 0, 0, 0,
  24,    0.39, 0, 0.17, 0, 0, 0, 0, 0,
  48,    0.26, 0, 0.0, 0, 0, 0, 0, 0
)

df_long <- df %>%
  pivot_longer(-Time, names_to = "Condition", values_to = "Survival") %>%
  mutate(
    Species = case_when(
      str_starts(Condition, "CB") ~ "C. brenneri (Syt11)",
      str_starts(Condition, "CE") ~ "C. elegans (N2)"
    ),
    Concentration = str_extract(Condition, "[0-9.]+") %>% as.numeric()
  ) %>%
  select(Time, Concentration, Species, Survival)

ggplot(df_filtered, aes(x = Time, y = Survival, color = Strain, shape = Strain)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  facet_wrap(~ Concentration, labeller = label_value) +
  labs(
    title = "(B) Levamisole",
    x = "Time (hours)",
    y = "Survival (%)",
    color = "Strain",
    shape = "Strain"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(color = "grey90", size = 0.3),
    panel.grid.minor = element_line(color = "grey95", size = 0.2),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  ) +
  scale_color_manual(values = c("C. brenneri (Syt11)" = "#E69F00", "C. elegans (N2)" = "#0072B2"))

#Albendazole
library(tidyverse)

df <- tribble(
  ~Time, ~Strain, ~Concentration, ~Survival,
  0, "C. brenneri (Syt11)", "0.05 mg/mL", 1,
  0, "C. elegans (N2)", "0.05 mg/mL", 1,
  0, "C. brenneri (Syt11)", "0.1 mg/mL", 1,
  0, "C. elegans (N2)", "0.1 mg/mL", 1,
  0, "C. brenneri (Syt11)", "0.25 mg/mL", 1,
  0, "C. elegans (N2)", "0.25 mg/mL", 1,
  0, "C. brenneri (Syt11)", "0.5 mg/mL", 1,
  0, "C. elegans (N2)", "0.5 mg/mL", 1,
  
  1, "C. brenneri (Syt11)", "0.05 mg/mL", 1,
  1, "C. elegans (N2)", "0.05 mg/mL", 1,
  1, "C. brenneri (Syt11)", "0.1 mg/mL", 1,
  1, "C. elegans (N2)", "0.1 mg/mL", 1,
  1, "C. brenneri (Syt11)", "0.25 mg/mL", 0.978667,
  1, "C. elegans (N2)", "0.25 mg/mL", 1,
  1, "C. brenneri (Syt11)", "0.5 mg/mL", 0.905,
  1, "C. elegans (N2)", "0.5 mg/mL", 1,
  
  3, "C. brenneri (Syt11)", "0.05 mg/mL", 1,
  3, "C. elegans (N2)", "0.05 mg/mL", 1,
  3, "C. brenneri (Syt11)", "0.1 mg/mL", 1,
  3, "C. elegans (N2)", "0.1 mg/mL", 1,
  3, "C. brenneri (Syt11)", "0.25 mg/mL", 0.91,
  3, "C. elegans (N2)", "0.25 mg/mL", 0.8867,
  3, "C. brenneri (Syt11)", "0.5 mg/mL", 0.8511,
  3, "C. elegans (N2)", "0.5 mg/mL", 0.85,
  
  6, "C. brenneri (Syt11)", "0.05 mg/mL", 0.996633,
  6, "C. elegans (N2)", "0.05 mg/mL", 0.9433,
  6, "C. brenneri (Syt11)", "0.1 mg/mL", 0.999967,
  6, "C. elegans (N2)", "0.1 mg/mL", 0.9133,
  6, "C. brenneri (Syt11)", "0.25 mg/mL", 0.753333,
  6, "C. elegans (N2)", "0.25 mg/mL", 0.8667,
  6, "C. brenneri (Syt11)", "0.5 mg/mL", 0.747667,
  6, "C. elegans (N2)", "0.5 mg/mL", 0.8033,
  
  12, "C. brenneri (Syt11)", "0.05 mg/mL", 0.98,
  12, "C. elegans (N2)", "0.05 mg/mL", 0.8367,
  12, "C. brenneri (Syt11)", "0.1 mg/mL", 0.97,
  12, "C. elegans (N2)", "0.1 mg/mL", 0.83,
  12, "C. brenneri (Syt11)", "0.25 mg/mL", 0.736667,
  12, "C. elegans (N2)", "0.25 mg/mL", 0.6933,
  12, "C. brenneri (Syt11)", "0.5 mg/mL", 0.75,
  12, "C. elegans (N2)", "0.5 mg/mL", 0.6433,
  
  24, "C. brenneri (Syt11)", "0.05 mg/mL", 0.963333,
  24, "C. elegans (N2)", "0.05 mg/mL", 0.49,
  24, "C. brenneri (Syt11)", "0.1 mg/mL", 0.96,
  24, "C. elegans (N2)", "0.1 mg/mL", 0.3567,
  24, "C. brenneri (Syt11)", "0.25 mg/mL", 0.71,
  24, "C. elegans (N2)", "0.25 mg/mL", 0.35,
  24, "C. brenneri (Syt11)", "0.5 mg/mL", 0.733333,
  24, "C. elegans (N2)", "0.5 mg/mL", 0.32,
  
  48, "C. brenneri (Syt11)", "0.05 mg/mL", 0.913333,
  48, "C. elegans (N2)", "0.05 mg/mL", 0.3033,
  48, "C. brenneri (Syt11)", "0.1 mg/mL", 0.883333,
  48, "C. elegans (N2)", "0.1 mg/mL", 0.17,
  48, "C. brenneri (Syt11)", "0.25 mg/mL", 0.683333,
  48, "C. elegans (N2)", "0.25 mg/mL", 0.0633,
  48, "C. brenneri (Syt11)", "0.5 mg/mL", 0.693333,
  48, "C. elegans (N2)", "0.5 mg/mL", 0,
)

df$Survival <- df$Survival * 100  # Convert to percentage

ggplot(df, aes(x = Time, y = Survival, color = Strain, shape = Strain)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  facet_wrap(~ Concentration, labeller = label_value) +
  labs(
    title = "(C) Albendazole",
    x = "Time (hours)",
    y = "Survival (%)",
    color = "Strain",
    shape = "Strain"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(color = "grey90", size = 0.3),
    panel.grid.minor = element_line(color = "grey95", size = 0.2),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  ) +
  scale_color_manual(values = c("C. brenneri (Syt11)" = "#E69F00", "C. elegans (N2)" = "#0072B2"))




