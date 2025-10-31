# COVID-19 Patient Data Visualizations with ggplot2
# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra)
library(ggalluvial)
library(viridis)

# Read and preprocess the data
data <- read.csv('output/dataset.csv', stringsAsFactors = FALSE)

data <- data %>%
  mutate(
    has_covid_vaccine = ifelse(has_covid_vaccine == "T", 1, 0),
    has_covid_admission = ifelse(has_covid_admission == "T", 1, 0),
    underlying_cause_death = ifelse(underlying_cause_death == "T", 1, 0),
    status_binary = ifelse(status == "dead", 1, 0),
    sex = as.factor(sex),
    vaccine_label = ifelse(has_covid_vaccine == 1, "Vaccinated", "Unvaccinated"),
    admission_label = ifelse(has_covid_admission == 1, "Admitted", "Not Admitted"),
    age_group = cut(age, breaks = c(0, 20, 40, 60, 80, 100, 130),
                    labels = c("0-20", "21-40", "41-60", "61-80", "81-100", "100+"))
  )

data$underlying_cause_death[is.na(data$underlying_cause_death) | 
                            data$underlying_cause_death == ""] <- 0

# Set theme for all plots
theme_set(theme_light(base_size = 12))
custom_colors <- c("alive" = "#2ecc71", "dead" = "#e74c3c")

# ============================================
# 1. SURVIVAL BY AGE GROUPS
# ============================================
p1 <- ggplot(data, aes(x = age_group, fill = status)) +
  geom_bar(position = "fill", alpha = 0.8) +
  scale_fill_manual(values = custom_colors) +
  scale_y_continuous(labels = percent) +
  labs(title = "Survival Rate by Age Group",
       x = "Age Group",
       y = "Proportion",
       fill = "Status") +
  theme_dark()

ggsave(
  filename = "output/1_survival_by_age.png",  # path and file name
  plot = p1,                                  # the plot object
  width = 8, height = 6,                        # size in inches
  dpi = 600                                     # resolution
)
# ============================================
# 2. ALLUVIAL DIAGRAM
# ============================================
alluvial_data <- data %>%
  group_by(vaccine_label, admission_label, status) %>%
  summarise(count = n(), .groups = 'drop')

p2 <- ggplot(alluvial_data,
       aes(axis1 = vaccine_label, axis2 = admission_label, axis3 = status,
           y = count)) +
  geom_alluvium(aes(fill = status), alpha = 0.7) +
  geom_stratum(width = 1/3, fill = "gray80", color = "white") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_fill_manual(values = custom_colors) +
  scale_x_discrete(limits = c("Vaccine Status", "Admission Status", "Outcome"),
                   expand = c(0.15, 0.05)) +
  labs(title = "Patient Flow: Vaccination → Admission → Outcome",
       subtitle = "Width represents number of patients",
       fill = "Outcome") +
  theme_light() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40"))
ggsave(
  filename = "output/2_alluvial_flow.png",  # path and file name
  plot = p2,                                  # the plot object
  width = 10, height = 6,                        # size in inches
  dpi = 600                                     # resolution
)
# ============================================
# 3. HEATMAP OF PATIENT CHARACTERISTICS
# ============================================
heatmap_data <- data %>%
  arrange(status_binary, age) %>%
  mutate(patient_id = factor(patient_id, levels = patient_id),
         age_scaled = scale(age)[,1]) %>%
  select(patient_id, sex, age_scaled, has_covid_vaccine, 
         has_covid_admission, status_binary) %>%
  pivot_longer(cols = c(age_scaled, has_covid_vaccine, has_covid_admission, status_binary),
               names_to = "variable", values_to = "value")

heatmap_data$variable <- factor(heatmap_data$variable,
                                levels = c("age_scaled", "has_covid_vaccine", 
                                          "has_covid_admission", "status_binary"),
                                labels = c("Age (scaled)", "Vaccinated", 
                                          "COVID Admission", "Died"))

p3 <- ggplot(heatmap_data, aes(x = variable, y = patient_id, fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  facet_grid(. ~ sex, scales = "free_y", space = "free_y") +
  scale_fill_gradient2(low = "#3498db", mid = "white", high = "#e74c3c",
                       midpoint = 0.5) +
  labs(title = "Patient Characteristics Heatmap",
       subtitle = "Grouped by sex, sorted by outcome and age",
       x = "", y = "Patient ID", fill = "Value") +
  theme(axis.text.y = element_text(size = 7),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray40"),
        legend.position = "right")

ggsave("output/3_patient_heatmap.png", p3, width = 12, height = 8, dpi = 300)

# ============================================
# 4. VIOLIN + BOX PLOT: AGE DISTRIBUTION
# ============================================
p4 <- ggplot(data, aes(x = status, y = age, fill = status)) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  geom_boxplot(width = 0.2, alpha = 0.8, outlier.shape = 21, outlier.size = 3) +
  geom_jitter(width = 0.1, alpha = 0.5, size = 2) +
  facet_wrap(~vaccine_label) +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Age Distribution by Outcome and Vaccination Status",
       x = "Status",
       y = "Age (years)",
       fill = "Status") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5),
        strip.background = element_rect(fill = "gray90"))

ggsave("output/4_age_distribution.png", p4, width = 10, height = 6, dpi = 300)

# ============================================
# 5. MOSAIC PLOT
# ============================================
mosaic_data <- data %>%
  group_by(vaccine_label, admission_label, status) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(vaccine_label, admission_label) %>%
  mutate(total = sum(count),
         proportion = count / total)

p5 <- ggplot(mosaic_data, aes(x = vaccine_label, y = proportion, 
                               fill = status, width = total)) +
  geom_bar(stat = "identity", position = "stack", color = "white", linewidth = 1) +
  facet_grid(. ~ admission_label) +
  scale_fill_manual(values = custom_colors) +
  scale_y_continuous(labels = percent) +
  labs(title = "Outcome Proportions: Vaccination × Admission Status",
       subtitle = "Bar width represents number of patients",
       x = "Vaccination Status",
       y = "Proportion",
       fill = "Outcome") +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray40"),
        strip.background = element_rect(fill = "gray90"))

ggsave("output/5_mosaic_plot.png", p5, width = 10, height = 6, dpi = 300)

# ============================================
# 6. FOREST PLOT (ODDS RATIOS)
# ============================================
# Run logistic regression
model <- glm(status_binary ~ sex + age + has_covid_admission + has_covid_vaccine,
             data = data, family = binomial(link = "logit"))

# Extract coefficients and confidence intervals
coef_data <- data.frame(
  variable = names(coef(model))[-1],  # Exclude intercept
  OR = exp(coef(model))[-1],
  CI_lower = exp(confint(model))[-1, 1],
  CI_upper = exp(confint(model))[-1, 2]
)

coef_data$variable <- gsub("sex", "Sex: ", coef_data$variable)
coef_data$variable <- gsub("age", "Age (per year)", coef_data$variable)
coef_data$variable <- gsub("has_covid_admission", "COVID Admission", coef_data$variable)
coef_data$variable <- gsub("has_covid_vaccine", "Vaccinated", coef_data$variable)

p6 <- ggplot(coef_data, aes(x = OR, y = reorder(variable, OR))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", linewidth = 1) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2, linewidth = 1) +
  geom_point(size = 4, color = "#3498db") +
  scale_x_log10() +
  labs(title = "Forest Plot: Odds Ratios for Death",
       subtitle = "Logistic regression results with 95% confidence intervals",
       x = "Odds Ratio (log scale)",
       y = "") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray40"),
        panel.grid.minor = element_blank())

ggsave("output/6_forest_plot.png", p6, width = 10, height = 6, dpi = 300)

# ============================================
# 7. STACKED BAR: MULTIPLE FACTORS
# ============================================
p7 <- ggplot(data, aes(x = sex, fill = status)) +
  geom_bar(position = "fill", alpha = 0.8) +
  facet_grid(vaccine_label ~ admission_label) +
  scale_fill_manual(values = custom_colors) +
  scale_y_continuous(labels = percent) +
  labs(title = "Outcome by Sex, Vaccination, and Admission Status",
       x = "Sex",
       y = "Proportion",
       fill = "Status") +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        strip.background = element_rect(fill = "gray90"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("output/7_multifactor_bars.png", p7, width = 10, height = 8, dpi = 300)

# ============================================
# 8. PREDICTED PROBABILITY VISUALIZATION
# ============================================
data$predicted_prob <- predict(model, type = "response")

p8 <- ggplot(data, aes(x = age, y = predicted_prob, color = status, shape = vaccine_label)) +
  geom_point(size = 4, alpha = 0.7) +
  geom_smooth(aes(group = vaccine_label), method = "loess", se = TRUE, alpha = 0.2) +
  facet_wrap(~admission_label) +
  scale_color_manual(values = custom_colors) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  labs(title = "Predicted Death Probability by Age",
       subtitle = "Separated by admission status, colored by actual outcome",
       x = "Age (years)",
       y = "Predicted Probability of Death",
       color = "Actual Status",
       shape = "Vaccine Status") +
  theme_light() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 9, color = "gray40"),
        strip.background = element_rect(fill = "gray90"))

ggsave("output/8_predicted_probability.png", p8, width = 12, height = 6, dpi = 300)

# ============================================
# 9. WAFFLE CHART STYLE
# ============================================
waffle_data <- data %>%
  group_by(status, sex) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(percentage = round(count / sum(count) * 100))

# Create grid coordinates
waffle_expanded <- waffle_data %>%
  uncount(percentage) %>%
  group_by(status, sex) %>%
  mutate(id = row_number()) %>%
  mutate(x = (id - 1) %% 10,
         y = (id - 1) %/% 10)

p9 <- ggplot(waffle_expanded, aes(x = x, y = y, fill = status)) +
  geom_tile(color = "white", linewidth = 0.5, width = 0.9, height = 0.9) +
  facet_wrap(~sex, ncol = 3) +
  scale_fill_manual(values = custom_colors) +
  coord_equal() +
  labs(title = "Patient Composition: Waffle Chart",
       subtitle = "Each square ≈ 1% of total patients",
       fill = "Status") +
  theme_light() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray40"),
        strip.text = element_text(face = "bold", size = 12))

ggsave("output/9_waffle_chart.png", p9, width = 10, height = 6, dpi = 300)

# ============================================
# 10. COMPREHENSIVE DASHBOARD
# ============================================
# Create smaller versions for dashboard
p_small1 <- ggplot(data, aes(x = age_group, fill = status)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Cases by Age Group", x = "", y = "Count") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

p_small2 <- ggplot(data, aes(x = status, y = age, fill = status)) +
  geom_boxplot() +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Age by Outcome", x = "", y = "Age") +
  theme(legend.position = "none")

p_small3 <- ggplot(data %>% group_by(vaccine_label, status) %>% 
                   summarise(count = n(), .groups = 'drop'),
                   aes(x = vaccine_label, y = count, fill = status)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Vaccination Status", x = "", y = "Count") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

p_small4 <- ggplot(data %>% group_by(admission_label, status) %>% 
                   summarise(count = n(), .groups = 'drop'),
                   aes(x = admission_label, y = count, fill = status)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = custom_colors) +
  scale_y_continuous(labels = percent) +
  labs(title = "Admission Impact", x = "", y = "Proportion") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

dashboard <- grid.arrange(p_small1, p_small2, p_small3, p_small4, 
                          ncol = 2, nrow = 2,
                          top = "COVID-19 Patient Data Dashboard")

ggsave("output/10_dashboard.png", dashboard, width = 12, height = 10, dpi = 300)

cat("\n=== ALL VISUALIZATIONS SAVED ===\n")
cat("1. 1_survival_by_age.png\n")
cat("2. 2_alluvial_flow.png\n")
cat("3. 3_patient_heatmap.png\n")
cat("4. 4_age_distribution.png\n")
cat("5. 5_mosaic_plot.png\n")
cat("6. 6_forest_plot.png\n")
cat("7. 7_multifactor_bars.png\n")
cat("8. 8_predicted_probability.png\n")
cat("9. 9_waffle_chart.png\n")
cat("10. 10_dashboard.png\n")