# COVID-19 Patient Data Visualizations
# Advanced visualizations for 2500+ patient dataset

library(ggplot2)
library(dplyr)
library(gridExtra)
library(viridis)
library(reshape2)

# Read and preprocess data
data <- read.csv('output/dataset.csv', stringsAsFactors = FALSE)

data <- data %>%
  mutate(
    has_covid_vaccine = ifelse(has_covid_vaccine == "T", 1, 0),
    has_covid_admission = ifelse(has_covid_admission == "T", 1, 0),
    status_binary = ifelse(status == "dead", 1, 0),
    sex = as.factor(sex),
    age_group = cut(age, breaks = c(0, 18, 40, 60, 80, 150),
                    labels = c("0-18", "19-40", "41-60", "61-80", "80+"))
  )

# Set theme
theme_set(theme_light(base_size = 12))

# ============================================
# 1. SURVIVAL OVERVIEW DASHBOARD
# ============================================

p1 <- ggplot(data, aes(x = status, fill = status)) +
  geom_bar(width = 0.6) +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  scale_fill_manual(values = c("alive" = "#2ecc71", "dead" = "#e74c3c")) +
  labs(title = "Overall Survival Status", x = "", y = "Count") +
  theme(legend.position = "none")

p2 <- ggplot(data, aes(x = factor(has_covid_vaccine), fill = status)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("alive" = "#2ecc71", "dead" = "#e74c3c")) +
  scale_x_discrete(labels = c("No Vaccine", "Vaccinated")) +
  labs(title = "Survival by Vaccination Status", x = "", y = "Proportion") +
  theme(legend.title = element_blank())

p3 <- ggplot(data, aes(x = sex, fill = status)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("alive" = "#2ecc71", "dead" = "#e74c3c")) +
  labs(title = "Survival by Sex", x = "", y = "Proportion") +
  theme(legend.title = element_blank())

p4 <- ggplot(data, aes(x = factor(has_covid_admission), fill = status)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("alive" = "#2ecc71", "dead" = "#e74c3c")) +
  scale_x_discrete(labels = c("No Admission", "Admitted")) +
  labs(title = "Survival by Hospital Admission", x = "", y = "Proportion") +
  theme(legend.title = element_blank())

png("dashboard_overview.png", width = 1200, height = 800, res = 100)
new_dashboard <- grid.arrange(p1, p2, p3, p4, ncol = 2,
             top = "COVID-19 Patient Survival Dashboard")
ggsave("output/11_new_dashboard.png", new_dashboard, width = 12, height = 10, dpi = 300)
dev.off()

# ============================================
# 2. AGE DISTRIBUTION ANALYSIS
# ============================================

# Age distribution by survival status
p_age1 <- ggplot(data, aes(x = age, fill = status)) +
  geom_histogram(bins = 40, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("alive" = "#2ecc71", "dead" = "#e74c3c")) +
  labs(title = "Age Distribution by Survival Status",
       x = "Age", y = "Count") +
  theme(legend.title = element_blank())

# Age density plot
p_age2 <- ggplot(data, aes(x = age, fill = status)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("alive" = "#2ecc71", "dead" = "#e74c3c")) +
  labs(title = "Age Density Distribution",
       x = "Age", y = "Density") +
  theme(legend.title = element_blank())

png("age_distributions.png", width = 1200, height = 500, res = 100)
age_dist_dash <- grid.arrange(p_age1, p_age2, ncol = 2)
ggsave("output/12_dashboard_age.png", age_dist_dash, width = 12, height = 10, dpi = 300)
dev.off()

# ============================================
# 3. AGE GROUP HEATMAP
# ============================================

# Calculate mortality rate by age group and other factors
heatmap_data <- data %>%
  group_by(age_group, has_covid_vaccine, has_covid_admission) %>%
  summarise(
    mortality_rate = mean(status_binary) * 100,
    count = n(),
    .groups = "drop"
  ) %>%
  mutate(
    vaccine_label = ifelse(has_covid_vaccine == 1, "Vaccinated", "Not Vaccinated"),
    admission_label = ifelse(has_covid_admission == 1, "Admitted", "Not Admitted")
  )

p_heatmap <- ggplot(heatmap_data, 
                    aes(x = age_group, y = paste(vaccine_label, admission_label, sep = "\n"))) +
  geom_tile(aes(fill = mortality_rate), color = "white", size = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", mortality_rate, count)), 
            size = 3, color = "white") +
  scale_fill_viridis(option = "plasma", name = "Mortality\nRate (%)") +
  labs(title = "Mortality Rate Heatmap by Age Group, Vaccination, and Admission Status",
       x = "Age Group", y = "") +
  theme(axis.text.x = element_text(angle = 0),
        panel.grid = element_blank())

ggsave("output/13_mortality_heatmap.png", p_heatmap, width = 12, height = 6, dpi = 100)

# ============================================
# 4. SANKEY-STYLE FLOW DIAGRAM (Bar-based)
# ============================================

# Create flow data
flow_data <- data %>%
  count(has_covid_vaccine, has_covid_admission, status) %>%
  mutate(
    vaccine_status = ifelse(has_covid_vaccine == 1, "Vaccinated", "Not Vaccinated"),
    admission_status = ifelse(has_covid_admission == 1, "Admitted", "Not Admitted")
  )

p_flow <- ggplot(flow_data, aes(x = 1, y = n, fill = vaccine_status)) +
  geom_col(position = "stack", width = 0.3) +
  geom_col(aes(x = 2, fill = admission_status), position = "stack", width = 0.3) +
  geom_col(aes(x = 3, fill = status), position = "stack", width = 0.3) +
  scale_fill_manual(values = c("Vaccinated" = "#3498db", "Not Vaccinated" = "#95a5a6",
                                "Admitted" = "#e67e22", "Not Admitted" = "#f39c12",
                                "alive" = "#2ecc71", "dead" = "#e74c3c")) +
  scale_x_continuous(breaks = 1:3, labels = c("Vaccination", "Admission", "Outcome")) +
  labs(title = "Patient Journey Flow", y = "Number of Patients") +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.x = element_blank())

ggsave("output/14_patient_flow.png", p_flow, width = 10, height = 6, dpi = 100)

# ============================================
# 5. RISK STRATIFICATION PLOT
# ============================================

# Calculate risk scores
risk_data <- data %>%
  group_by(age_group, has_covid_vaccine, has_covid_admission) %>%
  summarise(
    mortality_rate = mean(status_binary),
    count = n(),
    .groups = "drop"
  ) %>%
  mutate(
    risk_category = case_when(
      mortality_rate < 0.1 ~ "Low Risk",
      mortality_rate < 0.3 ~ "Moderate Risk",
      mortality_rate < 0.5 ~ "High Risk",
      TRUE ~ "Critical Risk"
    ),
    risk_category = factor(risk_category, 
                          levels = c("Low Risk", "Moderate Risk", "High Risk", "Critical Risk"))
  )

p_risk <- ggplot(risk_data, aes(x = age_group, y = mortality_rate * 100, 
                                 size = count, color = risk_category)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("Low Risk" = "#2ecc71", "Moderate Risk" = "#f2ff39ff",
                                 "High Risk" = "#fd0202ff", "Critical Risk" = "#000000ff")) +
  scale_size_continuous(range = c(3, 15), name = "Patient Count") +
  labs(title = "Risk Stratification by Age Group",
       subtitle = "Bubble size represents patient count",
       x = "Age Group", y = "Mortality Rate (%)",
       color = "Risk Category") +
  theme(legend.position = "right")

ggsave("output/15_risk_stratification.png", p_risk, width = 12, height = 6, dpi = 100)

# ============================================
# 6. VIOLIN PLOT: AGE DISTRIBUTION BY GROUPS
# ============================================

data_plot <- data %>%
  mutate(group = paste(
    ifelse(has_covid_vaccine == 1, "Vax", "No Vax"),
    ifelse(has_covid_admission == 1, "Admit", "No Admit"),
    sep = " + "
  ))

p_violin <- ggplot(data_plot, aes(x = group, y = age, fill = status)) +
  geom_violin(alpha = 0.7, position = position_dodge(width = 0.9)) +
  geom_boxplot(width = 0.1, position = position_dodge(width = 0.9), 
               alpha = 0.5, outlier.shape = NA) +
  scale_fill_manual(values = c("alive" = "#2ecc71", "dead" = "#e74c3c")) +
  labs(title = "Age Distribution Across Treatment Groups",
       x = "Treatment Group", y = "Age") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())

ggsave("output/16_age_violin_plot.png", p_violin, width = 10, height = 6, dpi = 100)

# ============================================
# 7. MORTALITY RATE BY SEX AND AGE
# ============================================

mortality_by_sex_age <- data %>%
  group_by(sex, age_group) %>%
  summarise(
    mortality_rate = mean(status_binary) * 100,
    count = n(),
    .groups = "drop"
  )

p_sex_age <- ggplot(mortality_by_sex_age, 
                    aes(x = age_group, y = mortality_rate, 
                        group = sex, color = sex)) +
  geom_line(size = 1.2) +
  geom_point(aes(size = count), alpha = 0.7) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Mortality Rate by Sex and Age Group",
       subtitle = "Point size represents patient count",
       x = "Age Group", y = "Mortality Rate (%)") +
  theme(legend.title = element_blank())

ggsave("output/17_mortality_by_sex_age.png", p_sex_age, width = 10, height = 6, dpi = 100)

# ============================================
# 8. CORRELATION MATRIX
# ============================================

# Create correlation matrix
cor_data <- data %>%
  select(age, has_covid_vaccine, has_covid_admission, status_binary) %>%
  cor()

cor_melted <- melt(cor_data)

p_cor <- ggplot(cor_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", value)), color = "white", size = 5) +
  scale_fill_gradient2(low = "#3498db", mid = "white", high = "#e74c3c",
                       midpoint = 0, name = "Correlation") +
  labs(title = "Correlation Matrix of Variables",
       x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

ggsave("output/18_correlation_matrix.png", p_cor, width = 8, height = 6, dpi = 100)

# ============================================
# 9. SURVIVAL CURVE (Kaplan-Meier Style Visual)
# ============================================

# Simulate cumulative mortality by age
survival_data <- data %>%
  arrange(age) %>%
  group_by(has_covid_vaccine) %>%
  mutate(
    cumulative_deaths = cumsum(status_binary),
    total = n(),
    survival_rate = 1 - (cumulative_deaths / total)
  ) %>%
  ungroup()

p_survival <- ggplot(survival_data, 
                     aes(x = age, y = survival_rate * 100, 
                         color = factor(has_covid_vaccine))) +
  geom_line(size = 1.2, alpha = 0.7) +
  scale_color_manual(values = c("0" = "#e74c3c", "1" = "#2ecc71"),
                     labels = c("Not Vaccinated", "Vaccinated")) +
  labs(title = "Survival Rate by Age and Vaccination Status",
       x = "Age", y = "Survival Rate (%)",
       color = "Status") +
  theme(legend.position = "top")

ggsave("output/19_survival_curve.png", p_survival, width = 10, height = 6, dpi = 100)

# ============================================
# 10. INTERACTIVE-STYLE SUMMARY PANEL
# ============================================

# Calculate key statistics
stats <- data %>%
  summarise(
    total_patients = n(),
    total_deaths = sum(status_binary),
    mortality_rate = mean(status_binary) * 100,
    avg_age = mean(age),
    vaccinated = sum(has_covid_vaccine),
    admitted = sum(has_covid_admission)
  )

# Create summary text plot
png("output/20_summary_statistics.png", width = 1000, height = 600, res = 100)
par(mar = c(0, 0, 0, 0))
plot.new()
text(0.5, 0.9, "COVID-19 PATIENT DATASET SUMMARY", cex = 2.5, font = 2)

text(0.25, 0.7, sprintf("Total Patients\n%d", stats$total_patients), 
     cex = 2, col = "#3498db", font = 2)
text(0.5, 0.7, sprintf("Total Deaths\n%d", stats$total_deaths), 
     cex = 2, col = "#e74c3c", font = 2)
text(0.75, 0.7, sprintf("Mortality Rate\n%.1f%%", stats$mortality_rate), 
     cex = 2, col = "#e67e22", font = 2)

text(0.25, 0.4, sprintf("Average Age\n%.1f years", stats$avg_age), 
     cex = 1.8, col = "#9b59b6")
text(0.5, 0.4, sprintf("Vaccinated\n%d (%.1f%%)", stats$vaccinated, 
                       stats$vaccinated/stats$total_patients*100), 
     cex = 1.8, col = "#2ecc71")
text(0.75, 0.4, sprintf("Hospital Admissions\n%d (%.1f%%)", stats$admitted,
                        stats$admitted/stats$total_patients*100), 
     cex = 1.8, col = "#e67e22")

text(0.5, 0.1, format(Sys.Date(), "Generated: %B %d, %Y"), cex = 1.2, col = "gray50")
dev.off()
