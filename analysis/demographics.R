# =========================================
# Patient Demographics Analysis with ggplot
# =========================================

# --- Load libraries ---
library(tidyverse)
library(here)

# --- Create output directory if not exists ---
if (!dir.exists(here("output"))) dir.create(here("output"))

# --- Load dataset ---
df_input <- read_csv(
  here::here("output", "status.csv"),
  col_types = cols(
    patient_id = col_integer(),
    sex = col_character(),
    age = col_double(),
    vaccination_status = col_character(),
    latest_covid_vaccine_date = col_date(format = "")
  )
)

# =========================================
# 1. Sex distribution
# =========================================
sex_plot <- ggplot(df_input, aes(x = sex)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Patients by Sex", x = "Sex", y = "Count") +
  theme_minimal()
ggsave(here("output", "sex_distribution.png"), sex_plot, width = 6, height = 4)

# =========================================
# 2. Vaccination status distribution
# =========================================
vacc_plot <- ggplot(df_input, aes(x = vaccination_status)) +
  geom_bar(fill = "salmon") +
  labs(title = "Distribution of Vaccination Status", x = "Vaccination Status", y = "Count") +
  theme_minimal()
ggsave(here("output", "vaccination_status_distribution.png"), vacc_plot, width = 6, height = 4)

# =========================================
# 3. Age distribution overall
# =========================================
age_hist_plot <- ggplot(df_input, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(title = "Age Distribution of Patients", x = "Age", y = "Number of Patients") +
  theme_minimal()
ggsave(here("output", "age_distribution.png"), age_hist_plot, width = 6, height = 4)

# =========================================
# 4. Age by vaccination status
# =========================================
age_vacc_plot <- ggplot(df_input, aes(x = vaccination_status, y = age, fill = vaccination_status)) +
  geom_boxplot() +
  labs(title = "Age Distribution by Vaccination Status", x = "Vaccination Status", y = "Age") +
  theme_minimal() +
  theme(legend.position = "none")
ggsave(here("output", "age_by_vaccination.png"), age_vacc_plot, width = 6, height = 4)

# =========================================
# 5. Vaccination dates distribution
# =========================================
vacc_dates_plot <- ggplot(df_input, aes(x = latest_covid_vaccine_date)) +
  geom_histogram(binwidth = 30, fill = "darkgreen", color = "white") +
  labs(title = "Distribution of Latest COVID Vaccine Dates", x = "Date", y = "Number of Patients") +
  theme_minimal()
ggsave(here("output", "vaccination_dates_distribution.png"), vacc_dates_plot, width = 6, height = 4)

# =========================================
# 6. Sex vs Vaccination status
# =========================================
sex_vacc_plot <- ggplot(df_input, aes(x = sex, fill = vaccination_status)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of Vaccination Status by Sex", x = "Sex", y = "Proportion", fill = "Vaccination Status") +
  theme_minimal()
ggsave(here("output", "sex_vs_vaccination.png"), sex_vacc_plot, width = 6, height = 4)

# =========================================
# 7. Age groups vs Vaccination status
# =========================================
df_input <- df_input %>%
  mutate(age_group = cut(age, breaks = c(0, 18, 30, 45, 60, Inf),
                         labels = c("<18", "18-30", "31-45", "46-60", "60+")))

age_group_plot <- ggplot(df_input, aes(x = age_group, fill = vaccination_status)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Vaccination Status by Age Group", x = "Age Group", y = "Proportion", fill = "Vaccination Status") +
  theme_minimal()
ggsave(here("output", "age_group_vs_vaccination.png"), age_group_plot, width = 6, height = 4)

# =========================================
# Script complete
# =========================================
message("All ggplot analyses complete! Plots saved in the 'output/' folder.")
