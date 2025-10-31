library(tidyverse)
library(here)

df_input <- read_csv("output/status.csv", col_types = cols(
  patient_id = col_integer(),
  sex = col_character(),
  age = col_double(),
  vaccination_status = col_character(),
  has_covid_admission = col_logical()
))

library(ggplot2)
library(here)

age_hist_plot <- ggplot(df_input, aes(x = age, fill = sex)) +
  geom_histogram(binwidth = 5, color = "white", position = "identity", alpha = 0.7) +
  scale_fill_manual(values = c("male" = "steelblue", "female" = "pink", "unknown" = "grey", "intersex" = "green")) +
  labs(title = "Age Distribution of Patients", x = "Age", y = "Number of Patients", fill = "Sex") +
  theme_light()

ggsave(here("output", "age_distribution_1.png"), age_hist_plot, width = 6, height = 4)

sex_covid_plot <- ggplot(df_input, aes(x = sex, fill = has_covid_admission)) +
  geom_bar() +
  labs(
    title = "COVID Admission by Sex",
    x = "Sex",
    y = "Number of Patients",
    fill = "COVID Admission"
  ) +
  theme_dark()

# --- Save plot ---
ggsave(here("output", "sex_vs_covid_admission.png"), sex_covid_plot, width = 6, height = 4)

age_covid_plot <- ggplot(df_input, aes(x = age, fill = has_covid_admission)) +
    geom_histogram(binwidth = 3, color = "red", position = "identity") +
    labs(
      title = "Age Distribution by COVID Admission",
      x = "Age",
      y = "Number of Patients",
      fill = "COVID Admission"
    ) +
    theme_light()   

ggsave(here("output", "age_vs_covid_admission.png"), age_covid_plot, width = 6, height = 4)

vac_covid_plot <- ggplot(df_input, aes(x = vaccination_status, fill = has_covid_admission)) +
  geom_bar(position = "dodge") +
  labs(
    title = "COVID Admission by Vaccination Status",
    x = "Vaccination Status",
    y = "Number of Patients",
    fill = "COVID Admission"
  ) +
  theme_light()

# --- Save plot ---
ggsave(here("output", "vaccination_vs_covid_admission.png"), vac_covid_plot, width = 6, height = 4)

