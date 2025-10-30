library(tidyverse)
library(here)

# --- Load input dataset ---
df_input <- read_csv(
  here::here("output", "dataset.csv.gz"),
  col_types = cols(
    patient_id = col_integer(),
    sex = col_character(),
    age = col_double(),
    latest_covid_vaccine_date = col_date(format = ""),
    has_covid_admission = col_character()
  )
)

# --- Add vaccination status column ---
df_input <- df_input %>%
  mutate(
    vaccination_status = if_else(
      is.na(latest_covid_vaccine_date),
      "Not Vaccinated",
      "Vaccinated"
    )
  )

# Print for the log (optional)
print(head(df_input))
print(table(df_input$vaccination_status))

# --- Save only the full dataset ---
write_csv(df_input, here::here("output", "status.csv"))
