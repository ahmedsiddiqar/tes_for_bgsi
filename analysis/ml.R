library(tidyverse)
library(broom)

# --- Load dataset ---
df <- read_csv("output/status.csv", col_types = cols(
  patient_id = col_integer(),
  sex = col_character(),
  age = col_double(),
  vaccination_status = col_character(),
  has_covid_admission = col_logical()
))

# --- Prepare outcome and predictors ---
df <- df %>%
  mutate(
    outcome = as.integer(has_covid_admission),
    sex = factor(sex),
    vaccination_status = factor(vaccination_status, 
                                levels = c("Not Vaccinated", "Vaccinated"))
  ) %>%
  drop_na(outcome, age, sex, vaccination_status)

# --- Fit logistic regression ---
model <- glm(
  outcome ~ age + sex + vaccination_status,
  family = binomial(link = "logit"),
  data = df
)

# --- Add predicted probabilities ---
df <- df %>%
  mutate(
    predicted_prob = predict(model, type = "response")  # probability of outcome
  )

# --- Print first rows of new dataset ---
print(head(df))

# --- Plot predicted probabilities ---
plot <- ggplot(df, aes(x = age, y = predicted_prob, color = vaccination_status)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  facet_wrap(~sex) +
  labs(
    title = "Predicted Probability of COVID Infection",
    x = "Age",
    y = "Predicted Probability",
    color = "Vaccination Status"
  ) +
  theme_dark()


# --- Save the plot ---
ggsave(
  filename = "output/predicted_prob_plot.png",  # path and file name
  plot = plot,                                  # the plot object
  width = 8, height = 6,                        # size in inches
  dpi = 600                                     # resolution
)
