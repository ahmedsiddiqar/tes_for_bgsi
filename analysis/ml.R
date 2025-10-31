library(tidyverse)
library(broom)

# --- Load dataset ---
df <- read_csv("output/dataset.csv", col_types = cols(
  patient_id = col_integer(),
  sex = col_character(),
  age = col_double(),
  has_covid_vaccine = col_character(),
  has_covid_admission = col_logical(),
  status =  col_character()
))

# --- Prepare outcome and predictors ---
data <- df %>%
  mutate(
    has_covid_vaccine = ifelse(has_covid_vaccine == "T", 1, 0),
    has_covid_admission = ifelse(has_covid_admission == "T", 1, 0),
    underlying_cause_death = ifelse(underlying_cause_death == "T", 1, 0),
    status_binary = ifelse(status == "dead", 1, 0),
    sex = as.factor(sex)
  )

# --- Fit logistic regression ---
model <- glm(status_binary ~ sex + age + has_covid_vaccine,
             data = data,
             family = binomial(link = "logit"))

# --- Add predicted probabilities ---
df <- df %>%
  mutate(
    predicted_prob = predict(model, type = "response")  # probability of outcome
  )

# --- Print first rows of new dataset ---
print(head(df))

# --- Plot predicted probabilities ---
plot <- ggplot(df, aes(x = age, y = predicted_prob, color = has_covid_vaccine)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") +
  facet_wrap(~sex) +
  labs(
    title = "Predicted Probability of COVID death",
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
