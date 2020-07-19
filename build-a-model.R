library(tidymodels)
library(readr)
library(broom.mixed)

urchins <- read_csv("https://tidymodels.org/start/models/urchins.csv") %>%
  setNames(c("food_regime", "initial_volume", "width")) %>%
  mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))

ggplot(urchins, aes(initial_volume, width, col = food_regime)) +
  geom_point() +
  geom_smooth(method = "lm", se = F) +
  scale_color_viridis_d(option = "plasma", end = .7) +
  labs(x = "Initial Volume", y = "Width")

lm_mod <- linear_reg() %>%
  set_engine("lm")

lm_fit <-
  lm_mod %>%
  fit(width ~ initial_volume * food_regime, data = urchins)

tidy(lm_fit)

# ---- model prediction ----

new_points <- expand.grid(initial_volume = 20, food_regime = c("Initial", "Low", "High"))
mean_pred <- tibble("food_regime" = c("Initial", "Low", "High"), predict(lm_fit, new_points))
mean_pred

conf_int_pred <- predict(lm_fit, new_points, type = "conf_int")
conf_int_pred


