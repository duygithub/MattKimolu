

# List of required packages
required_packages <- c(
  "ggplot2", "readr", "dplyr", "tidyr", "tidyverse", 
  "rmarkdown", "here", "emmeans", "readxl", "data.table"
)

# Function to check and install missing packages
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Call the function to install and load the necessary packages
install_if_missing(required_packages)


#######################################################################


###############################################################


dat<- read.csv(here("simulated_raw_data1.csv"))


dat$data_entry_type <- factor(dat$data_entry_type,
                              levels = c("25", "50L2", "50L1", "100IM"))



model2 <- lm(elapsed_time ~ stroke_count + data_entry_type + I(stroke_count^2) + stroke_count * data_entry_type , data =dat )
summary(model2)

##############################################


## Predictive plot:

# grid of predictors to plot over
newdat <- expand.grid(
  stroke_count = seq(min(dat$stroke_count, na.rm = TRUE),
                     max(dat$stroke_count, na.rm = TRUE), length.out = 200),
  data_entry_type = unique(dat$data_entry_type)
)

# predictions + 95% CI
p <- predict(model2, newdata = newdat, se.fit = TRUE)
newdat <- newdat %>%
  mutate(fit = p$fit, se = p$se.fit,
         lwr = fit - 1.96*se, upr = fit + 1.96*se)

# plot: points + fitted curve + CI ribbon
ggplot(dat, aes(stroke_count, elapsed_time, color = data_entry_type)) +
  geom_point(alpha = 0.35, size = 1) +
  geom_ribbon(data = newdat,
              aes(y = fit, ymin = lwr, ymax = upr, fill = data_entry_type, color = NULL),
              alpha = 0.15) +
  geom_line(data = newdat, aes(y = fit), linewidth = 1) +
  labs(x = "Stroke count", y = "Elapsed time",
       title = "Fitted quadratic-with-interaction by data_entry_type") +
  theme_minimal()

##########################################################
## Residual plot:


par(mfrow = c(2,2))
plot(model2, which = 1)  # Residuals vs Fitted (linearity/variance check)
plot(model2, which = 2)  # Normal Q-Q (normality check)
plot(model2, which = 3)  # Scale-Location (homoscedasticity)
plot(model2, which = 4)  # Residuals vs Leverage (influence)



