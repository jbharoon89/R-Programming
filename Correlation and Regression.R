### Correlation and Regression

# Multivariate Variables
# Scatterplot of weight vs. weeks

data(ncbirths)

ggplot(data = ncbirths, aes(x = weeks, y = weight)) +
  geom_point()

# The cut() function takes two arguments: 
# the continuous variable you want to discretize and 
# the number of breaks that you want to make in that continuous variable 
# in order to discretize it.

# Boxplot of weight vs. weeks
ggplot(data = ncbirths, aes(x = cut(weeks, breaks = 5), y = weight)) + 
  geom_boxplot()

library(openintro)

# Mammals scatterplot
ggplot(data = mammals, aes(x = BodyWt, y = BrainWt)) +
  geom_point()

# Baseball player scatterplot
ggplot(data = mlbBat10, aes(x = OBP, y = SLG)) +
  geom_point()

# Body dimensions scatterplot
ggplot(data = bdims, aes(x = hgt, y = wgt, color = factor(sex))) +
  geom_point()

# Smoking scatterplot
ggplot(data = smoking, aes(x = age, y = amtWeekdays)) +
  geom_point()

# Transformations
# Scatterplot with coord_trans()
ggplot(data = mammals, aes(x = BodyWt, y = BrainWt)) +
  geom_point() + 
  coord_trans(x = "log10", y = "log10")

# Scatterplot with scale_x_log10() and scale_y_log10()
ggplot(data = mammals, aes(x = BodyWt, y = BrainWt)) +
  geom_point() +
  scale_x_log10() + 
  scale_y_log10()

# Outliers
# Scatterplot of SLG vs. OBP
mlbBat10 %>% 
  filter(AB >= 200) %>%
  ggplot(aes(x = OBP, y = SLG)) +
  geom_point()

# Identify the outlying player
mlbBat10 %>%
  filter(AB >= 200, OBP < 0.2)

##### Correlation #####
# "pairwise.complete.obs" -> allows cor() to compute the correlation coefficient
# for those observations where the values of x and y are both not missing.

# Compute correlation
ncbirths %>%
  summarize(N = n(), r = cor(weight, mage))

# Compute correlation for all non-missing pairs
ncbirths %>%
  summarize(N = n(), r = cor(weight, weeks, use = "pairwise.complete.obs"))

# Compute properties of Anscombe
Anscombe %>%
  group_by(set) %>%
  summarize(N = n(), mean(x), sd(x), mean(y), sd(y), cor(x, y))

# Correlation for all baseball players
mlbBat10 %>%
  summarize(N = n(), r = cor(OBP, SLG))

# Correlation for all players with at least 200 ABs
mlbBat10 %>%
  filter(AB >= 200) %>%
  summarize(N = n(), r = cor(OBP, SLG))

# Correlation of body dimensions
bdims %>%
  group_by(sex) %>%
  summarize(N = n(), r = cor(hgt, wgt))

# Correlation among mammals, with and without log
mammals %>%
  summarize(N = n(), 
            r = cor(BodyWt, BrainWt), 
            r_log = cor(log(BodyWt), log(BrainWt)))

# Create faceted scatterplot
ggplot(data = noise, aes(x = x, y = y)) +
  geom_point() + 
  facet_wrap(~ z)

# Compute correlations for each dataset
noise_summary <- noise %>%
  group_by(z) %>%
  summarize(N = n(), spurious_cor = cor(x, y))

# Isolate sets with correlations above 0.2 in absolute strength
noise_summary %>%
  filter(abs(spurious_cor) > 0.2)

# abs() -> absolute value of spurious correlation.

# Scatterplot with regression line
ggplot(data = bdims, aes(x = hgt, y = wgt)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

# Estimate optimal value of my_slope
add_line(my_slope = 1)

####bdims_summary
#    N   r  mean_hgt   sd_hgt  mean_wgt   sd_wgt
# 1 507 0.7173011 171.1438 9.407205 69.14753 13.34576

# Print bdims_summary
bdims_summary

# Add slope and intercept
bdims_summary %>%
  mutate(slope = r * sd_wgt / sd_hgt, 
         intercept = mean_wgt - slope * mean_hgt)

# Height of children vs. height of father
ggplot(data = Galton_men, aes(x = father, y = height)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_smooth(method = "lm", se = FALSE)

# Height of children vs. height of mother
ggplot(data = Galton_women, aes(x = mother, y = height)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_smooth(method = "lm", se = FALSE)

# Because the slope of the regression line is smaller than 1 
# (the slope of the diagonal line) for both males and females, 
# we can verify Sir Francis Galton's regression to the mean concept!

### Interpretation of REgression Modelling
# Linear model for weight as a function of height
lm(wgt ~ hgt, data = bdims)

# Linear model for SLG as a function of OBP
lm(SLG ~ OBP, data = mlbBat10)

# Log-linear model for body weight as a function of brain weight
lm(log(BodyWt) ~ log(BrainWt), data = mammals)

mod <- lm(wgt ~ hgt, data = bdims)
# Show the coefficients
coef(mod)

# Show the full output
summary(mod)

# Mean of weights equal to mean of fitted values?
mean(mod) == mean(fitted.values(mod))

# Mean of the residuals
mean(residuals(mod))

# Load broom
library(broom)

# Create bdims_tidy
bdims_tidy <- augment(mod)

# Glimpse the resulting data frame
glimpse(bdims_tidy)

# Add the line to the scatterplot
ggplot(data = bdims, aes(x = hgt, y = wgt)) + 
  geom_point() + 
  geom_abline(data = coefs, 
              aes(intercept = `(Intercept)`, slope = hgt),  
              color = "dodgerblue")

# View summary of model
summary(mod)

# Compute the mean of the residuals
mean(residuals(mod))

# Compute RMSE
sqrt(sum(residuals(mod)^2) / df.residual(mod))

# View model summary
summary(mod)

# Compute R-squared
bdims_tidy %>%
  summarize(var_y = var(wgt), var_e = var(.resid)) %>%
  mutate(R_squared = 1 - var_e / var_y)

# Compute SSE for null model
mod_null %>%
  summarize(SSE = var(.resid))

# Compute SSE for regression model
mod_hgt %>%
  summarize(SSE = var(.resid))

# The leverage of an observation in a regression model is defined entirely in 
# terms of the distance of that observation from the mean of the explanatory variable.
# That is, observations close to the mean of the explanatory variable have low leverage,
# while observations far from the mean of the explanatory variable have high leverage.
# Points of high leverage may or may not be influential.

# Rank points of high leverage
mod %>%
  augment() %>%
  arrange(desc(.hat)) %>%
  head()

# Rank influential points
mod %>%
  augment() %>%
  arrange(desc(.cooksd)) %>%
  head()

# Create nontrivial_players
nontrivial_players <- mlbBat10 %>%
  filter(AB >= 10, OBP < 0.5)

# Fit model to new data
mod_cleaner <- lm(SLG ~ OBP, data = nontrivial_players)

# View model summary
summary(mod_cleaner)

# Visualize new model
ggplot(data = nontrivial_players, aes(x = OBP, y = SLG)) +
  geom_point() + 
  geom_smooth(method = "lm")

# Rank high leverage points
mod %>%
  augment() %>%
  arrange(desc(.hat), .cooksd) %>%
  head()
