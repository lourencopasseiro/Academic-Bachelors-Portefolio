library(fpp3)
library(fma)
library(tidyverse)
library(urca)
library(ggplot2)
library(readr)
library(lmtest)
library(car)
library(sandwich)
hapiness <- read_csv("2o ano 2oSEM/econ_dataset.csv")

attach(hapiness)


# Let's check if we have a high correlation between the variables


cor(`Log GDP Per Capita`, `Life Ladder`) # high
cor(`Log GDP Per Capita`, `Social Support`) # high
cor(`Log GDP Per Capita`, `Healthy Life Expectancy At Birth`) #high
cor(`Log GDP Per Capita`, `Freedom To Make Life Choices`)
cor(`Log GDP Per Capita`, Generosity)
cor(`Log GDP Per Capita`, `Perceptions Of Corruption`)
cor(`Log GDP Per Capita`, `Positive Affect`)
cor(`Log GDP Per Capita`, `Negative Affect`)
cor(`Log GDP Per Capita`, `Confidence In National Government`)
# GDP per Capita has a high correlation with 'social support' and 'Healthy Life Expectancy At Birth'
# and with the dependent variable but does not have a high correlation with the other 
# independent variables

cor(`Social Support`, `Life Ladder`) # high
cor(`Social Support`, `Healthy Life Expectancy At Birth`) #high
cor(`Social Support`, `Freedom To Make Life Choices`)
cor(`Social Support`, Generosity)
cor(`Social Support`, `Perceptions Of Corruption`)
cor(`Social Support`, `Positive Affect`)
cor(`Social Support`, `Negative Affect`)
cor(`Social Support`, `Confidence In National Government`)
# Social Support has a high correlation with Healthy Life Expectancy At Birth and
# with the dependent variable but does not have a high correlation with the other 
# independent variables

cor(`Healthy Life Expectancy At Birth`, `Life Ladder`) # high
cor(`Healthy Life Expectancy At Birth`, `Freedom To Make Life Choices`)
cor(`Healthy Life Expectancy At Birth`, Generosity)
cor(`Healthy Life Expectancy At Birth`, `Perceptions Of Corruption`)
cor(`Healthy Life Expectancy At Birth`, `Positive Affect`)
cor(`Healthy Life Expectancy At Birth`, `Negative Affect`)
cor(`Healthy Life Expectancy At Birth`, `Confidence In National Government`)
# Healthy Life Expectancy At Birth has a high correlation with the dependent variable
# but does not have a high correlation with the other independent variables 

cor(`Freedom To Make Life Choices`, `Life Ladder`)
cor(`Freedom To Make Life Choices`, Generosity)
cor(`Freedom To Make Life Choices`, `Perceptions Of Corruption`)
cor(`Freedom To Make Life Choices`, `Positive Affect`)
cor(`Freedom To Make Life Choices`, `Negative Affect`)
cor(`Freedom To Make Life Choices`, `Confidence In National Government`)
# Freedom To Make Life Choices does not have a high correlation with the variables

cor(Generosity, `Life Ladder`)
cor(Generosity, `Perceptions Of Corruption`)
cor(Generosity, `Positive Affect`)
cor(Generosity, `Negative Affect`)
cor(Generosity, `Confidence In National Government`)
# Generosity does not have a high correlation with the variables

cor(`Perceptions Of Corruption`, `Life Ladder`)
cor(`Perceptions Of Corruption`, `Positive Affect`)
cor(`Perceptions Of Corruption`, `Negative Affect`)
cor(`Perceptions Of Corruption`, `Confidence In National Government`)
# Perceptions Of Corruption does not have a high correlation with the variables

cor(`Positive Affect`, `Life Ladder`)
cor(`Positive Affect`, `Negative Affect`)
cor(`Positive Affect`, `Confidence In National Government`)
# Positive Affects does not have high correlation with the variables 

cor(`Negative Affect`, `Life Ladder`)
cor(`Negative Affect`, `Confidence In National Government`)
# Negative Affects does not have high correlation with the variables

cor(`Confidence In National Government`, `Life Ladder`)
# Confidence In National Government does not have high correlation with the dependent variables


# Let's fit the model

hap_model <- lm(`Life Ladder` ~ `Healthy Life Expectancy At Birth` +
                  `Freedom To Make Life Choices` + 
                  Generosity + 
                  `Perceptions Of Corruption` + 
                  `Positive Affect` +
                  `Negative Affect` +
                  `Confidence In National Government`, data = hapiness)
vif(hap_model)    
# Therefore, we can conclude that there is no evidence of perfect collinearity
# among the predictor variables, since any of the values are above 10, 
# ranging between 1.314212-2.639660

u.hat <- resid(hap_model) 
mean(u.hat) 
# The extremely small mean of the residuals (-6.288373e-18) provides evidence that 
# the zero conditional mean assumption is likely satisfied. Therefore, 
# regression model's estimators are unbiased.

summary(hap_model)
bptest(hap_model) 


# BP test
summary(lm(u.hat^2 ~ `Healthy Life Expectancy At Birth` +
             `Freedom To Make Life Choices` + 
             Generosity + 
             `Perceptions Of Corruption` + 
             `Positive Affect` +
             `Negative Affect` +
             `Confidence In National Government`))
bptest(hap_model, ~ `Healthy Life Expectancy At Birth` +
         `Freedom To Make Life Choices` + 
         Generosity + 
         `Perceptions Of Corruption` + 
         `Positive Affect` +
         `Negative Affect` +
         `Confidence In National Government`)
qchisq(0.95, 6)
# This results suggest that given the p-value of 0.0318, 
# which is less than the conventional threshold of 0.05, 
# and the BP statistic being greater than the chi-squared critical value, 
# there is significant evidence to reject the null hypothesis. 
# This indicates that there is heteroscedasticity in the regression model.

# White test

summary(lm(resid(hap_model) ~ `Healthy Life Expectancy At Birth` +
             `Freedom To Make Life Choices` + 
             Generosity + 
             `Perceptions Of Corruption` + 
             `Positive Affect` +
             `Negative Affect` +
             `Confidence In National Government` + 
             I(`Healthy Life Expectancy At Birth`^2) +
             I(`Freedom To Make Life Choices`^2) + 
             I(Generosity^2) + 
             I(`Perceptions Of Corruption`^2) + 
             I(`Positive Affect`^2) +
             I(`Negative Affect`^2) +
             I(`Confidence In National Government`^2) +
             I(`Healthy Life Expectancy At Birth`*`Freedom To Make Life Choices`) +
             I(`Healthy Life Expectancy At Birth`*Generosity) +
             I(`Healthy Life Expectancy At Birth`*`Perceptions Of Corruption`) +
             I(`Healthy Life Expectancy At Birth`*`Positive Affect`) + 
             I(`Healthy Life Expectancy At Birth`*`Negative Affect`) +
             I(`Healthy Life Expectancy At Birth`*`Confidence In National Government`) +
             I(`Freedom To Make Life Choices`*Generosity) +
             I(`Freedom To Make Life Choices`*`Perceptions Of Corruption`) +
             I(`Freedom To Make Life Choices`*`Positive Affect`) + 
             I(`Freedom To Make Life Choices`*`Negative Affect`) +
             I(`Freedom To Make Life Choices`*`Confidence In National Government`) +
             I(Generosity*`Perceptions Of Corruption`) +
             I(Generosity*`Positive Affect`) + 
             I(Generosity*`Negative Affect`) +
             I(Generosity*`Confidence In National Government`) +
             I(`Perceptions Of Corruption`*`Positive Affect`) + 
             I(`Perceptions Of Corruption`*`Negative Affect`) +
             I(`Perceptions Of Corruption`*`Confidence In National Government`) +
             I(`Positive Affect`*`Negative Affect`) +
             I(`Positive Affect`*`Confidence In National Government`) +
             I(`Negative Affect`*`Confidence In National Government`)))

bptest(hap_model, ~ `Healthy Life Expectancy At Birth` +
             `Freedom To Make Life Choices` + 
             Generosity + 
             `Perceptions Of Corruption` + 
             `Positive Affect` +
             `Negative Affect` +
             `Confidence In National Government` + 
             I(`Healthy Life Expectancy At Birth`^2) +
             I(`Freedom To Make Life Choices`^2) + 
             I(Generosity^2) + 
             I(`Perceptions Of Corruption`^2) + 
             I(`Positive Affect`^2) +
             I(`Negative Affect`^2) +
             I(`Confidence In National Government`^2) +
             I(`Healthy Life Expectancy At Birth`*`Freedom To Make Life Choices`) +
             I(`Healthy Life Expectancy At Birth`*Generosity) +
             I(`Healthy Life Expectancy At Birth`*`Perceptions Of Corruption`) +
             I(`Healthy Life Expectancy At Birth`*`Positive Affect`) + 
             I(`Healthy Life Expectancy At Birth`*`Negative Affect`) +
             I(`Healthy Life Expectancy At Birth`*`Confidence In National Government`) +
             I(`Freedom To Make Life Choices`*Generosity) +
             I(`Freedom To Make Life Choices`*`Perceptions Of Corruption`) +
             I(`Freedom To Make Life Choices`*`Positive Affect`) + 
             I(`Freedom To Make Life Choices`*`Negative Affect`) +
             I(`Freedom To Make Life Choices`*`Confidence In National Government`) +
             I(Generosity*`Perceptions Of Corruption`) +
             I(Generosity*`Positive Affect`) + 
             I(Generosity*`Negative Affect`) +
             I(Generosity*`Confidence In National Government`) +
             I(`Perceptions Of Corruption`*`Positive Affect`) + 
             I(`Perceptions Of Corruption`*`Negative Affect`) +
             I(`Perceptions Of Corruption`*`Confidence In National Government`) +
             I(`Positive Affect`*`Negative Affect`) +
             I(`Positive Affect`*`Confidence In National Government`) +
             I(`Negative Affect`*`Confidence In National Government`))
qchisq(0.95, 35)
# This results suggest that given the p-value of 0.0001864, 
# which is less than the conventional threshold of 0.05, 
# and the BP statistic being greater than the chi-squared critical value, 
# there is significant evidence to reject the null hypothesis. 
# This indicates that there is heteroscedasticity in the regression model.

# White Simplified

summary(lm(resid(hap_model)^2 ~ fitted(hap_model3) + I(fitted(hap_model3)^2)))
bptest(hap_model3, ~ fitted(hap_model3) + I(fitted(hap_model3)^2) )
qchisq(0.95, 2)
# Both the regression of the squared residuals and the white simplified test suggest 
# that there is no significant evidence of heteroscedasticity in our model.
# The variance of the residuals does not appear to depend on the fitted values, 
# indicating that the assumption of homoscedasticity is reasonably met.


# As the bp and white tests reached the conclusion of a significant statistical evidence
# to reject the null hypothesis, indicating the presence of heteroskedasticity, we will 
# resort to the Robust estimation

# Robust estimation

hccm1 <- vcovHC(hap_model, type = 'HC0')
robust<-coeftest(hap_model, vcov=hccm1)
robust

# Final Equation with Robust Estimation

# LifeLadder = −0.674 + 0.079HealthyLifeExpectancyAtBirth + 1.874FreedomToMakeLifeChoices
#              + 0.366Generosity − 1.127PerceptionsOfCorruption + 2.543PositiveAffect 
#              − 1.413NegativeAffect − 1.521ConfidenceInNationalGovernment


# RESET test


RESET_hap <- lm(`Life Ladder` ~ `Healthy Life Expectancy At Birth` +
                  `Freedom To Make Life Choices` + 
                  Generosity + 
                  `Perceptions Of Corruption` + 
                  `Positive Affect` +
                  `Negative Affect` +
                  `Confidence In National Government` +
                  I(fitted(hap_model)^2) +
                  I(fitted(hap_model)^3), data = hapiness)
summary(RESET_hap)
linearHypothesis(RESET_hap, matchCoefs(RESET_hap,"fitted"))


# Let's check what functional form should the equation be:


reg_test = lm(log(`Life Ladder`) ~ `Healthy Life Expectancy At Birth`)
fitt = fitted(reg_test)

# Plot the data with points
ggplot(hapiness, aes(x = `Healthy Life Expectancy At Birth`, y = `Life Ladder`)) +
  geom_point() +
  theme_minimal()

# Plot the data with points and linear model smooth line
ggplot(hapiness, aes(x = `Healthy Life Expectancy At Birth`, y = `Life Ladder`)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  theme_minimal()

# Plot the log-transformed response with linear model smooth line
ggplot(hapiness, aes(x = `Healthy Life Expectancy At Birth`, y = log(`Life Ladder`))) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  theme_minimal()

# Plot the data with points and fitted values line
ggplot(hapiness, aes(x = `Healthy Life Expectancy At Birth`, y = `Life Ladder`)) +
  geom_point() +
  geom_line(aes(x = `Healthy Life Expectancy At Birth`, y = exp(fitt) ), color = 'red', lwd = 0.7) +
  theme_minimal()
# With results graph we are able to recognize that we should have a log-level relation between y and x


RESET_hap2 <- lm(log(`Life Ladder`) ~ `Healthy Life Expectancy At Birth` +
                  `Freedom To Make Life Choices` + 
                  Generosity + 
                  `Perceptions Of Corruption` + 
                  `Positive Affect` +
                  `Negative Affect` +
                  `Confidence In National Government` +
                  I(fitted(hap_model)^2) +
                  I(fitted(hap_model)^3), data = hapiness)
summary(RESET_hap2) 
linearHypothesis(RESET_hap2, matchCoefs(RESET_hap,"fitted")) 

# The p-value (0.4197) is greater than 0.05, indicating that we do not reject 
# the null hypothesis. In other words, there is not enough evidence to conclude that
# the quadratic and cubic terms of the fitted values are significantly different
# from zero.  This suggests that there is no evidence of misspecification 
# in the original model. The quadratic and cubic terms of the fitted values 
# do not add significant information to the model.

# Final Model:

final_reg <- lm(log(`Life Ladder`) ~ `Healthy Life Expectancy At Birth` +
                  `Freedom To Make Life Choices` + 
                  Generosity + 
                  `Perceptions Of Corruption` + 
                  `Positive Affect` +
                  `Negative Affect` +
                  `Confidence In National Government`, data = hapiness)
summary(final_reg)


