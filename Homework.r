---
author: "Saad Ahmad"
instructor: "Bo Wei"
course: "STATS 513"
---

# Question 1

library(faraway)
library(ggplot2)

data(sat)
?sat
head(sat)

model = lm(sat$total ~ sat$expend + sat$salary + sat$ratio + sat$takers)
model
summary(model)

ggplot(data = sat, 
        aes(x = model$fitted.values, y = model$residuals)) +
        geom_abline(slope = 0) + 
        geom_point(col = "red")


qqnorm(model$residuals, col = "brown")
qqline(model$residuals, col = "black")

# Takeaways:
# The plot of Fitted.Values vs Residuals shows scattered data points which implies it is homoscedasticity and it lacks heteroscedasticity. 
# The residuals are distributed normally.

# Question 2

data(longley)
?longley
head(longley)

model2 = lm(longley$Employed ~ longley$GNP.deflator + longley$GNP + longley$Unemployed 
                            + longley$Armed.Forces + longley$Population + longley$Year)
summary(model2)

# Compute and comment on the condition numbers.

kappa(longley[,-1])
##### The number is really high.

# Compute and comment on the correlations between the predictors

?cor
cor(longley)
##### There is a high correlation (p) between the variables

# Compute and comment on the variance inflation factors.

library(faraway)
model3 = lm(Employed ~ ., data = longley)
vif(model3)
##### On comparing VIF among other variables, Armed.Forces has 3.59.

# Choose a reduced set of predictors that does not exhibit as much
# collinearity as the full set, fit a new linear model with this reduced
# set, and comment on the differences between the reduced model and
# the full model.

model4 = lm(longley$Employed ~ longley$GNP.deflator + longley$GNP 
                             + longley$Population + longley$Year)
summary(model4)
##### These variables which I used are highly collinear. 
