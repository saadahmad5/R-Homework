---
title: "Homework 4 - STATS 513"
author: "Saad Ahmad"
output:
  word_document: default
  html_notebook: default
---

# Question 1
# Consider the stackloss data, with stack.loss being the response and the other three variables being predictors. Fit models with the following 3 methods respectively and Compare the results. In each case, comment on the significance of predictors.

library(faraway)
library(quantreg)
library(MASS)

# 1. Ordinary least squares

linearModel1 = lm(stack.loss ~ ., data = stackloss)
summary(linearModel1)

##### Acid.Conc. is not significant whereas the other variables such as Air.Flow & Water.Temp are.

# 2. Least absolute deviations

linearModel2 = rq(stack.loss ~ ., data = stackloss)
summary(linearModel2)

##### All the co-efficients are now significant at 0.05 as all 95% confidence intervals doesn't contain 0. The coefficients of Water.Temp and Acid.Conc. are changed significantly.

# 3. Huber’s robust regression

linearModel3 = rlm(stack.loss ~ ., data = stackloss)
summary(linearModel3)

##### The coefficients besides the Acid.Conc. are significant.

# Use diagnostic methods to detect any outliers or influential points. Remove these points and then use least squares. Compare the results.

##### Determining the outliers or influential points for the least squares method. 

##### For outliers we will use the standard Bonferonni method as Professor Bo Wei mentioned in class. The p-value of the largest studentized residual is 0.00396 and the 0.05/17 quantile is 0.00294 which is not significant and we don't have any outliers.

?rstudent
temp = rstudent(linearModel1)
### Compute p-value
2*(1-pt(max(abs(temp)), df = 17))
### compare to alpha/n
0.05/17

##### For the influential points, we will use the Cook's distance as discussed by Professor Bo Wei in the class. Taking out the least squares. The value of the coefficient on Water.Temp is significantly less than we had previously in the least square model. The coefficients on Air.Flow and Water.Temp are still significant.

cookDistance = cooks.distance(linearModel1)
stackloss_model1 = stackloss[-21,]
linearModel1_modified = lm(stack.loss ~ ., data = stackloss_model1)
summary(linearModel1)

---

# Question 2
# Utilize the Box-Cox model to determine if there should be a transformation on the stack.loss variable in the stackloss data. If there should be one, read the estimate of λfrom the plot and fit a model with transformation

linearModel_BoxCox = boxcox(lm(stack.loss~.,data = stackloss), lambda = seq(-.4, 1, by = .05))
linearModel_powlaw = lm(stack.loss^.33~. , data = stackloss)
summary(linearModel_powlaw)

##### This transformed model leaves us with the coefficient for the predictor Acid.Conc. as the only predictor with non-significant p-value
