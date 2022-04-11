---
title: "Homework - STATS 513"
author: "Saad Ahmad"
output:
  word_document: default
  html_notebook: default
---


### Ankylosing spondylitis is a chronic form of arthritis. A study was conducted to determine whether daily stretching of the hip tissues would improve mobility. The data are found in hips. The flexion angle of the hip before the study (fbef) is a predictor and the flexion angle after the study (faft) is the response. (The variables rbef and raft won’t be used.)

library(faraway)
?hips
data(hips)
summary(hips)

#### (1). Plot the data using different plotting symbols for the treatment and the control status.

plot(fbef ~ grp, hips)
plot(faft~fbef, pch=as.character(grp), hips)

#### (2). Fit a model to determine whether there is a treatment effect. (Hints: Please use model selection to select the model. For example, you may need to determine whether the interaction term should be included)

##### Without interation term
linearModel1 = lm(faft ~ fbef + grp, data = hips)
summary(linearModel1)

##### With interation term
linearModel2 = lm(faft ~ fbef + grp + fbef*grp, data = hips)
summary(linearModel2)

#### (3). Compute the difference between the flexion before and after and test whether this difference varies between treatment and control. Contrast this approach to your previous model. (use var.equal = T in the test)

difference = (hips$faft - hips$fbef)
t.test(difference[grp=="treat"], difference[grp=="control"])

##### Here the p-value = 0.06193 being not-significant hence there is no treatment effect because of the angle difference.

#### (4). What is the estimated size of the treatment effect? Give a 95% confidence interval.

linearModel3 = lm(faft ~ fbef + grp, data = hips)
confint(linearModel3)

##### The 95% C.I. is (3.497, 10.553).

#### (5). Notice that both legs of each subject have been included in the study as separate observations. Explain what difficulties this causes with the model assumptions.

##### It could cause some correlated errors as we are using both legs of the same person as a separate observations.

#### (6). Compute the average angles for each subject and repeat the modeling with this reduced data set (hips new). Point out differences in the conclusions if any.

hips_new<-aggregate(list(fbef=hips$fbef, faft=hips$faft),by=list(person=hips$person), mean)
hips_new$grp=hips$grp[match(hips_new$person,hips$person)]
plot(fbef ~ grp, hips_new)
plot(faft~fbef, pch=as.character(grp), hips_new)

