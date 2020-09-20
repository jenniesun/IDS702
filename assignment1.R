---
  title: "Assignment1"
author: "Jennie Sun"
date: "8/28/2020"
output: pdf_document
---
  
  library(kableExtra)
library(knitr)
library(tinytex)
library(tidyr)
library(broom)
library(dplyr)
library(gridExtra)
library(GGally)


## Question 1 RESPIRATORY RATES FOR CHILDREN

## Do exploratory analysis on the data and include a useful plot that a physician could use to assess a “normal” range of respiratory rates for children of any age between 0 and 3.
resp <- read.csv("/Users/Jennie/Downloads/IDS-702-data-modeling/assignments/assignment1/Respiratory.csv")
ggpairs(resp[,-1],mapping=ggplot2::aes(colour = "red4",alpha=0.6))
#A “normal” range of respiratory rates for children of any age between 0 and 3 is from 0 to 80.

## Write down a regression model for predicting respiratory rates from age. Make sure to use the right mathematical notation.
#Rate = -0.69571Age+47.05216

## Fit the model to the data and interpret your results.
model <- lm(Rate ~ Age, data=resp)
#The predictor - age is significant at 0.001 level. The average respiratory rate is expected to decrease by 0.69571 unit for each additional unit increment in age on average. According to the adjusted R-squared, about 47.58% of the variation is explained by the model.

## Include a table showing the output from the regression model including the estimated intercept, slope, residual standard error, and proportion of variation explained by the model.
model <- lm(Rate ~ Age, data=resp)
summary=tidy(model)
kable(summary, format='markdown',booktabs=T, caption='summary output', digits = 320)

## Is there enough evidence that the model assumptions are reasonable for this data? You should consider transformations (think log transformations, etc) if you think there’s a violation of normality and/or linearity.
#Check linearity:
ggplot(resp,aes(x=Age, y=model$residual)) +
  geom_point(alpha = .7) +  geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs Age",x="Age",y="Residuals")
#The plot of residual against Age shows that there are slightly more points stacked around the zero mark, but in general most points are distributed randomly and there doesn't seem to be a clear pattern, so the linearity assumption holds. 

#Check the independence and equal variance:
plot(model,which=1,col=c("blue4"))
#According to the residual against fitted values plot, there are slightly more points stacked towards the end, but in general most of the points seem to be distributed randomly, and there doesn't seem to be a clear pattern. So the independence and equal variance assumptions hold. 

#Check normality:
plot(model,which=2,col=c("blue4"))

#The Normal Q-Q plot above shows that most of the points on the graph seem to fall on the 45 degree angle line, with some deviations at both ends, which could be caused by outliers in the dataset that need further investigations. Therefore, the normality assumption holds. 
#Though the adjusted-R Square value is not perfect being 0.4758, suggesting only around half of response can be explained by the regression model, the linear model is at best a rough approximation to the data. There might be other factors involved, and the residual errors due to other unmeasured factors need to be further investigated. In conclusion, the main assumptions are plausible in this model.

## Demonstrate the usefulness of the model by providing 95% prediction intervals for the rate for three individual children: a 1 month old, an 18 months old, and a 29 months old.
ages = c(1, 18, 29)
ages = as.data.frame(ages)
colnames(ages) <- c('Age')
pred <- predict(model, newdata=ages, interval = 'prediction')
pred = as.data.frame(pred)
pred = pred %>%
  mutate(exp_fit=exp(fit),
         exp_upr=exp(upr),
         exp_lwr=exp(lwr))

pred = cbind(ages, pred)
kable(pred, format='markdown',booktabs=T, caption='95% Prediction Intervals')

# 95% prediction intervals for the rate for three individual children:
#   1 month old: (30.92683, 61.78607)
# 18 months old: (19.11397 49.94468)
# 29 months old: (11.43713, 42.31582)


## Question 2 THE DRAMATIC U.S. PRESIDENTIAL ELECTION OF 2000

## Make a scatterplot of the variables Buchanan2000 and Bush2000. What evidence is there in the scatterplot that Buchanan received more votes than expected in Palm Beach County?
elec <- read.csv("/Users/Jennie/Downloads/IDS-702-data-modeling/assignments/assignment1/Elections.csv")
plot(elec$Buchanan2000 ~ elec$Bush2000, main="Votes Scatterplot",
     xlab="Buchanan2000", ylab="Bush2000 ", pch=18)
abline(lm(elec$Buchanan2000 ~ elec$Bush2000))

#Cook's Distance:
elec_model <- lm(elec$Buchanan2000 ~ elec$Bush2000, data=elec)
plot(elec_model,which=5,col=c("blue4"))
#According to the Redisuals vs Leverage plot, two points - 67 and 13 - are beyond the cook's distance 1 mark, with point 67 being far beyond the 1 mark). Referring to the original dataset, point 67 represents the the votes Buchanan received in Palm Beach County, which is extremely higher than those from other counties. This serves as the evidence that Buchanan received more votes than expected in this county. 


## Fit a linear regression model to the data to predict Buchanan votes from Bush votes, without using Palm Beach County results. You should consider transformations for both variables if you think there’s a violation of normality and/or linearity.

subset <- subset(elec, County != 'Palm Beach')
log_elec_model <- lm(log(Buchanan2000) ~ log(Bush2000), data=subset)

#Check linearity for Bush2000: 
ggplot(subset,aes(x=Bush2000, y=log_elec_model$residual)) +
  geom_point(alpha = .7) +  geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs Bush2000",x="Bush2000",y="Residuals")

#Check linearity for Buchanan2000: 
ggplot(subset,aes(x=Buchanan2000, y=log_elec_model$residual)) +
  geom_point(alpha = .7) +  geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs Buchanan2000",x="Buchanan2000",y="Residuals")

#The linearity assumption seem to be valid for both variables since the points are pretty much randomly distributed. However, most points are distributed around the lower end (near the 0 mark), so it there is potentially a relationship that is not being captured by the model.

#Check independence and equal variance:
plot(log_elec_model,which=1,col=c("blue4"))
#Most points are generally randomly distributed, so the independence assumption hold. The number of points above and below the 0 residuals don't vary much, and the red line is pretty flat, generally speaking. Therefore, the equal variance assumption holds.

#Check normality:
plot(log_elec_model,which=2,col=c("blue4"))
#Most points on the graph seem to fall on the 45 degree angle line, with some deviations at both ends, which could be caused by outliers in the dataset that need further investigations. Therefore, the normality assumption holds, and we conclude that the main assumptions are plausible in this model. 

## Include the output from the final regression model that you used, as well as evidence that the model fits the assumptions reasonably well.
summary1=tidy(log_elec_model)
kable(summary1, format='markdown',booktabs=T, caption='Regression Model Summary', digits=320)
#I decided to transform the linear model by logging both Buchanan2000 & Bush2000 because if not logging the two variables, the x (Buchanan2000 and Bush2000) against residuals plots would be less randomly distribted, which could potentially violate the linearity assumption. It would also violate the independence assumption because the points on the Residuals vs Fitted plot would not have a randomly distributed pattern. Lastly, the points on the Normal-QQ plot fit closer to th 45 degree angle line in the logged model. Therefore, the logged model fits the assumptions reasonably well. 


## Obtain a 95% prediction interval for the number of Buchanan votes in Palm Beach from this result, assuming the relationship is the same in this county as in the others. If it is assumed that Buchanan’s actual count contains a number of votes intended for Gore, what can be said about the likely size of this number from the prediction interval?
palm_bush <- data.frame(Bush2000=c(152846))
pred2 <- exp(predict(log_elec_model, newdata=palm_bush, interval="prediction"))
kable(pred2, format='markdown', caption='95% Prediction Interval')
#According to the 95% prediction interval for the number of Buchanan votes in Palm Beach, the likely size of the votes is from 250.8001 to 1399.164, with a predicted true value of 592.3769. So in the best case for Buchanan, he would still be 1399.164, which would still be around 2000 short of votes he actually got. 


## Question 3 AIRBNB LISTINGS FOR SEATTLE, WA
## Analyze the data using host_is_superhost, host_identity_verified, room_type, accommodates, bathrooms and bedrooms as predictors. You should start by doing EDA, then model fitting, and model assessment. You should consider transformations if needed.
listing <- read.table("/Users/Jennie/Downloads/IDS-702-data-modeling/assignments/assignment1/Listings_QueenAnne.txt", header=TRUE)
listing_model <- lm(log(price) ~ host_is_superhost + host_identity_verified + 
                      room_type + log(accommodates) + log(bathrooms) + bedrooms, data=listing)

#EDA: 
ggpairs(listing[,-1],mapping=ggplot2::aes(colour = "red4",alpha=0.6))
#6 plots arranged in 2 rows and 3 columns: 
require(gridExtra)
p1 <- ggplot(listing, aes(as.factor(host_is_superhost), price)) + 
  geom_bar(stat = "identity") + 
  labs(y = "price", x = "host_is_superhost")
p2 <- ggplot(listing, aes(as.factor(host_identity_verified), price)) + 
  geom_bar(stat = "identity") + 
  labs(y = "price", x = "host_identity_verified")
p3 <- ggplot(listing, aes(as.factor(room_type), price)) + 
  geom_bar(stat = "identity") + 
  labs(y = "price", x = "room_type")
p4 <- ggplot(listing, aes(as.factor(accommodates), price)) + 
  geom_bar(stat = "identity") + 
  labs(y = "price", x = "accommodates")
p5 <- ggplot(listing, aes(as.factor(bathrooms), price)) + 
  geom_bar(stat = "identity") + 
  labs(y = "price", x = "bathrooms")
p6 <- ggplot(listing, aes(as.factor(bedrooms), price)) + 
  geom_bar(stat = "identity") + 
  labs(y = "price", x = "bedrooms")
grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)

# From the EDA and 6 plots above, the price of an airbnb listing tends to be higher when the host is not a superhost comparing to when the host is one. 
# The price tends to be slightly higher when the host identity is verified comparing to when the it is not.
# In terms of room type, the price tends to be much higher for entire home/apt comparing to the price of private room, and especially to the price of shared room.
# For the variable accommodates, there is not a clear pattern between accommodates and price. From the plot, it looks like listings that accommodate 4 people have the highest price, followed by those that accommodate 6 and 2 people. 
# For the variable bathrooms, listings with 1 bathroom have the highest price, followed by those with 2 bathrooms. There is not a pattern for the rest of the listings with other numbers of bathrooms. 
# for the variable bedrooms, listings with 1 and 2 bedrooms have the highest price, and the price tends to decrease as the number of bedrooms increases. 


## Include the output from the final regression model that you used, as well as evidence that the model fits the assumptions reasonably well. Your regression output should includes a table with coefficients and SEs, and p-values or confidence intervals.
summary2=tidy(listing_model)
kable(summary2, format='markdown',booktabs=T, caption='Regression Model Summary', digits=320)

#host_is_superhost, host_identity_verified, room_type are not applicable for assumptions checking because they are categorical variables, so we will exclude them here for this purpose.

#Check linearity:
ggplot(listing_model,aes(x=log(listing$accommodates), y=listing_model$residual)) +
  geom_point(alpha = .7) +  geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs accommodates",x="accommodates",y="Residuals")

ggplot(listing_model,aes(x=log(listing$bathrooms), y=listing_model$residual)) +
  geom_point(alpha = .7) +  geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs bathrooms",x="bathrooms",y="Residuals")

ggplot(listing_model,aes(x=log(listing$bedrooms), y=listing_model$residual)) +
  geom_point(alpha = .7) +  geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs bedrooms",x="bedrooms",y="Residuals")

ggplot(listing_model,aes(x=log(listing$price), y=listing_model$residual)) +
  geom_point(alpha = .7) + geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs price",x="price",y="Residuals")

#Looking at the x variables against residuals plots, most of the points seem to be equally distributed above and below the 0.0 residuals line, and are randomly distributed, so no clear pattern is identified, and the linearity assumption holds.

#Check independence and equal variance:
plot(listing_model,which=1,col=c("blue4"))

#Looking at the residual against fitted values plot, most of the points seem to be distributed randomly, and there doesn't seem to be a clear pattern. The red line is very close to flat and to the 0.0 residuals line. The points seem to be equally distributed above and below the 0.0 residuals line as well. So the independence and equal variance assumptions hold.

#Check normality:
plot(listing_model,which=2,col=c("blue4"))

#Looking at the Normal Q-Q plot, most points on the graph seem to fall on the 45 degree angle line, with a few deviations at both ends, which could be caused by outliers in the dataset that need further investigations. Therefore, the normality assumption holds, and we conclude that the main assumptions are plausible in this model.


## Interpret the results of your fitted model in the context of the data.
# Only the relationships regarding room_typePrivate room, log(accommodates), log(bathrooms), and bedrooms will be interpreted since only these four predictors are significant at 0.05 level.
# 
# For the predictor room_type, Entire home/apt is the baseline here, the average listing price is expected to decrease by 0.329859 unit on average, at the average price for Entire home/apt, while holding other variables constant.
# For the predictor accommodates, the average listing price is expected to increase by e^(0.328015)-1 = 38.82% for each additional unit increment in accommodates on average, while holding other variables constant.
# For the predictor bathrooms, the average listing price is expected to increased by e^(0.400984)-1 = 49.33% for each additional unit increment in bathrooms on average, while holding other variables constant.
# For bedrooms, the average listing price is expected to increased by 0.110732 unit for each additional unit increment in bedrooms on average, while holding other variables constant.

## Are there any (potential) outliers, leverage points or influential points? Provide evidence to support your response. Also, if there are influential points and/or outliers, exclude the points, fit your model without them, and report the changes in your overall conclusions.
#Plot Cook's Distance and leverage points:
plot(listing_model,which=5,col=c("blue4"))

#Looking at the Residuals vs Leverage plot, there are two influential points, point 31 and point 138, which are beyond the Cook's distance 1 mark. Since both points are beyond the 0.5 mark on the leverage score, both are also high influential points as well as potential outliers. 

#Information regarding the two influential points:
cooksdistance <- cooks.distance(listing_model)
influential <- as.numeric(names(cooksdistance)[(cooksdistance > 3 * mean(cooksdistance, na.rm = TRUE))]) 
listing[influential,]

#Removing the two influential points identified by Cook's Distance and re-fitting the model without them:
subset2 <- subset(listing, id != '5143477' & id != '20481127')
new_listing <- lm(log(price) ~ host_is_superhost + host_identity_verified + 
                    room_type + log(accommodates) + log(bathrooms) + bedrooms, data=subset2)
summary3=tidy(new_listing)
kable(summary3, format='markdown',booktabs=T, caption='Regression Model Summary', digits=320)

#Re-plotting the Residuals vs Leverage: no high influential points or points with high leverage scores are found; most of the points are between -2 to 2 standardized residuals range, with 2 potential outliers.
plot(new_listing,which=5,col=c("blue4"))

#Re-checking linearity assumption: points are generally distributed randomly without any clear pattern, so the assumption still holds.
ggplot(new_listing,aes(x=log(subset2$accommodates), y=new_listing$residual)) +
  geom_point(alpha = .7) +  geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs accommodates",x="accommodates",y="Residuals")

ggplot(new_listing,aes(x=log(subset2$bathrooms), y=new_listing$residual)) +
  geom_point(alpha = .7) +  geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs bathrooms",x="bathrooms",y="Residuals")

ggplot(new_listing,aes(x=log(subset2$bedrooms), y=new_listing$residual)) +
  geom_point(alpha = .7) +  geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs bedrooms",x="bedrooms",y="Residuals")

ggplot(new_listing,aes(x=log(subset2$price), y=new_listing$residual)) +
  geom_point(alpha = .7) + geom_hline(yintercept=0,col="red3") + theme_classic() +
  labs(title="Residuals vs price",x="price",y="Residuals")

#Re-checking independence and normality assumptions: points are generally distributed randomly and are following the 45 degree angle line, so both assumptions still hold.
plot(new_listing,which=1,col=c("blue4"))
plot(new_listing,which=2,col=c("blue4"))


# In conclusion, after removing the two influential points, all the assumptions still hold, and there are no more influential points or leverage points in the model, suggesting that the new model is likely to fit better with the data. 
# In the new output summary:
# For the predictor room_type, the average listing price is expected to decrease by 0.343339 unit on average, at the average price for Entire home/apt(baseline), while holding other variables constant.
# For the predictor accommodates, the average listing price is expected to increase by e^(0.303302)-1 = 35.43% for each additional unit increment in accommodates on average, while holding other variables constant.
# For the predictor bathrooms, the average listing price is expected to increased by e^(0.403184)-1 = 49.66% for each additional unit increment in bathrooms on average, while holding other variables constant.
# For bedrooms, the average listing price is expected to increased by 0.118474 unit for each additional unit increment in bedrooms on average, while holding other variables constant.

## Overall, are there any potential limitations of this analysis? If yes, what are two potential limitations?
# 1. Although the two outliers are removed from the analysis, the model could still not capture the true relationship between these two categorical variables - host_is_superhost and host_identity_verified - and the response variable - price according to the output from the regression model. 
# 2. For some of the residuals plots, a good amount of the points still tend to cluster around the lower end (0 mark) on the plots, and on the normal qq plot, there are still some deviations at both ends. There might be other factors involved, and the residual errors due to other unmeasured factors need to be further investigated. 

