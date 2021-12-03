#Assignment 1 - ZK

#Part 1 


#packages
library(gridExtra)
library(psych)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(car)
library(lmtest)
library(cAIC4) 	
library(r2glmm) 	
library(lme4) 
library(lmerTest) 
library(MuMIn) 
library(cAIC4)


#Read in dataset
home_sample_1 <- read.csv("https://tinyurl.com/yxm5rd89")

#Research question 1

###check dataset

#get an overview of the variables
View(home_sample_1)
summary(home_sample_1)
#pain max 55, STAI min : 4.2?!

#>>> check plots of variables:

#pain
plot_part1_pain <- home_sample_1 %>% 
  ggplot()+
  aes(x = pain)+
  geom_histogram()

#sex
plot_part1_sex <- home_sample_1 %>% 
  ggplot()+
  aes(x = sex)+
  geom_bar()

#age
plot_part1_age <- home_sample_1 %>% 
  ggplot()+
  aes(x = age)+
  geom_histogram()

#STAI trait
plot_part1_STAI_trait <- home_sample_1 %>% 
  ggplot()+
  aes(x = STAI_trait)+
  geom_histogram()

  

#pain catastrophizing
plot_part1_pain_cat <- home_sample_1 %>% 
  ggplot()+
  aes(x = pain_cat)+
  geom_histogram()

#mindfulness
plot_part1_mindfulness<- home_sample_1 %>% 
  ggplot()+
  aes(x = mindfulness)+
  geom_histogram()

#cortisol serum
plot_part1_cortisolserum <- home_sample_1 %>% 
  ggplot()+
  aes(x = cortisol_serum)+
  geom_histogram()

#cortisol saliva
plot_part1_cortisolsaliva <- home_sample_1 %>% 
  ggplot()+
  aes(x = cortisol_saliva)+
  geom_histogram()

#Everything looks fine, except for pain max: 55
#and STAI min : 4.2, these values are not in range of
#the scales.


#Check distribution of predictors of model 1 (age and sex) on the
#outcome variable pain

scatter_sex_mod1 <- home_sample_1%>% 
      ggplot() +	
      aes(x = sex, y = pain) +	
      geom_point()
      

scatter_age_mod1 <- home_sample_1 %>% 
  ggplot() +	
  aes(x = age, y = pain) +	
  geom_point()

grid.arrange(scatter_sex_mod1, scatter_age_mod1, ncol=2)

#check for influential cases on model 1 with
#the residuals vs levarage plot and Cook's Distance 

model1_part1 %>% 	
  plot(which = 5)	

model1_part1 %>% 	
  plot(which = 4)	

#The value 55 for the pain measurement and the value 4.2 for 
#the STAI measurement are not in range of the scales. This can be 
#seen in summary function of the dataset and in the plots for the 
#corresponding variables. Furthermore, both,
#the residuals vs levarage plot and Cook's Distance indicate that
#unit 88 is highly influential (Cook's distance 0.8 > 4/160).

# The corresponding cases will therefore be removed 

home_sample_1_cleared <- home_sample_1 %>% 
  slice(-c(34, 88))

##First model with sex and age a predictors
model1_part1 <- lm(pain ~ sex + age, data = home_sample_1_cleared)

###Check assumptions for linear regression model 1 

##1 Normality

# QQ plot	
model1_part1 %>% 	
  plot(which = 2)	
#scatterplot does not show any major violation

#histogram of residuals
residuals_model1_part1 = enframe(residuals(model1_part1))	
residuals_model1_part1 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	

#residuals seem to be normaly distributed

# check the skew and kurtosis	
describe(residuals(model1_part1))

#skew = 0.1, kurtosis = -0.02 
#>> no violation

#No indication for a violation of the assumption of Normality

##2 Linearity

#residual plot function to see if relation between outcome variable
#and predictors is linear

model1_part1 %>% 
  residualPlots()
#There is some curvature on the plots but the tests
#are not significant, therefore we assume the
#assumption of linearity to be held true.

##3 Homoscedasticty

#check plot of the standardized residuals
model1_part1 %>% 
  plot(which = 3)

#Ncvtest
model1_part1 %>% 
  ncvTest()

#Breush-Pagan test
model1_part1 %>% 
  bptest()

#Neither the NCV nor the Breush Pagan test are significant. 
#The plot with the standardized residuals does not show a problematic 
#shape. We therefore take the assumption of Homoscedasticty for not violated.

##4 Multicolliniartity

# Compute the variance inflation factor to check:

model1_part1 %>% 
  vif()

#No multicollinarity 

# Model 1 is not significant -> Both coeffients and the F-test

#Second Model with STAI, pain catastrophizing, mindfulness,
#and cortisol measures as additional predictors

model2_part1 <- lm(pain ~ sex + age + STAI_trait
    + pain_cat + mindfulness + cortisol_serum 
    + cortisol_saliva, data = home_sample_1_cleared)

###Check assumptions for linear regression model 2 

##1 Normality

# QQ plot	
model2_part1 %>% 	
  plot(which = 2)	

#scatterplot does not show any major violation

#histogram of residuals
residuals_model2_part1 = enframe(residuals(model2_part1))	
residuals_model2_part1 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	

#residuals seem to be normaly distributed

# check the skew and kurtosis	
describe(residuals(model2_part1))
#skew = -0.15, kurtosis = -0.03 
#>> no violation

#No indication for a violation of the assumption of Normality

##2 Linearity

#residual plot function to see if relation between outcome variable
#and predictors is linear

model2_part1 %>% 
  residualPlots()
#There is some curvature on the plots but the tests
#are not significant, therefore we assume the
#assumption of linearity to be held true.

##3 Homoscedasticty

#check plot of the standardized residuals
model2_part1 %>% 
  plot(which = 3)

#Ncvtest
model2_part1 %>% 
  ncvTest()

#Breush-Pagan test
model2_part1 %>% 
  bptest()


##4 Multicolliniartity

# Compute the variance inflation factor to check:

model2_part1 %>% 
  vif()

#Cortisol_serum and Cortisol_saliva have a problematic VIF (4.787 and 5.070) this is not 
#suprising since they both measure cortisol levels of the participants 

#check correlation matrix for Cortisol_serum and Cortisol_saliva
cor_cortisol <- cor.test(home_sample_1_cleared$cortisol_serum,
                         home_sample_1_cleared$cortisol_saliva, 
                         method = "pearson")
#Correlation between the two variables is very high (0.853)

#According to the text, serum cortisol is regarded to be a better predictor than 
#salia cortisol, therefore cortisol saliva will be removed from the model 


#Build new model without cortisol_saliva:

model3_part1 <- lm(pain ~ sex + age + STAI_trait
                   + pain_cat + mindfulness 
                   + cortisol_serum, data = home_sample_1_cleared)

#run summary of new model 
summary(model3_part1)

#Model still siginifcant, R2 changed only minimally

###Comparing the two models - Model fit analysis 

#R2
summary(model1_part1)$adj.r.squared
summary(model3_part1)$adj.r.squared

#R2 increases drastically

#Sum of sqaures (residual error)
anova(model1_part1)
anova(model3_part1)
# Residual Sum of squares decreas from 332.89 to 173.60

#AIC
AIC(model1_part1)
AIC(model3_part1)
# AIC decreases from 574.127 to 479.262

#F-test comparing the two nested models
anova(model1_part1, model3_part1)
# F-test is significant
# All indices are in favor for the more complex model!


###Part 2 

#Building a backwards regression model

model_backward_part2 <- lm(pain~ age + sex + STAI_trait + pain_cat + 
                            mindfulness + cortisol_serum + IQ + weight + household_income, data = home_sample_1_cleared)


#>>> checking plots of new variables (IQ, weight, household_income)

#household_income
plot_part2_household_income <- home_sample_1_cleared %>% 
  ggplot()+
  aes(x = household_income)+
  geom_histogram()#

#IQ
plot_part2_IQ <- home_sample_1_cleared %>% 
  ggplot()+
  aes(x = IQ)+
  geom_histogram()

#weight
plot_part2_weight<- home_sample_1_cleared %>% 
  ggplot()+
  aes(x = weight)+
  geom_histogram()

#Check distribution of the new predictors of the backwards regression model 
#(IQ, weight, household_income) on the outcome variable pain

#household_income

scatter_household_income<- home_sample_1_cleared%>% 
  ggplot() +	
  aes(x = household_income, y = pain) +	
  geom_point()

#IQ

scatter_IQ <- home_sample_1_cleared%>% 
  ggplot() +	
  aes(x = IQ, y = pain) +	
  geom_point()

#weight

scatter_weight<- home_sample_1_cleared%>% 
  ggplot() +	
  aes(x = weight, y = pain) +	
  geom_point()

#check for influential cases on backwards model with
#the residuals vs levarage plot and Cook's Distance 

model_backward_part2 %>% 	
  plot(which = 5)	

model_backward_part2 %>% 	
  plot(which = 4)	

#Cases 84,85 and 46 have a relatively high Cook's distance, but 
#the dataset doesnt seem wrong so probably I should keep them 

###Model diagnostics backwards regression

##Normality

# QQ plot	
model_backward_part2 %>% 	
  plot(which = 2)	

#scatterplot does not show any major violation

#histogram of residuals
residuals_model_backward_part2 = enframe(residuals(model_backward_part2))	
residuals_model_backward_part2 %>% 	
  ggplot() +	
  aes(x = value) +	
  geom_histogram()	

#residuals seem to be normaly distributed

# check the skew and kurtosis	
describe(residuals(model_backward_part2))
#skew = -0.17, kurtosis = -0.08
#>> no violation

#No indication for a violation of the assumption of Normality

##2 Linearity

#residual plot function to see if relation between outcome variable
#and predictors is linear

model_backward_part2%>% 
  residualPlots()
#There is some curvature on the plots but the tests
#are not significant, therefore we assume the
#assumption of linearity to be held true.

##3 Homoscedasticty

#check plot of the standardized residuals
model_backward_part2 %>% 
  plot(which = 3)

#Ncvtest
model_backward_part2 %>% 
  ncvTest()

#Breush-Pagan test
model_backward_part2%>% 
  bptest()

#No sign for homoscedasticity

##4 Multicolliniartity

# Compute the variance inflation factor to check:

model_backward_part2 %>% 
  vif()

#No high VIF values

##There seems to be no problem with the variables or the assumptions
#therefore lets start the backwards regression

model_backwards_train = step(model_backward_part2, direction = "backward")	

#final model obtained with bachward regression:

backward_model <- lm(pain ~ age + pain_cat + mindfulness + cortisol_serum, data = home_sample_1_cleared)
theory_based_model <- model3_part1

##Compare backward model and theory model

summary(theory_based_model)$adj.r.squared
#[1] 0.504055
summary(backward_model)$adj.r.squared
#[1] 0.5073085


AIC(theory_based_model)
#[1] 479.2624
AIC(backward_model)
#[1] 476.3015

##Theory based model seems to perform worse.

###Test the two models on new data

#read in new data

home_sample_2 <- read.csv("https://tinyurl.com/87v6emky")


###check dataset

#get an overview of the variables
View(home_sample_2)
summary(home_sample_2)

#>>> check plots of variables:

#pain
plot_part2_sample2_pain <- home_sample_2 %>% 
  ggplot()+
  aes(x = pain)+
  geom_histogram()
plot_part2_sample2_pain

#sex
plot_part2_sample2_sex <- home_sample_2 %>% 
  ggplot()+
  aes(x = sex)+
  geom_histogram()
plot_part2_sample2_sex

#gender
plot_part2_sample2_sex <- home_sample_2 %>% 
  ggplot()+
  aes(x = sex)+
  geom_bar()
plot_part2_sample2_sex

#age
plot_part2_sample2_age <- home_sample_2 %>% 
  ggplot()+
  aes(x = age)+
  geom_dotplot()
plot_part2_sample2_age

#STAI_trait
plot_part2_sample2_STAI_trait<- home_sample_2 %>% 
  ggplot()+
  aes(x = STAI_trait)+
  geom_histogram()
plot_part2_sample2_STAI_trait

#pain_cat
plot_part2_sample2_pain_cat<- home_sample_2 %>% 
  ggplot()+
  aes(x = pain_cat)+
  geom_histogram()
plot_part2_sample2_pain_cat

#cortisol_serum
plot_part2_sample2_cortisol_serum<- home_sample_2 %>% 
  ggplot()+
  aes(x = cortisol_serum)+
  geom_histogram()
plot_part2_sample2_cortisol_serum

#cortisol_saliva
plot_part2_sample2_cortisol_saliva<- home_sample_2 %>% 
  ggplot()+
  aes(x = cortisol_saliva)+
  geom_histogram()
plot_part2_sample2_cortisol_saliva

#mindfulness
plot_part2_sample2_mindfulness<- home_sample_2 %>% 
  ggplot()+
  aes(x = mindfulness)+
  geom_histogram()
plot_part2_sample2_mindfulness

#weight
plot_part2_sample2_weight<- home_sample_2 %>% 
  ggplot()+
  aes(x = weight)+
  geom_histogram()
plot_part2_sample2_weight

#IQ

plot_part2_sample2_IQ<- home_sample_2 %>% 
  ggplot()+
  aes(x = IQ)+
  geom_histogram()
plot_part2_sample2_IQ

#household_income
plot_part2_sample2_household_income<- home_sample_2 %>% 
  ggplot()+
  aes(x = household_income)+
  geom_histogram()
plot_part2_sample2_household_income

#Everything seems fine

##

# calculate predicted values 	
pred_theory <- predict(theory_based_model, home_sample_2)	
pred_backward <- predict(backward_model, home_sample_2)	

#Calculate residuals sum of squares for both models on new data 
RSS_newdata_theory = sum((home_sample_2[,"pain"] - pred_theory)^2)	
RSS_newdata_back = sum((home_sample_2[,"pain"] - pred_backward)^2)	
RSS_newdata_theory
RSS_newdata_back	
# RSS_newdata_theory
#[1] 243.8192
# RSS_newdata_back	
#[1] 249.5637

##Theory based model has lower RSS!!

###part 3 

#function to extrat standardized beta coefficients
stdCoef.merMod <- function(object) {	
  sdy <- sd(getME(object,"y"))	
  sdx <- apply(getME(object,"X"), 2, sd)	
  sc <- fixef(object)*sdx/sdy	
  se.fixef <- coef(summary(object))[,"Std. Error"]	
  se <- se.fixef*sdx/sdy	
  return(data.frame(stdcoef=sc, stdse=se))	
}	

#read in new data

hospital_data_3 <- read.csv("https://tinyurl.com/b385chpu")
hospital_data_4 <- read.csv("https://tinyurl.com/4f8thztv")

#check summary of the data

summary(hospital_data_3)
view(hospital_data_3)

#One case reported 'woman' as sex 
#instead of 'female', this case will be removed

hospital_data_3 <- hospital_data_3 %>% 
  slice(-c(25))
  

# lowest value on variable household_income is negative....?

plot_part3_householdincome <- hospital_data_3 %>% 
  ggplot()+
  aes(x = household_income)+
  geom_histogram()
# Remove the case? >> No, it probably means that this case is in debt.

summary(hospital_data_3)

#seems fine

#Build random intercept model
 
model_intercept_data3 = lmer(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + 
                               cortisol_serum + (1|hospital), data = hospital_data_3)	
#check the model for significance using r2beta
#to check CIs of the model 
r2beta(model_intercept_data3)

#Compare coeffiecents:

summary(model_intercept_data3)
stdCoef.merMod(model_intercept_data3)

#compute confidence intervals 
confint(model_intercept_data3)
coef_CI = suppressWarnings(confint(model_intercept_data3))	
coef_CI	


#Coefficents are very similar but not the same 
   
# marginal R2+ confidence intervals	
rsquared_withCI <- r2beta(model_intercept_data3, method = "nsj", data = hospital_data_3)	

# marginal and conditional R2
M_C_R <- r.squaredGLMM(model_intercept_data3)	



##test the model on dataset 4 

#make predictions
pred_intercept <- predict(model_intercept_data3, hospital_data_4, allow.new.levels = TRUE)

#calculate RSS
RSS_intercept_data4 = sum((hospital_data_4[,"pain"] - pred_intercept)^2)

#calculate TSS
TSS_intercept_data4 = sum((hospital_data_4$pain - predict(model_intercept_data3))^2)	

#Calculate Variance explained (R2):

Rsquared_interceptmode_data4 <- 1-(RSS_intercept_data4/TSS_intercept_data4)
Rsquared_interceptmode_data4

#Compare to R2 for data 4 to R2 from data 3 
M_C_R 
Rsquared_interceptmode_data4

#build model with only the most influential predictor

summary(model_intercept_data3)

#most influential predictor is cortisol-serum, the new model will therefore be build with this one

model_most_influential <- lmer(pain ~ cortisol_serum +  (cortisol_serum|hospital), data = hospital_data_3)

#analyze model
summary(model_most_influential)
confint(model_most_influential)
coef_CI = suppressWarnings(confint(model_intercept_data3))	
coef_CI	
stdCoef.merMod(model_most_influential)
# marginal and conditional R2
M_C_R_I <- r.squaredGLMM(model_most_influential)	


#compare the intercept with the slope model 

cAIC(model_intercept_data3)
cAIC(model_most_influential)

anova(model_intercept_data3, model_most_influential)

#make predictions for regression line

hospital_data_3 %>% 
  pred_influential <- predict(model_most_influential, hospital_data_, allow.new.levels = TRUE)

#build graph for different hospitals
hospital_data_3 %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, group = hospital)+		
  geom_point(aes(color = hospital), size = 4) +		
  geom_line(color='red', aes(y=pred_influential, x= cortisol_serum))+		
  facet_wrap( ~ hospital, ncol = 2)

