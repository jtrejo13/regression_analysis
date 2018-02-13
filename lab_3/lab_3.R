#Lab3: Multiple Linear Regression

library(SDSRegressionR)

#import data...
vitamin <- read.csv("data/VitaminD.csv", stringsAsFactors=FALSE)
names(vitamin)

# Code to add a Key Identifier - NOT needed in this case
# library(tibble)
# vitamin <- rownames_to_column(vitamin, "NewID")

#Inital correlationss
vars <- c("D25", "ageyears", "D125", "weight", "bmi", "pth")
library(psych)
corr.test(vitamin[,vars])

#First model
q_mod <- lm(D25 ~ ageyears + D125 + weight + bmi + pth, data=vitamin)
summary(q_mod)
library(car)
vif(q_mod)
1/vif(q_mod)

# Remove weight due to multicolinearity with other independents:
q_mod2 <- lm(D25 ~ ageyears + D125 + bmi + pth, data=vitamin)
summary(q_mod2)
vif(q_mod2)

# Check assumptions

# Homoscedasticity
residFitted(q_mod2)

#Find the outliers...
cooksPlot(q_mod2, key.variable = "ID", print.obs = TRUE, sort.obs = TRUE)
threeOuts(q_mod2, key.variable = "ID")

# Remove the outlier(s)
"%not in%" <- Negate("%in%")
good_vit <- vitamin[vitamin$ID %not in% c("ID323", "ID496"),]

#Final Model
q_mod_f <- lm(D25 ~ ageyears + D125 + bmi + pth, data=good_vit)
summary(q_mod_f)

# Re-check cooksPlot to confirm outlier removal
cooksPlot(q_mod_f, key.variable = "ID", print.obs = TRUE, sort.obs = TRUE)

# Re-check Homoscedasticity
residFitted(q_mod_f)

confint(q_mod_f) #Confidence intervals for the slopes (for reporting)
lmBeta(q_mod_f) #Standardized Betas for our final model
pCorr(q_mod_f) #Partial and Part correlation coefficients
 
#Predict
library(lsmeans)
ref.grid(q_mod_f)
lsmeans(q_mod_f, "pth", at=list(pth=80))
