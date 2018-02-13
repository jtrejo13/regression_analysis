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

# #Remove the problem independent:
# q_mod2 <- lm(MS.QoL ~ DREEM.S.SP + Resilience + BDI + Age, data=clinical)
# summary(q_mod2)
# vif(q_mod2)
# 
# #Good model: Check assumptions
# residFitted(q_mod2)
# 
# #Find the outliers...
# cooksPlot(q_mod2, key.variable = "IDR", print.obs = TRUE, sort.obs = TRUE)
# threeOuts(q_mod2, key.variable = "IDR")
# 
# #Remove the outlier(s)
# "%not in%" <- Negate("%in%")
# good_clin <- clinical[clinical$IDR %not in% c("IDR897"),]
# 
# #Final Model
# q_mod_f <- lm(MS.QoL ~ DREEM.S.SP + Resilience + BDI + Age, data=good_clin)
# summary(q_mod_f)
# residFitted(q_mod_f) #Just checking
# confint(q_mod_f) #Confidence intervals for the slopes (for reporting)
# lmBeta(q_mod_f) #Standardized Betas for our final model
# pCorr(q_mod_f) #Partial and Part correlation coefficients
# 
# #Predict
# library(lsmeans)
# ref.grid(q_mod_f)
# lsmeans(q_mod_f, "DREEM.S.SP", at=list(DREEM.S.SP=10))
# 
# #Visualize...
# #New "mean" data and prediction for fit and confidence
# library(psych)
# describe(good_clin$DREEM.S.SP)
# pgr <- summary(lsmeans(q_mod_f, "DREEM.S.SP", at=list(DREEM.S.SP=seq(8, 26, 0.5))))
# 
# #Get the graph....
# s.sp_gr <- simpleScatter(good_clin, DREEM.S.SP, MS.QoL,
#                          title="Social Perception and Quality of Life", 
#                          xlab="DREEM Social Self Perception", ylab="Quality of Life")
# #Add fit and confidence
# s.sp_gr + 
#   geom_line(data=pgr, aes(x=DREEM.S.SP, y=lsmean), color="red") +
#   geom_ribbon(data=pgr, aes(x=DREEM.S.SP, y=lsmean, ymin=lower.CL, ymax=upper.CL), alpha=0.3)