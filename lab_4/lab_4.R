#### Here is the R script you will use:  (remember that # indicates a comment) ####
#Lab4: Sequential Regression

library(SDSRegressionR)

#import data...
coh <- read.csv("data/coherance.csv", stringsAsFactors=FALSE)
names(coh)

library(tibble)
coh <- rownames_to_column(coh, "SubID")

#Determine and run the final model
full <- lm(quallife_mean ~ age + female + child + full_ed + coherance_mean + 
             impact_mean, data=coh)

#Look for any issues:
library(car)
vif(full)
residFitted(full)
cooksPlot(full, key.variable = "SubID", print.obs = TRUE, sort.obs=TRUE, save.cutoff = TRUE)
cooksCutOff * 2
threeOuts(full)

#Clean up
"%not in%" <- Negate("%in%")
good_coh <- coh[coh$SubID %not in% c(...),] #You should complete this part...

#Re-run the final model
fullg <- lm(quallife_mean ~ age + female + child + full_ed + coherance_mean + 
              impact_mean, data=good_coh)

#Tag observations in the final model - IMPORTANT!
good_coh$in_fullg <- tagObs(fullg)

#Keep just those in the model
good_coh_fullg <- good_coh[which(good_coh$in_fullg == 1), ]
sum(good_coh_fullg$in_fullg) #Double check

#Now for the Sequential Regression:
#Model 1:
m1_seq <- lm(quallife_mean ~ age + female + child + full_ed, data=good_coh_fullg)
summary(m1_seq)
summary(m1_seq)$r.squared
lmBeta(m1_seq)
pCorr(m1_seq)

#Model 2:
m2_seq <- lm(quallife_mean ~ age + female + child + full_ed + coherance_mean + 
               impact_mean, data=good_coh_fullg)
summary(m2_seq)
summary(m2_seq)$r.squared
lmBeta(m2_seq)
pCorr(m2_seq)

#Now the Sequential Results
anova(m1_seq, m2_seq)
