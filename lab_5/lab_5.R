#### Here is the R script you will use:  (remember that # indicates a comment) ####
#Lab5: Categorical Variables

install.packages("devtools") #if needed...
devtools::install_github("MichaelJMahometa/SDSRegressionR", force=TRUE)

library(SDSRegressionR)

#Import data...
work <- read.csv("data/workers.csv", stringsAsFactors=FALSE)
names(work) #We have an ID, so we need not make one...
sing <- work[which(work$Marital.status == "Single"), ]

#Examine the categorical variable:
table(sing$Job)

#Recode into dummy variables:
#Job Type
sing$Academic <- NA
sing$Academic[!is.na(sing$Job) ] <- 0
sing$Academic[sing$Job == "Academic"] <- 1

sing$Professional <- NA
sing$Professional[!is.na(sing$Job) ] <- 0
sing$Professional[sing$Job == "Professional"] <- 1

sing$SupportServices <- NA
sing$SupportServices[!is.na(sing$Job) ] <- 0
sing$SupportServices[sing$Job == "SupportServices"] <- 1

#Run the model with DUMMIES (SupportServices as reference)
hap <- lm(Happiness ~ Age + Female + Have.child + Academic + Professional +
            Social.support, data=sing)
summary(hap)

#Run the model with FACTOR VARIABLE (SupportServices as reference)
sing$Job <- factor(sing$Job, levels=c("SupportServices", "Academic", "Professional"))
hap_f <- lm(Happiness ~ Age + Female + Have.child + Job + Social.support, data=sing)
summary(hap_f)

#Check the model...
library(car)
vif(hap_f)
residFitted(hap_f)
cooksPlot(hap_f, key.variable = "SubID", print.obs=TRUE, sort.obs = TRUE)
threeOuts(hap, key.variable = "SubID")

#Drop the outliers
"%not in%" <- Negate("%in%")
g_sing <- sing[sing$SubID %not in% c(...),] #Let's discuss

#Rerun
hap2_f <- lm(Happiness ~ Age + Female + Have.child + Job + Social.support, data=g_sing)
summary(hap2_f)

lmBeta(hap2_f)
pCorr(hap2_f)

#Overall ANOVA
library(car)
Anova(hap2_f, type="III")

#Job R^2
lmSingleR2(hap2_f, "Age")

#Post-hoc exploration
library(lsmeans)
hap2_f_mn <- lsmeans(hap2_f, "Job")
hap2_f_mn
pairs(hap2_f_mn, adjust="none") #Too little
pairs(hap2_f_mn, adjust="bonferroni") #Too much
pairs(hap2_f_mn, adjust="holm") #Just right