# Lab12: Categorical Predictors in Logistic

library(SDSRegressionR)

# Data
ghq <- read.csv("data/psychDiabetes.csv", stringsAsFactors=FALSE)
names(ghq)

# Factorize where needed
table(ghq$Marriage)
ghq$Marriage <- factor(ghq$Marriage, levels=c(0,1,2,3), labels=c("Married"," Never Married", "Divorced", "Widowed"))
table(ghq$NumberOfOtherIllnesses)
ghq$NumberOfOtherIllnesses <- factor(ghq$NumberOfOtherIllnesses, levels=c(4,3,2,1), labels=c("=0", "=1", "=2", ">=3"))
table(ghq$DailySleepDuration)
ghq$DailySleepDuration <- factor(ghq$DailySleepDuration, levels=c(0,1), 
                               labels=c("<7", ">=7"))
table(ghq$Distress)
ghq$Distress <- factor(ghq$Distress, levels=c(0,1), 
                                 labels=c("No Distress", "Distress"))

#Initial model
d_mod <- glm(Distress ~ Age + Marriage + NumberOfOtherIllnesses + DailySleepDuration, ghq, family="binomial")

# Assumption check
library(car)
vif(d_mod)

# Outlier check
cooksPlot(d_mod, key.variable = "SubID", print.obs = TRUE, sort.obs = TRUE)

# Clean data
"%not in%" <- Negate("%in%")
g_ghq <- ghq[which(ghq$SubID %not in% 
                         c("519", "970", "579", "1440" )), ]

# Re-run the model
d_mod2 <- glm(Distress ~ Age + Marriage + NumberOfOtherIllnesses + DailySleepDuration, g_ghq, family="binomial")
summary(d_mod2)

# Investiage
diff = 1562.6 - 1521.8
1 - pchisq(diff, 5, lower.tail = TRUE)

library(rms)
lrm(Distress ~ Age + Marriage + NumberOfOtherIllnesses + DailySleepDuration, g_ghq)

# Pseudo R2
library(DescTools)
PseudoR2(d_mod2, which = "Nagelkerke")

#Test the full categorical variable
library(car)
Anova(d_mod2, type="III")

#Post-hoc testing
library(lsmeans)
lsmeans(d_mod2, "NumberOfOtherIllnesses", type="response")
pairs(lsmeans(d_mod2, "NumberOfOtherIllnesses"), reverse=TRUE, adjust="none")

#Graph
mns <- summary(lsmeans(d_mod2, "NumberOfOtherIllnesses", type="response"))

library(ggplot2)
ggplot(mns, aes(x=NumberOfOtherIllnesses, y=prob)) +
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL), width=0.2) +
  theme_bw()
