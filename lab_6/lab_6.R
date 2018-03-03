#### Here is the R script you will use:  (remember that # indicates a comment) ####
#Lab6: Categorical Interaction

library(SDSRegressionR)

#Import data...
work <- read.csv("data/workers.csv", stringsAsFactors=FALSE)
names(work)
sing <- work[which(work$Marital.status == "Single"),]

#Examine the categorical variable:
table(sing$Job)

#Run the Factoring (SupportServices as reference)
sing$Female_f <- factor(sing$Female, levels=c(0,1), labels=c("Male", "Female"))
sing$Job_f <- factor(sing$Job, levels=c("SupportServices", "Academic", "Professional"))

#Initial Model:
#Run the model (SupportServices as reference)
hap <- lm(Happiness ~ Age + Female_f + Job_f + Social.support + 
            Job_f*Social.support, data=sing)
summary(hap)

#Check the diagnostics/outliers
residFitted(hap)
library(car)
vif(hap)
cooksPlot(hap, key.variable="SubID", print.obs=TRUE, sort.obs=TRUE, save.cutoff=TRUE)
threeOuts(hap, key.variable="SubID")

#Get good data
"%not in%" <- Negate("%in%")
g_sing <- sing[sing$SubID %not in% c(...),]

#Re-run the model:
hap2 <- lm(Happiness ~ Age + Female_f + Job_f + Social.support + 
             Job_f*Social.support, data=g_sing)
summary(hap2)

#Check the overall interaction significance
library(car)
Anova(hap2, type="III")

#Simple Slopes
library(lsmeans)
ref.grid(hap2)
lsmeans(hap2, "Social.support", at=list(Social.support = c(0,1)), by="Job_f")
mod_means <- lsmeans(hap2, "Social.support", at=list(Social.support = c(0,1)), by="Job_f")
pairs(mod_means, reverse=TRUE)

# (difference of differences -- Interaction terms)
pairs(update(pairs(mod_means, reverse=TRUE), by=NULL), reverse=TRUE, adjust="none")

#CI Plot (for fun)
library(lsmeans)
mns <- summary(lsmeans(hap2, "Social.support", at=list(Social.support = seq(0,60,1)), by="Job_f"))
simpleScatter(g_sing, x=Social.support, y=Happiness, ptalpha = 0,
              title="Social Support and Happiness by Employment Group") +
  geom_line(data=mns[mns$Job_f=="SupportServices",], aes(x=Social.support, y=lsmean, color="blue")) +
  geom_line(data=mns[mns$Job_f=="Academic",], aes(x=Social.support, y=lsmean, color="red")) + 
  geom_line(data=mns[mns$Job_f=="Professional",], aes(x=Social.support, y=lsmean, color="green")) +
  geom_ribbon(data=mns[mns$Job_f=="SupportServices",],
              aes(y=lsmean, ymin=lower.CL, ymax=upper.CL), alpha=0.3) + 
  geom_ribbon(data=mns[mns$Job_f=="Academic",],
              aes(y=lsmean, ymin=lower.CL, ymax=upper.CL), alpha=0.3) + 
  geom_ribbon(data=mns[mns$Job_f=="Professional",],
              aes(y=lsmean, ymin=lower.CL, ymax=upper.CL), alpha=0.3) + 
  scale_colour_manual(name = "Groups", 
                      values =c("blue", "red", "green"), 
                      labels = c("SupportServices", "Academic", "Professional"))

#Or a straght ggplot...
ggplot(g_sing,aes(x=Social.support,y=Happiness,color=Job_f)) +
  stat_smooth(method=lm, se = FALSE, fullrange=TRUE) +
  geom_point() + 
  labs(title="Job Type Interaction", x="Social.support", y="Happiness") +
  theme_bw()