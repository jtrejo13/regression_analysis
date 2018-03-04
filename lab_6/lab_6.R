#Lab6: Categorical Interaction

library(SDSRegressionR)

#Import data...
state <- read.csv("data/stateAnx.csv", stringsAsFactors=FALSE)
names(state)
 
#Examine the categorical variable:
table(state$Group)

#Run the Factoring (Basic Sciences as reference)
state$Group_f <- factor(state$Group, labels=c("Basic Sciences", "Clinical Sciences"))

#Initial Model:
#Run the model (SupportServices as reference)
anx <- lm(State.Anxiety ~ Age + BDI + WHOQOL.PSY + Group_f +
            Group_f*WHOQOL.PSY, data=state)
summary(anx)

#Check the diagnostics/outliers
residFitted(anx)
library(car)
vif(anx)
cooksPlot(anx, key.variable="IDR", print.obs=TRUE, sort.obs=TRUE, save.cutoff=TRUE)
threeOuts(anx, key.variable="IDR")

#Get good data
"%not in%" <- Negate("%in%")
g_state <- state[state$IDR %not in% c("IDR932", "IDR1289", "IDR154", "IDR639", "IDR686"),]

#Re-run the model:
anx2 <- lm(State.Anxiety ~ Age + BDI + WHOQOL.PSY + Group_f +
            Group_f*WHOQOL.PSY, data=g_state)
summary(anx2)

#Check the overall interaction significance
library(car)
Anova(anx2, type="III")

#Simple Slopes
library(lsmeans)
ref.grid(anx2)
lsmeans(anx2, "WHOQOL.PSY", at=list(WHOQOL.PSY = c(0,1)), by="Group_f")
mod_means <- lsmeans(anx2, "WHOQOL.PSY", at=list(WHOQOL.PSY = c(0,1)), by="Group_f")
pairs(mod_means, reverse=TRUE)

# (difference of differences -- Interaction terms)
pairs(update(pairs(mod_means, reverse=TRUE), by=NULL), reverse=TRUE, adjust="none")

lmDecomp(anx2, "WHOQOL.PSY", "Group_fClinical Sciences", mod.type=2, print.ros = TRUE)

#CI Plot (for fun)
library(lsmeans)
mns <- summary(lsmeans(anx2, "WHOQOL.PSY", at=list(WHOQOL.PSY = seq(16,96,1)), by="Group_f"))
simpleScatter(g_state, x=WHOQOL.PSY, y=State.Anxiety, ptalpha = 0,
              title="Quality of Life:Psychological Assessment and State Anxiety by Student Group") +
  geom_line(data=mns, aes(x=WHOQOL.PSY, y=lsmean, color=Group_f)) +
  geom_ribbon(data=mns, aes(y=lsmean, ymin=lower.CL, ymax=upper.CL, group=Group_f), alpha=0.3) + 
  #Change to your group names and number of groups
  scale_colour_manual(name = "Groups", 
                      values =c("red", "blue"), 
                      labels = c("Basic Sciences", "Clinical Sciences"))

#Or a straght ggplot...
ggplot(g_state,aes(x=WHOQOL.PSY,y=State.Anxiety,color=Group_f)) +
  stat_smooth(method=lm, se = FALSE, fullrange=TRUE) +
  geom_point() + 
  labs(title="Student Group Type Interaction", x="WHOQOL.PSY", y="State.Anxiety") +
  theme_bw()
