# Lab7: Quantitative Interaction

library(SDSRegressionR)

# Load Data
conn <- read.csv("data/connected.csv", stringsAsFactors=FALSE)
names(conn)

# First run
c_mod <- lm(Problem.Beh ~ age + female + School.Connectedness + Family.Connectedness + Depressive.Sympt + Depressive.Sympt*School.Connectedness, data=conn)
summary(c_mod)

# Diagnostics
library(car)
vif(c_mod)
residFitted(c_mod)
cooksPlot(c_mod, key.variable = "STUD_ID", print.obs=TRUE, sort.obs=TRUE)
threeOuts(c_mod, key.variable = "STUD_ID")

# Outlier Removal
"%not in%" <- Negate("%in%")
g_conn <- conn[conn$STUD_ID %not in% c("SID134", "SID192"),]
summary(g_conn$School.Connectedness)
mean(g_conn$School.Connectedness)
sd(g_conn$School.Connectedness)

# Re-run
c_mod2 <- lm(Problem.Beh ~ age + female + School.Connectedness + Family.Connectedness + Depressive.Sympt + Depressive.Sympt*School.Connectedness, data=g_conn)
summary(c_mod2)

# Find the Simple Slope locations (Pick-a-Point)
mean(g_conn$School.Connectedness, na.rm=TRUE)
mean(g_conn$School.Connectedness, na.rm=TRUE) - sd(g_conn$School.Connectedness, na.rm=TRUE)
mean(g_conn$School.Connectedness, na.rm=TRUE) + sd(g_conn$School.Connectedness, na.rm=TRUE)

#lsmeans for simple slopes
library(lsmeans)
simple_mns <- lsmeans(c_mod2, "Depressive.Sympt",
                      at=list(Depressive.Sympt=c(0,1), School.Connectedness=c(2.658, 4.358, 6.057)),
                      by="School.Connectedness")
simple_mns
pairs(simple_mns, reverse=TRUE)

# And Find the Regions of Significance
lmDecomp(c_mod2, "Depressive.Sympt", "School.Connectedness", mod.values=c(2.658, 4.358, 6.057),
         print.sslopes=FALSE, print.ros=TRUE)

# Better graphs
ROS +
  labs(x="School Connectedness", title="ROS of School Connectedness", subtitle="Depressive Symptoms predicting Problem Behavior")

mns <- summary(lsmeans(c_mod2, "Depressive.Sympt",
                       at=list(Depressive.Sympt=seq(0, 10, 1), School.Connectedness=c(2.658, 4.358, 6.057)),
                       by="School.Connectedness"))
simpleScatter(g_conn, x=Depressive.Sympt, y=Problem.Beh, ptalpha = 0,
              title="Depressive Symptoms on Problem Behavior Moderated by School Connectedness") +
  geom_line(data=mns, aes(x=Depressive.Sympt, y=lsmean, linetype=factor(School.Connectedness))) +
  scale_linetype_manual(name = "School Connectedness Values",
                        values = c("dashed", "dotdash", "dotted"),
                        labels = c("One SD Below","Mean of School Connectedness", "One SD Above"))

