# Lab7: Quantitative Interaction

# library(SDSRegressionR)
# 
# #Load Data
# vit <- read.csv("data/VitaminD.csv", stringsAsFactors=FALSE)
# names(vit)
# 
# #Run first model for diagnostics FIRST
# #First run
# v_mod <- lm(D25 ~ D125 + ageyears + bmi + pth + bmi*ageyears + pth*ageyears, data=vit)
# summary(v_mod)
# 
# #Diagnostics
# library(car)
# vif(v_mod)
# residFitted(v_mod)
# cooksPlot(v_mod, key.variable = "ID", print.obs=TRUE, sort.obs=TRUE)
# 
# #Three bad outliers
# "%not in%" <- Negate("%in%")
# g_vit <- vit[vit$ID %not in% c("ID322", "ID156", "ID496"),]
# 
# #Re-run
# v_mod2 <- lm(D25 ~ D125 + ageyears + bmi + pth + bmi*ageyears + pth*ageyears, data=g_vit)
# summary(v_mod2)
# 
# #Find the Simple Slope locations (Pick-a-Point)
# mean(g_vit$ageyears, na.rm=TRUE)
# mean(g_vit$ageyears, na.rm=TRUE) - sd(g_vit$ageyears, na.rm=TRUE)
# mean(g_vit$ageyears, na.rm=TRUE) + sd(g_vit$ageyears, na.rm=TRUE)
# 
# #lsmeans for simple slopes
# library(lsmeans)
# simple_mns <- lsmeans(v_mod2, "bmi", 
#                       at=list(bmi=c(0,1), ageyears=c(8.18, 9.87, 11.56)), 
#                       by="ageyears")
# simple_mns
# pairs(simple_mns, reverse=TRUE)
# 
# # And Find the Regions of Significance
# lmDecomp(v_mod2, "bmi", "ageyears", mod.values=c(8.18, 9.87, 11.56), 
#          print.sslopes=FALSE, print.ros=TRUE)
# 
# #Better graphs
# ROS +
#   labs(x="Age in Years", title="ROS of Age", subtitle="BMI predicting D25 Serum")
# 
# mns <- summary(lsmeans(v_mod2, "bmi", 
#                        at=list(bmi=seq(10, 30, 1), ageyears=c(8.18, 9.87, 11.56)), 
#                        by="ageyears"))
# simpleScatter(g_vit, x=bmi, y=D25, ptalpha = 0,
#               title="BMI on D25 Moderated by Age") +
#   geom_line(data=mns, aes(x=bmi, y=lsmean, linetype=factor(ageyears))) +
#   scale_linetype_manual(name = "Age Values", 
#                         values = c("dashed", "dotdash", "dotted"), 
#                         labels = c("One SD Below","Mean of Age", "One SD Above"))