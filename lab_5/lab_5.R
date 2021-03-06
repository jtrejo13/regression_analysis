# Lab5: Categorical Variables

install.packages("devtools")
devtools::install_github("MichaelJMahometa/SDSRegressionR", force=TRUE)

library(SDSRegressionR)

#Import data...
dent <- read.csv("data/dentalAnxiety.csv", stringsAsFactors=FALSE)
names(dent)

# #Examine the categorical variables
table(dent$Sex)
table(dent$Education)

# Recode into dummy variables:
# Sex

dent$Female <- NA
dent$Female[!is.na(dent$Sex) ] <- 0
dent$Female[dent$Sex == 2] <- 1

dent$Male <- NA
dent$Male[!is.na(dent$Sex) ] <- 0
dent$Male[dent$Sex == 1] <- 1

# Education
dent$Primary <- NA
dent$Primary[!is.na(dent$Education) ] <- 0
dent$Primary[dent$Education == 1] <- 1

dent$Secondary <- NA
dent$Secondary[!is.na(dent$Education) ] <- 0
dent$Secondary[dent$Education == 2] <- 1

dent$HighSchool <- NA
dent$HighSchool[!is.na(dent$Education) ] <- 0
dent$HighSchool[dent$Education == 3] <- 1

dent$University <- NA
dent$University[!is.na(dent$Education) ] <- 0
dent$University[dent$Education == 4] <- 1

# Run the model with DUMMIES (Male and Primary as reference)
fear <- lm(DFS ~ Age + Female + BDI + Secondary + HighSchool +
            University, data=dent)
summary(fear)

# Run the model with FACTOR VARIABLE (Male and Primary as reference)
dent$Education_factor <- factor(dent$Education, levels=c(1, 2, 3, 4),
                         labels=c("Primary", "Secondary", "HighSchool", "University"))
dent$Sex_factor <- factor(dent$Sex, levels=c(1, 2),
                          labels=c("Male", "Female"))
fear_f <- lm(DFS ~ Age + Sex_factor + BDI + Education_factor, data=dent)
summary(fear_f)

# Check the model
library(car)
vif(fear_f)
residFitted(fear_f)
cooksPlot(fear_f, key.variable = "UniqueID", print.obs=TRUE, sort.obs = TRUE)
threeOuts(fear_f, key.variable = "UniqueID")

#Drop the outliers
"%not in%" <- Negate("%in%")
g_dent <- dent[dent$UniqueID %not in% c("SUB200", "SUB145", "SUB220"),]

#Rerun
fear2_f <- lm(DFS ~ Age + Sex_factor + BDI + Education_factor, data=g_dent)
summary(fear2_f)

lmBeta(fear2_f)
pCorr(fear2_f)

# Overall ANOVA
library(car)
Anova(fear2_f, type="III")

# R^2
lmSingleR2(fear2_f, "Age")
lmSingleR2(fear2_f, "Sex_factor")
lmSingleR2(fear2_f, "BDI")
lmSingleR2(fear2_f, "Education_factor")

# Post-hoc exploration
library(lsmeans)
fear2_f_mn <- lsmeans(fear2_f, "Education_factor")
fear2_f_mn
pairs(fear2_f_mn, adjust="none")
pairs(fear2_f_mn, adjust="bonferroni")
pairs(fear2_f_mn, adjust="holm")

