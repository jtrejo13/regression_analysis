##Lab 1: Assessment

#Read in the data
patients <- read.csv("data/ms_edss.csv", stringsAsFactors = FALSE)

#Make sure it's a data.frame
patients <- as.data.frame(patients)

#Assign some variables of interest
vars <- c("delta_TotalGrayVol", "delta_edss", "delta_SubCortGrayVol")

#Make sure libraries are available
library(psych)
library(SDSRegressionR)

#Run a correlation matrix, and get t, p and r^2 values
corr.test(patients[,vars])
corr.test(patients[,vars])$p
corr.test(patients[,vars])$t
corr.test(patients[,vars])$r^2

#Check for linearity
simpleScatter(patients, delta_TotalGrayVol, delta_edss, line=TRUE)

#Run a Simple Linear Regression model
c_mod <- lm(delta_edss ~ delta_TotalGrayVol, patients)
c_mod

#Check for outliers
studResidPlot(c_mod)
levPlot(c_mod)
cooksPlot(c_mod)
threeOuts(c_mod)

#Remove outliers
"%not in%" <- Negate("%in%")
patients_nout <- patients[row.names(patients) %not in% c(71, 62, 24), ]
patients[c(71, 62, 24),]

#Re-run the correlations
corr.test(patients_nout[,vars])
corr.test(patients_nout[,vars])$p
corr.test(patients_nout[,vars])$t
corr.test(patients_nout[,vars])$r^2

#Re-run the Simple Linear Regression Model
simpleScatter(patients_nout, delta_TotalGrayVol, delta_edss, line=TRUE)
c_mod2 <- lm(delta_edss ~ delta_TotalGrayVol, patients_nout)
c_mod2

# Is the change Sub Cortical Gray Matter Volume, a better predictor of overall function change?
#Run a correlation matrix, and get t, p and r^2 values
corr.test(patients[,vars])

#Check for linearity
simpleScatter(patients, delta_SubCortGrayVol, delta_edss, line=TRUE)

#Run a Simple Linear Regression model
c_mod_sub <- lm(delta_edss ~ delta_SubCortGrayVol, patients)
