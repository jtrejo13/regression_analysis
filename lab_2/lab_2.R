##Lab 2: Simple Regression, Inference

data <- read.csv("data/Resilience.csv", stringsAsFactors=FALSE)

# Subseting the data
basic <- subset(data, Group=="Basic Sciences")

# Define variables that might be of interest
vars <- c("MS.QoL", "DREEM.A.SP")

# New way to get basic descriptives:
library(psych)
describe(basic[,vars], IQR = TRUE)
 
# Take an intial look at the variables
corr.test(basic[,vars])$r
corr.test(basic[,vars])$r^2*100

# Check linearity
library(SDSRegressionR)
simpleScatter(basic, DREEM.A.SP, MS.QoL, line=TRUE)

# Run the intial model
init_model <- lm(MS.QoL ~ DREEM.A.SP, data=basic)
summary(init_model)

# Look for outliers
threeOuts(init_model)
cooksPlot(init_model)

#Remove the highest outlier
"%not in%" <- Negate("%in%")
good <- basic[row.names(basic) %not in% (c(208)), ]
corr.test(good[,vars])$r^2*100

#Re-run the model
new_model <- lm(MS.QoL ~ DREEM.A.SP, data=good)
summary(new_model)

# Grab confidence intervals
confint(new_model)

# Predict new MS.QoL
nw_dreem <- data.frame(DREEM.A.SP = 10)
predict(new_model, nw_dreem, interval="prediction")

# Test if different slope is significant
library(car)
linearHypothesis(new_model, "DREEM.A.SP =  0.175")
