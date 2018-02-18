# Lab4: Sequential Regression

library(SDSRegressionR)

# import data...
int <- read.csv("data/introverts.csv", stringsAsFactors=FALSE)
names(int)

library(tibble)
# int <- rownames_to_column(int, "SubID") Not needed since data includes 'ID' column

# Determine and run the final model
full <- lm(Happiness ~ Age + ERA + QSR + Neuroticism + Extraversion, data=int)
summary(full)

# Look for any issues:
library(car)
vif(full)
residFitted(full)
cooksPlot(full, key.variable = "ID", print.obs = TRUE, sort.obs=TRUE, save.cutoff = TRUE)
cooksCutOff * 3
threeOuts(full)

# Clean up
"%not in%" <- Negate("%in%")
good_int <- int[int$ID %not in% c('ID231', 'ID272', 'ID265', 'ID326'),]

# Re-run the final model
fullg <- lm(Happiness ~ Age + ERA + QSR + Neuroticism + Extraversion, data=good_int)
summary(fullg)

# Tag observations in the final model - IMPORTANT!
good_int$in_fullg <- tagObs(fullg)

# Keep just those in the model
good_int_fullg <- good_int[which(good_int$in_fullg == 1), ]
sum(good_int_fullg$in_fullg) # Double check

# Sequential Regression:
# Model 1:
m1_seq <- lm(Happiness ~ Age + ERA + QSR, data=good_int_fullg)
summary(m1_seq)
summary(m1_seq)$r.squared
lmBeta(m1_seq)
pCorr(m1_seq)

#Model 2:
m2_seq <- lm(Happiness ~ Age + ERA + QSR + Neuroticism + Extraversion, data=good_int_fullg)
summary(m2_seq)
summary(m2_seq)$r.squared
lmBeta(m2_seq)
pCorr(m2_seq)

# Now the Sequential Results
anova(m1_seq, m2_seq)
