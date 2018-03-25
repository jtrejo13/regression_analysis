# Lab8: Mediation

library(SDSRegressionR)

# Load Data:
sci <- read.csv("data/hsKnowledge.csv", stringsAsFactors = FALSE)
names(sci)

# Full model to look check assumptions
full <- lm(science ~ math + read + write + socst, data=sci)
residFitted(full)
cooksPlot(full, key.variable="hss_id", print.obs=TRUE, sort.obs=TRUE)
threeOuts(full, key.variable="hss_id")

# Remove outliers:
"%not in%" <- Negate("%in%")
g_sci <- sci[sci$hss_id %not in% c("HSS_167", "HSS_150"),]

# Total effect model
pathc <- lm(science ~ math + write + socst, data=g_sci)
summary(pathc)
lmBeta(pathc)

#Path A
patha <- lm(read ~ math + write + socst, data=g_sci)
summary(patha)
lmBeta(patha)

# Path B and Cprime
cprime <- lm(science ~ math + read + write + socst, data=g_sci)
summary(cprime)
lmBeta(cprime)

# Indirect Effect (Multiply paths a and b)
ind <- summary(patha)$coef["math", "Estimate"] *
  summary(cprime)$coef["read", "Estimate"]
ind

# Sobel test
se <- sqrt((summary(cprime)$coef["read", "Estimate"]^2 *
              summary(patha)$coef["math", "Std. Error"]^2) +
             (summary(patha)$coef["math", "Estimate"]^2 *
                summary(cprime)$coef["read", "Std. Error"]^2))
z <- ind/se
z
(1 - pnorm(z)) * 2

# CIs
# lower <- ind - (1.96 * se)
# lower
# upper <- ind + (1.96 * se)
# upper

# Bootstrapping
K <- 20000
ID<- rep(NA, K)
for(i in 1:length(ID)){
  thisdata <- g_sci[sample(nrow(g_sci), nrow(g_sci), replace=TRUE),]
  a <- lm(read ~ math + write + socst, data=thisdata)
  b <- lm(science ~ math + read + write + socst, data=thisdata)
  ID[i] <- summary(a)$coef["math", "Estimate"] *
    summary(b)$coef["read", "Estimate"]
}
ID <- ID[order(ID)]
median(ID) #Center estimate (the Indirect Effect)
ID[0.025*length(ID)] #Lower CI for the Indirect Effect
ID[0.975*length(ID)] #Lower CI for the Indirect Effect

# RMediate (to double check)
library(RMediation)
# Path A
patha <- lm(read ~ math + write + socst, data=g_sci)
# Path B amd C prime
cprime <- lm(science ~ math + read + write + socst, data=g_sci)
aval <- summary(patha)$coef["math", "Estimate"]
bval <- summary(cprime)$coef["read", "Estimate"]
aval_se <- summary(patha)$coef["math", "Std. Error"]
bval_se <- summary(cprime)$coef["read", "Std. Error"]
medci(aval, bval, aval_se, bval_se, plot=TRUE, plotCI=TRUE)
