#Lab10: Multiple Logistic Regression

library(SDSRegressionR)
# Import data
prostate <- read.csv("data/prostate.csv", stringsAsFactors = FALSE)
names(prostate)

# Count check
count(prostate$CAPSULE)

#Intital Model
b_mod <- glm(CAPSULE ~ AGE + PSA + VOL + GLEASON, data=prostate, family="binomial")
summary(b_mod)

library(car)
vif(b_mod)
cooksPlot(b_mod, key.variable = "PatientID", print.obs = TRUE, sort.obs = TRUE)
threeOuts(b_mod, key.variable = "PatientID")

# Remove outliers
"%not in%" <- Negate("%in%")
g_prostate <- prostate[prostate$PatientID %not in% c('ID_76', 'ID_8', 'ID_89'),]

# Re-run
b_mod2 <- glm(CAPSULE ~ AGE + PSA + VOL + GLEASON, data=g_prostate, family="binomial")
summary(b_mod2)

# Odds-ratios
exp(b_mod2$coef)
exp(confint.default(b_mod2))

# Stats
library(rms)
b_mod2.2 <- lrm(CAPSULE ~ AGE + PSA + VOL + GLEASON, g_prostate)
b_mod2.2

#Examine the variables of interest graphically...
#Look at ranges...
summary(g_prostate)

# Predict
library(lsmeans)
prostate_mns <- summary(lsmeans(b_mod2, "PSA",
                             at=list(PSA = seq(0, 140, 10)), type="response"))

# Graph PSA
g <- simpleScatter(g_prostate, PSA, CAPSULE, title="PSA",
                   xlab="PSA", ylab="P(Prostate Cancer)")
g +
  geom_line(data=prostate_mns, aes(x=PSA, y=prob), color="red") +
  geom_line(data=prostate_mns, aes(x=PSA, y=asymp.LCL), linetype="dashed") +
  geom_line(data=prostate_mns, aes(x=PSA, y=asymp.UCL), linetype="dashed") +
  geom_vline(xintercept = 55, color="blue")


prostate_mns_G <- summary(lsmeans(b_mod2, "GLEASON",
                                at=list(GLEASON = seq(0, 10, 0.5)), type="response"))

#Graph GLEASON
g2 <- simpleScatter(g_prostate, GLEASON, CAPSULE, title="GLEASON",
                   xlab="GLEASON", ylab="P(Prostate Cancer)")
g2 +
  geom_line(data=prostate_mns_G, aes(x=GLEASON, y=prob), color="red") +
  geom_line(data=prostate_mns_G, aes(x=GLEASON, y=asymp.LCL), linetype="dashed") +
  geom_line(data=prostate_mns_G, aes(x=GLEASON, y=asymp.UCL), linetype="dashed") +
  geom_vline(xintercept = 6.7700, color="blue")

library(lsmeans)
summary(b_mod2)
ref.grid(b_mod2)

# PSA
alpha_PSA <- -5.373861
beta_PSA  <- 0.032964
omega_PSA <- (-0.027747 * 66.145) + (-0.014582 * 16.264) + (1.025764 * 6.3669)
vmark_PSA <- (log(0.50 / (1 - 0.50)) - alpha_PSA - omega_PSA) / beta_PSA
vmark_PSA

# GLEASON
alpha_G <- -5.373861
beta_G <- 1.025764
omega_G <- (-0.027747 * 66.145) + (-0.014582 * 16.264) + (0.032964 * 15.377)
vmark_G <- (log(0.50 / (1 - 0.50)) - alpha_G - omega_G) / beta_G
vmark_G