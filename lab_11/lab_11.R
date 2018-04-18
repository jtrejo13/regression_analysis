#Lab11: Multinomial Logistic Regression

library(SDSRegressionR)

# Import Data
emp <- read.csv("data/employment.csv", stringsAsFactors = FALSE)

# Factor
table(emp$mhs)
table(emp$fhs)
table(emp$empl)
emp$mhs <- factor(emp$mhs, levels=c(0,1), labels=c("No", "Yes"))
emp$fhs <- factor(emp$fhs, levels=c(0,1), labels=c("No", "Yes"))
emp$empl <- factor(emp$empl, levels=c(0,1,2), labels=c("Not Employed", "Working", "In School"))

#Model
library(nnet)
m_emp <- multinom(empl ~ age + mhs + fhs + adjinc + wtest, data = emp)
summary(m_emp)

# Null
x1 <- deviance(multinom(empl~1, data=emp))
x1

# Full
x2 <- deviance(m_emp)
x2

# Overall
x3 <- deviance(multinom(empl~1, data=emp)) - deviance(m_emp)
x3
pchisq(x3, 8, lower.tail=FALSE)

# Model fit:
library(DescTools)
PseudoR2(m_emp, "Nagelkerke")
 
# Individual paramerters
z <- summary(m_emp)$coefficients/summary(m_emp)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1))*2
round(p, 6)

# Odds-ratios
exp(coef(m_emp))

# Classification:
# Bring in new data:
test_emp <- read.csv("data/employmentTest.csv", stringsAsFactors = FALSE)
test_emp$mhs <- factor(test_emp$mhs, levels=c(0,1), labels=c("No", "Yes"))
test_emp$fhs <- factor(test_emp$fhs, levels=c(0,1), labels=c("No", "Yes"))
test_emp$empl <- factor(test_emp$empl, levels=c(0,1,2), labels=c("Not Employed", "Working", "In School"))
table(test_emp$mhs)
table(test_emp$fhs)
table(test_emp$empl)

test_emp <- data.frame(test_emp, empl_mod = predict(m_emp, test_emp))
addmargins(table(test_emp$empl, test_emp$empl_mod, dnn=c("Obs", "Predicted")))

(7+40+47) / 196 * 100

#Graphing (Original)
library(lsmeans)
summary(emp)
test_mns <- summary(lsmeans(m_emp, c("empl", "wtest"),
                             at=list(wtest=seq(-2,2))))
test_mns

ggplot(test_mns, aes(y=prob, x=wtest, color=empl)) +
  geom_line() +
  labs(title="Aptitude impact") +
  scale_x_continuous(breaks=c(-2:2), labels = c(-2:2), minor_breaks = NULL) +
  theme_bw()