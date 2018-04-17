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

# Overall
x2 <- deviance(multinom(empl~1, data=emp)) - deviance(m_emp)
x2
pchisq(x2, 8, lower.tail=FALSE)

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
# 
# #Change the baseline:
# buy$Purchase_Leave <- factor(buy$Purchase, 
#                              levels=c("Leave Item", "Buy Item", "Wish List"))
# 
# #Model_Leave
# m_buy_l <- multinom(Purchase_Leave ~ Usefulness + Packaging + 
#                       Price + Gender, data = buy)
# summary(m_buy_l)
# 
# #Individual paramerters_Leave
# z <- summary(m_buy_l)$coefficients/summary(m_buy_l)$standard.errors
# z
# p <- (1 - pnorm(abs(z), 0, 1))*2
# round(p, 6)
# 
# #Odds-ratios_Leave
# exp(coef(m_buy_l))

# Classification:
# Bring in new data:
test_emp <- read.csv("data/employmentTest.csv", stringsAsFactors = FALSE)
test_emp$mhs <- factor(test_emp$mhs, levels=c(0,1), labels=c("No", "Yes"))
test_emp$fhs <- factor(test_emp$fhs, levels=c(0,1), labels=c("No", "Yes"))
test_emp$empl <- factor(test_emp$empl, levels=c(0,1,2), labels=c("Not Employed", "Working", "In School"))
table(test_emp$mhs)
table(test_emp$fhs)
table(test_emp$empl)

test_buy <- data.frame(test_buy, purch_mod = predict(m_buy, test_buy))
addmargins(table(test_buy$Purchase, test_buy$purch_mod, dnn=c("Obs", "Predicted")))

# (2+35+73) / 204
# max(c(24/204), (82/204), (98/204))
# 
# #Graphing (Original)
# library(lsmeans)
# summary(buy)
# use_mns <- summary(lsmeans(m_buy, c("Purchase", "Usefulness"), 
#                            at=list(Usefulness=seq(0,10,1))))
# 
# ggplot(use_mns, aes(y=prob, x=Usefulness, color=Purchase)) +
#   geom_line() +
#   labs(title="Usefulness impact") +
#   scale_x_continuous(breaks=c(1:10), labels = c(1:10), minor_breaks = NULL) +
#   theme_bw()
# 
# hist(buy$Usefulness)
# 
# price_mns <- summary(lsmeans(m_buy, c("Purchase", "Price"), 
#                              at=list(Price=seq(0,10,1))))
# 
# ggplot(price_mns, aes(y=prob, x=Price, color=Purchase)) +
#   geom_line() +
#   labs(title="Price impact") +
#   theme_bw()
