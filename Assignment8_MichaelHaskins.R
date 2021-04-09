Dat <- read.table( file="med_care_demand.txt", header=T, sep="\t" )
fit <- glm( gp_visits ~ . , data=Dat, family=poisson )

dim(Dat)
require(ggplot2)
#install.packages("sandwich")
require(sandwich)
#install.packages("msm")
require(msm)

summary(Dat)
var(Dat$hosp_stays)
var(Dat$number_chronic_diseases)
var(Dat$school)

summary(fit)

cov.fit <- vcovHC(fit, type="HC0")
std.err <- sqrt(diag(cov.fit))
r.est <- cbind(Estimate= coef(fit), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(fit)/std.err), lower.tail=FALSE),
               LL = coef(fit) - 1.96 * std.err,
               UL = coef(fit) + 1.96 * std.err)
r.est


with(fit, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))



Dat$phat <- predict(fit, type="response")

## order by program and then by math
Dat <- Dat[with(Dat, order(health, hosp_stays)), ]

## create the plot
ggplot(Dat, aes(x = health, y = phat, colour = hosp_stays)) +
  geom_point(aes(y = gp_visits), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  labs(title = "Number of GP visits vs Health Classification",x = "Self health classification", y = "Expected number of GP visits") +
  theme(text = element_text(size=20))


