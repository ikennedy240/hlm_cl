
#' ---
#' title: "hetChecks.R -- code for evaluating heteroskedasticity" 
#' author: "Kaylea Champion"
#' ---

##Data prep for heteroskedasticity checks
library(olsrr)
library("car")
lt_data <- data #copy


lt_data$l_Topic7 <- log(lt_data$Topic7)
lt_data$l_Topic18 <- log(lt_data$Topic18)
lt_data$l_Topic20 <- log(lt_data$Topic20)
lt_data$l_Topic25 <- log(lt_data$Topic25)
lt_data$l_Topic34 <- log(lt_data$Topic34)

png('qqTopic7.png')
qqPlot(lt_data$Topic7)
dev.off()

png('qqLTopic7.png')
qqPlot(lt_data$l_Topic7)
dev.off()

#7 looks better

png('qqTopic18.png')
qqPlot(lt_data$Topic18)
dev.off()

png('qqLTopic18.png')
qqPlot(lt_data$l_Topic18)
dev.off()

# 18 looks better

png('qqTopic20.png')
qqPlot(lt_data$Topic20)
dev.off()

png('qqLTopic20.png')
qqPlot(lt_data$l_Topic20)
dev.off()

#20 looks a little better but still troubled

png('qqTopic25.png')
qqPlot(lt_data$Topic25)
dev.off()

png('qqLTopic25.png')
qqPlot(lt_data$l_Topic25)
dev.off()

#25 looks better

png('qqTopic34.png')
qqPlot(lt_data$Topic34)
dev.off()

png('qqLTopic34.png')
qqPlot(lt_data$l_Topic34)
dev.off()

#34 looks better

##

##Output some Racial QQs
png('qqWhite.png')
mq <- qqPlot(lt_data$white_proportion)
dev.off()

png('qqBlack.png')
qqPlot(lt_data$black_proportion)
dev.off()

png('qqLatinx.png')
qqPlot(lt_data$latinx_proportion)
dev.off()

png('qqAsian.png')
qqPlot(lt_data$asian_proportion)
dev.off()

model7nl <- lm(Topic7 ~ white_proportion + black_proportion + asian_proportion, data=lt_data)
ols_test_f(model7nl, data=lt_data, vars=c("white_proportion","black_proportion", "asian_proportion"))

model18nl <- lm(Topic18 ~ white_proportion + black_proportion + asian_proportion, data=lt_data)
ols_test_f(model18nl, data=lt_data, vars=c("white_proportion","black_proportion", "asian_proportion"))

model20nl <- lm(Topic20 ~ white_proportion + black_proportion + asian_proportion, data=lt_data)
ols_test_f(model20nl, data=lt_data, vars=c("white_proportion","black_proportion", "asian_proportion"))

model25nl <- lm(Topic25 ~ white_proportion + black_proportion + asian_proportion, data=lt_data)
ols_test_f(model25nl, data=lt_data, vars=c("white_proportion","black_proportion", "asian_proportion"))

model34nl <- lm(Topic34 ~ white_proportion + black_proportion + asian_proportion, data=lt_data)
ols_test_f(model34nl, data=lt_data, vars=c("white_proportion","black_proportion", "asian_proportion"))

ols_test_score(model7nl)
ols_test_score(model18nl)
ols_test_score(model20nl)
ols_test_score(model25nl)
ols_test_score(model34nl)


model7_isl <- lm(l_Topic7 ~ white_proportion + black_proportion + asian_proportion, data=lt_data)
ols_test_f(model7_isl, data=lt_data, vars=c("white_proportion","black_proportion", "asian_proportion"))

model18_isl <- lm(l_Topic18 ~ white_proportion + black_proportion + asian_proportion, data=lt_data)
ols_test_f(model18_isl, data=lt_data, vars=c("white_proportion","black_proportion", "asian_proportion"))

model20_isl <- lm(l_Topic20 ~ white_proportion + black_proportion + asian_proportion, data=lt_data)
ols_test_f(model20_isl, data=lt_data, vars=c("white_proportion","black_proportion", "asian_proportion"))

model25_isl <- lm(l_Topic25 ~ white_proportion + black_proportion + asian_proportion, data=lt_data)
ols_test_f(model25_isl, data=lt_data, vars=c("white_proportion","black_proportion", "asian_proportion"))

model34_isl <- lm(l_Topic34 ~ white_proportion + black_proportion + asian_proportion, data=lt_data)
ols_test_f(model34_isl, data=lt_data, vars=c("white_proportion","black_proportion", "asian_proportion"))

ols_test_score(model7_isl)
ols_test_score(model18_isl)
ols_test_score(model20_isl)
ols_test_score(model25_isl)
ols_test_score(model34_isl)

#Finding: racial proportions appear to be heteroskedastic within topics


plot(model7nl)
plot(model7_isl)

plot(model18nl)
plot(model18_isl)

plot(model20nl)
plot(model20_isl)

plot(model25nl)
plot(model25_isl)

plot(model34nl)
plot(model34_isl)



plot(msimple_7)
plot(mlin_7)
plot(m3_7)

plot(msimple_18)
plot(mlin_18)
plot(m3_18)

plot(msimple_20)
plot(mlin_20)
plot(m3_20)

plot(msimple_25)
plot(mlin_25)
plot(m3_25)

plot(msimple_34)
plot(mlin_34)
plot(m3_34)


qqnorm(residuals(msimple_7)); qqline(residuals(msimple_7), col=2)
qqnorm(residuals(mlin_7)); qqline(residuals(mlin_7), col=2)
qqnorm(residuals(m3_7));  qqline(residuals(m3_7), col=2)

qqnorm(residuals(msimple_18)); qqline(residuals(msimple_18), col=2)
qqnorm(residuals(mlin_18)); qqline(residuals(mlin_18), col=2)
qqnorm(residuals(m3_18)); qqline(residuals(m3_18), col=2)

qqnorm(residuals(msimple_20)); qqline(residuals(msimple_20), col=2)
qqnorm(residuals(mlin_20)); qqline(residuals(mlin_20), col=2)
qqnorm(residuals(m3_20)); qqline(residuals(m3_20), col=2)

qqnorm(residuals(msimple_25)); qqline(residuals(msimple_25), col=2)
qqnorm(residuals(mlin_25)); qqline(residuals(mlin_25), col=2)
qqnorm(residuals(m3_25)); qqline(residuals(m3_25), col=2)

qqnorm(residuals(msimple_34)); qqline(residuals(msimple_34), col=2)
qqnorm(residuals(mlin_34)); qqline(residuals(mlin_34), col=2)
qqnorm(residuals(m3_34)); qqline(residuals(m3_34), col=2)

qqnorm(residuals(msimple_7))
qqnorm(residuals(mlin_7))
qqnorm(residuals(m3_7))

qqnorm(residuals(msimple_18))
qqnorm(residuals(mlin_18))
qqnorm(residuals(m3_18))

qqnorm(residuals(msimple_20))
qqnorm(residuals(mlin_20))
qqnorm(residuals(m3_20))

qqnorm(residuals(msimple_25))
qqnorm(residuals(mlin_25))
qqnorm(residuals(m3_25))

qqnorm(residuals(msimple_34))
qqnorm(residuals(mlin_34))
qqnorm(residuals(m3_34))

library(ggplot2)
library(gridExtra)

a7 <- qplot(residuals(msimple_7), bins=100, xlab = "Residuals") + ggtitle("Base Linear Model")
b7 <- qplot(residuals(mlin_7), bins=100, xlab="Residuals") + ggtitle("Linear M. with Controls")
c7 <- qplot(residuals(m3_7), bins=100, xlab="Residuals") + ggtitle("Hierarchical Model")

#resid7.png
grid.arrange(a7, b7, c7, nrow=1)

a18 <- qplot(residuals(msimple_18), bins=100)
b18<- qplot(residuals(mlin_18), bins=100)
c18<- qplot(residuals(m3_18), bins=100)

grid.arrange(a18, b18, c18, nrow=1)

a20<- qplot(residuals(msimple_20), bins=100, xlab="Residuals") + ggtitle("Base Linear Model")
b20<- qplot(residuals(mlin_20), bins=100, xlab="Residuals") + ggtitle("Linear M. with Controls")
c20<- qplot(residuals(m3_20), bins=100, xlab="Residuals") + ggtitle("Hierarchical Model")

#resid20.png
grid.arrange(a20, b20, c20, nrow=1)

a25 <- qplot(residuals(msimple_25), bins=100)
b25 <- qplot(residuals(mlin_25), bins=100)
c25 <- qplot(residuals(m3_25), bins=100)

resid25 <- grid.arrange(a25, b25, c25, nrow=1)

a34 <- qplot(residuals(msimple_34), bins=100)
b34 <- qplot(residuals(mlin_34), bins=100)
c34 <- qplot(residuals(m3_34), bins=100)

resid34 <- grid.arrange(a34, b34, c34, nrow=1)
