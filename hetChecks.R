
#5 topics, predict in bivariate (composite racial proportions) and multivariable
#run an LM and a GLM of each of these, change 'model type' from lm to lmer (passes literal) and set a grouping_var
#- update to most recent version, use estimate effects function to run models
#  --> focal covariates will be dots in the plot
#-black, asian, multi
#-all covariates (proportion versions when available)

#---scale vs don't scale them
#---look at choice of logit vs linear

#--check skedasticity of which vars on which scale

#=====check the spread versus white, black, asian====

#unlogged version of topic proportions

#logged topic proportions

##Data prep for heteroskedasticity checks
library(olsrr)
library("car")
lt_data <- data #copy


lt_data$l_Topic7 <- log(lt_data$Topic7)
lt_data$l_Topic18 <- log(lt_data$Topic18)
lt_data$l_Topic20 <- log(lt_data$Topic20)
lt_data$l_Topic25 <- log(lt_data$Topic25)
lt_data$l_Topic34 <- log(lt_data$Topic34)
##

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

##Some plots of logged version
png('qqLogWhite.png')
qqPlot(log(lt_data$white_proportion))
dev.off()

png('qqLogBlack.png')
qqPlot(log1p(lt_data$black_proportion))
dev.off()

png('qqLogLatinx.png')
qqPlot(log(lt_data$latinx_proportion))
dev.off()

png('qqLogAsian.png')
qqPlot(log(lt_data$asian_proportion))
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
ols_test_f(model7, data=lt_data, vars=c("white_proportion","black_proportion", "asian_proportion"))

model18_isl <- lm(l_Topic18 ~ white_proportion + black_proportion + asian_proportion, data=lt_data)
ols_test_f(model18, data=lt_data, vars=c("white_proportion","black_proportion", "asian_proportion"))

model20_isl <- lm(l_Topic20 ~ white_proportion + black_proportion + asian_proportion, data=lt_data)
ols_test_f(model20, data=lt_data, vars=c("white_proportion","black_proportion", "asian_proportion"))

model25_isl <- lm(l_Topic25 ~ white_proportion + black_proportion + asian_proportion, data=lt_data)
ols_test_f(model25, data=lt_data, vars=c("white_proportion","black_proportion", "asian_proportion"))

model34_isl <- lm(l_Topic34 ~ white_proportion + black_proportion + asian_proportion, data=lt_data)
ols_test_f(model34, data=lt_data, vars=c("white_proportion","black_proportion", "asian_proportion"))

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

