# ---- Analysis of bilingual masked priming
# ---- 12/01/2017
# to use after f.diagnostics

#ITA
attach(datartITA)
par(mfrow=c(1,3))
qqnorm(rt)
qqnorm(log(rt))
qqnorm(-1000/rt)
par(mfrow=c(1,1))
detach(datartITA)

library(lmerTest)
italmer1 <- lmer(-1000/rt ~ TrialCount + Rotation + (1|Subject) + (1|Target), data= datartITA, REML = F)
summary(italmer1)
#2
library(rms)
italmer2 <- lmer(-1000/rt ~ rcs(TrialCount) + (1|Subject) + (1|Target), data= datartITA, REML = F)
anova(italmer1, italmer2)
summary(italmer2)
#3
italmer3 <- lmer(-1000/rt ~ Logfreq.Zipf.t + Lent + Nt + (1|Subject) + (1|Target), data= datartITA, REML = F)
summary(italmer3)
#4
italmer4 <- lmer(-1000/rt ~ Logfreq.Zipf.t + (1|Subject) + (1|Target), data= datartITA, REML = F)
anova(italmer3, italmer4)
#5
datartITA$Morphtype <- relevel(datartITA$Morphtype, "OR")
italmer5 <- lmer(-1000/rt ~ Relatedness * Morphtype + Logfreq.Zipf.t + (1|Subject) + (1|Target), data= datartITA, REML = F)
anova(italmer4, italmer5)
summary(italmer5)

#5b
italmer5b <- lmer(-1000/rt ~ Relatedness * Morphtype + Logfreq.Zipf.t + (1|Subject) + (1|Target), data= subset(datartITA, abs(scale(resid(italmer5)))<2), REML = F)
summary(italmer5b)
anova(italmer5b)

library(languageR)
inv <- function(x) { -1000/x}
plotLMER.fnc(italmer5b, fun = inv, pred = "Relatedness", intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T)

#ENG
englmer1 <- lmer(-1000/rt ~ TrialCount + Rotation + (1|Subject) + (1|Target), data= datartENG, REML = F)
summary(englmer1)
#2
library(rms)
englmer2 <- lmer(-1000/rt ~ rcs(TrialCount) + (1|Subject) + (1|Target), data= datartENG, REML = F)
anova(englmer1, englmer2)
summary(englmer2)
#3
englmer3 <- lmer(-1000/rt ~ Logfreq.Zipf.t + Lent + Nt + (1|Subject) + (1|Target), data= datartENG, REML = F)
summary(englmer3)
#4
englmer4 <- lmer(-1000/rt ~ Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data= datartENG, REML = F)
anova(englmer3, englmer4)
#5
englmer5 <- lmer(-1000/rt ~ Logfreq.Zipf.t + (1|Subject) + (1|Target), data= datartENG, REML = F)
anova(englmer4, englmer5)
#6
datartENG$Morphtype <- relevel(datartENG$Morphtype, "OR")
englmer6 <- lmer(-1000/rt ~ Relatedness * Morphtype + Logfreq.Zipf.t + Lent +(1|Subject) + (1|Target), data= datartENG, REML = F)
anova(englmer6, englmer4)
summary(englmer6)
#6b
englmer6b <- lmer(-1000/rt ~ Relatedness * Morphtype + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data= subset(datartENG, abs(scale(resid(englmer6)))<2), REML = F)
summary(englmer6b)
anova(englmer6b)

plotLMER.fnc(englmer6b, fun = inv, pred = "Relatedness", intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T)

#cross-experiment interaction -- in order to be completely sure that pattern of results in ITA and ENG differ, we need to have a three-way interaction here btw Relatedness, MorphType and Language (no doubt we'll get it, btw). In order to do this analysis, you need to merge ITA and ENG data. Pay attention that columns need to correspond in the two data frame (I'd only select the obvious covariates, i.e., trial count, rotation, language block order (in case we counterbalanced this), and the target features; and leave out all the rest).

#################################
# language proficiency analysis #
#################################
#first take a look at variables distributions
proficiencyData <- datartENG[,c('Subject','Age','Gender','Handedness','Rotation','phoneticFluency','morphComprehension','spelling','readingComprehension','vocabulary','oralComprehension','AoA1', 'AoA2', 'AoA3','AoA4','AoA5','AoA6','AoA7','AoA8','AoA9')];
proficiencyData <- unique(proficiencyData);



