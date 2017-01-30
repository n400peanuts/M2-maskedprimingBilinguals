# ---- Analysis of bilingual masked priming
# ---- 12/01/2017
# to use after f.diagnostics and pre-processing

#---------------------------------------------------------------------------------------------------#
#                                                    ITA                                            #
#---------------------------------------------------------------------------------------------------#
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
italmer5 <- lmer(-1000/rt ~ as.factor(Relatedness) * Morphtype + Logfreq.Zipf.t + (1|Subject) + (1|Target), data= datartITA, REML = F)
anova(italmer4, italmer5)
summary(italmer5)
#5b
italmer5b <- lmer(-1000/rt ~ as.factor(Relatedness) * Morphtype + Logfreq.Zipf.t + (1|Subject) + (1|Target), data= subset(datartITA, abs(scale(resid(italmer5)))<2), REML = F)
summary(italmer5b)
anova(italmer5b)

library(languageR)
inv <- function(x) { -1000/x}
plotLMER.fnc(italmer5b, fun = inv, pred = "as.factor(Relatedness)", intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T)

#---------------------------------------------------------------------------------------------------#
#                                                    ENG                                            #
#---------------------------------------------------------------------------------------------------#
englmer1 <- lmer(-1000/rt ~ TrialCount + Rotation + (1|Subject) + (1|Target), data= datartENG, REML = F)
summary(englmer1)
#2
library(rms)
englmer2 <- lmer(-1000/rt ~ rcs(TrialCount) + (1|Subject) + (1|Target), data= datartENG, REML = F)
anova(englmer1, englmer2)
summary(englmer2)
anova(englmer2)
#3
englmer3 <- lmer(-1000/rt ~ rcs(TrialCount) + Logfreq.Zipf.t + Lent + Nt + (1|Subject) + (1|Target), data= datartENG, REML = F)
anova(englmer2, englmer3)
summary(englmer3)
#4
englmer4 <- lmer(-1000/rt ~ rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data= datartENG, REML = F)
anova(englmer3, englmer4)
#5
datartENG$Morphtype <- relevel(datartENG$Morphtype, "OR")
englmer5 <- lmer(-1000/rt ~ as.factor(Relatedness) * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent +(1|Subject) + (1|Target), data= datartENG, REML = F)
anova(englmer5, englmer4)
summary(englmer5)
#5b
englmer5b <- lmer(-1000/rt ~ as.factor(Relatedness) * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data= subset(datartENG, abs(scale(resid(englmer5)))<2), REML = F)
summary(englmer5b)
anova(englmer5b)

plotLMER.fnc(englmer5b, fun = inv, pred = "as.factor(Relatedness)", intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T)

#---------------------------------------------------------------------------------------------------#
#                                     Cross-experiment interaction                                  #
#   in order to be completely sure that pattern of results in ITA and ENG differ,                   #
#   we need to have a three-way interaction here btw Relatedness, MorphType and Language            #
#---------------------------------------------------------------------------------------------------#
rbind(datartENG, datartITA) -> crossExp
languagelmer1 <- lmer(-1000/rt ~ as.factor(Relatedness) * Morphtype * Language + rcs(TrialCount) + Lent + (1|Subject) + (1|Target), data = crossExp, REML = F)
summary(languagelmer1)
anova(languagelmer1)

#---------------------------------------------------------------------------------------------------#
#                                     Language proficiency analysis                                 #
#---------------------------------------------------------------------------------------------------#

#first take a look at variables distributions
proficiencyData <- datartENG[,c('Subject','Age','Gender','Handedness','Rotation','phoneticFluency', 'phoneticComprehension','morphComprehension','spelling','readingComprehension','vocabulary','oralComprehension','AoA1', 'AoA2', 'AoA3','AoA4','AoA5','AoA6','AoA7','AoA8','AoA9')];
proficiencyData <- unique(proficiencyData);
summary(proficiencyData)

hist(proficiencyData$phoneticFluency, breaks = seq(0,50,5)) 
hist(proficiencyData$phoneticComprehension, breaks = seq(0,15,1)) #capacità di discriminazione fonologica
hist(proficiencyData$morphComprehension, breaks = seq(0,10,1)) 
hist(proficiencyData$spelling, breaks = seq(0,20,2)) 
hist(proficiencyData$readingComprehension, breaks = seq(0,7,1)) 
hist(proficiencyData$vocabulary, breaks = seq(0,20,2)) 
hist(proficiencyData$oralComprehension, breaks = seq(0,6,1)) 

#correlation
round(cor(proficiencyData[,6:12]), digits = 2)
collin.fnc(proficiencyData[,6:12]) #see baayen clustering, condition number K
library(corrplot)
corrplot(cor(proficiencyData[,6:12]), type = "lower", order = "hclust", diag = T, method = "circle", outline = T, addgrid.col = F, tl.col = "black", tl.pos = "n")
corrplot(cor(proficiencyData[,6:12]), order = "hclust")

#tentative of clustering
plot(varclus(as.matrix(proficiencyData[,6:12])))

#mixed-models of Proficiency with one variable at time 
#I would start with the overall dataset of crossExp
proficiencylmer <- lmer(-1000/rt ~ as.factor(Relatedness) * phoneticFluency*Language + Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = crossExp)
summary(proficiencylmer1)
anova(proficiencylmer1)
#nothing emerged, so only english dataset:
#phoneticFluency
proficiencylmer1 <- lmer(-1000/rt ~ as.factor(Relatedness) * phoneticFluency + Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
summary(proficiencylmer1)
anova(proficiencylmer1)
#phoneticComprehension
proficiencylmer2 <- lmer(-1000/rt ~ as.factor(Relatedness) * phoneticComprehension + Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
summary(proficiencylmer2) #quasi...
anova(proficiencylmer2)
#morphComprehension
proficiencylmer3 <- lmer(-1000/rt ~ as.factor(Relatedness) * morphComprehension + Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
summary(proficiencylmer3) 
anova(proficiencylmer3)
#spelling
proficiencylmer4 <- lmer(-1000/rt ~ as.factor(Relatedness) * spelling + Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
summary(proficiencylmer4) 
anova(proficiencylmer4)
#readingComprehension
proficiencylmer5 <- lmer(-1000/rt ~ as.factor(Relatedness) * readingComprehension + Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
summary(proficiencylmer5) 
anova(proficiencylmer5)
#vocabulary
proficiencylmer6 <- lmer(-1000/rt ~ as.factor(Relatedness) * vocabulary + Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
proficiencylmer6 <- lmer(-1000/rt ~ as.factor(Relatedness) + vocabulary * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
summary(proficiencylmer6) #vocabulary*relatedness significativo. seconda analisi significativa con i TR.
anova(proficiencylmer6)

#oralComprehension
proficiencylmer7 <- lmer(-1000/rt ~ as.factor(Relatedness) * oralComprehension + Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
summary(proficiencylmer7) 
anova(proficiencylmer7)

#Second tentative of clustering using principal component analysis UNSUPERVISED
prof.pr <- prcomp(data.frame(datartENG[, c("rt","phoneticComprehension", "morphComprehension", "spelling", "readingComprehension", "vocabulary", "oralComprehension")], row.names = NULL))
summary(prof.pr)
props <- round((prof.pr$sdev^2/sum(prof.pr$sdev^2)), 3)
barplot(props, col = as.numeric(props > 0.5), xlab = "principal components", ylab = "proportion of variance explained")
abline(h = 0.05)
