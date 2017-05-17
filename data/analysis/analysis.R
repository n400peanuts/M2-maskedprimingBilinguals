# ---- Analysis of bilingual masked priming
# ---- 12/01/2017
# to use after pre-processing.R

#---- Diagnostics.Rfunction of Davide Crepaldi (see: http://www.davidecrepaldi.net/wordpress/software-utilities-2/)
#---------------------------------------------------------------------------------------------------#
#                                         ITA f.Diagnostics                                         #
#---------------------------------------------------------------------------------------------------#
library(languageR)
library(lmerTest)
library(ggplot2)
library(rms)
library(doBy)
library(mgcv)
inv <- function(x) { -1000/x}

subset(masterFile, Language=="ita")-> masterfileIta

sbj.id<- masterfileIta$Subject
acc<- masterfileIta$Accuracy
lexicality<- tolower(masterfileIta$Lexicality)
target<- masterfileIta$Target
rt<- masterfileIta$rt

source("Diagnostics.R") #I've uploaded this function into the git folder, so that everyone can import it into R, and use it. 
diagnostics.f(rt = rt, acc = acc, sbj.id = sbj.id, target = target, lexicality = lexicality, "ita")

#Subj. 16 has reported seeing clearly the prime, for this reason will be taken out of the analysis, in addition to the words that scored less than 70% of accuracy. 
# We filter also Rts from 250 to 1600ms
subset(target.diagnostics, acc<0.7)-> parolebrutte #.giusto per sapere quali sono
subset(masterfileIta, masterfileIta$rt>250 & masterfileIta$rt<1600 & masterfileIta$Subject!=16 & masterfileIta$Target!= "congruo" & masterfileIta$Target!= "guado" & masterfileIta$Target!= "guano" & masterfileIta$Target!= "uggia" & masterfileIta$Target!= "vello" & masterfileIta$Target!= "avo" & masterfileIta$Lexicality=="WORD") -> dataAccITA
#Then, we select only right answers
subset(dataAccITA, dataAccITA$Accuracy==1)-> datartITA
summary(datartITA)

#First look at the means
round(xtabs(datartITA$rt ~ datartITA$Morphtype + datartITA$Primetype) / xtabs(~datartITA$Morphtype + datartITA$Primetype), digits = 0)

#---------------------------------------------------------------------------------------------------#
#                                         ENG f.Diagnostics                                         #
#---------------------------------------------------------------------------------------------------#
subset(masterFile, Language=="eng")-> masterfileEng


sbj.id<- masterfileEng$Subject
acc<- masterfileEng$Accuracy
lexicality<- masterfileEng$Lexicality
lexicality<- tolower(masterfileEng$Lexicality)
target<-masterfileEng$Target
rt<-masterfileEng$rt

diagnostics.f(rt = rt, acc = acc, sbj.id = sbj.id, target = target, lexicality = lexicality, "eng")

#Subjs 16 saw the prime. Sbjs 22 and 26 will be taken out because their performance is very far from the others and the accuracy less than 40%, see the output graph from the diagnostics function.
# We filter also Rts from 250 to 1900ms
subset(masterfileEng, masterfileEng$rt>250 & masterfileEng$rt<1900 & masterfileEng$Subject!=16 & masterfileEng$Subject!=22 & masterfileEng$Subject!=26 & masterfileEng$Lexicality=="WORD") -> dataAccENG
#Then, we select only right answers
subset(dataAccENG, dataAccENG$Accuracy==1)-> datartENG
#First look at the means
round(xtabs(datartENG$rt ~ datartENG$Morphtype + datartENG$Primetype) / xtabs(~datartENG$Morphtype + datartENG$Primetype), digits = 0)

#clean up the workspace
rm(rt, acc, sbj.id, target, lexicality, masterfileIta, masterfileEng);

#---------------------------------------------------------------------------------------------------#
#                                         d primes analysis                                         #
#---------------------------------------------------------------------------------------------------#
masterfileEng <- subset(masterFile, masterFile$Language=='eng')
xtabs( ~ masterfileEng$Resp + masterfileEng$Subject) #look at response given by sbjs

#I decided to remove the no responses from the subset
masterfileEng <- subset(masterfileEng, masterfileEng$Resp != 0)


sbj.numbers <- subset(masterfileEng, masterfileEng$Subject!= "16" & masterfileEng$Subject!= "22" & masterfileEng$Subject!= "26") #remove sbj 16 as his d' value is NAN
sbj.numbers <- unique(sbj.numbers$Subject)

dprimes <- vector(length=length(sbj.numbers), mode="integer");
subject <- vector(length=length(sbj.numbers), mode = "character")

counter<-1;

for (s in sbj.numbers)
  
{ 
  #make a 2 by 2 table with proportion of hits, miss, false alarms, correct rejections
  table <- (xtabs(~ masterfileEng$Accuracy[masterfileEng$Subject == s] + 
                    masterfileEng$Resp[masterfileEng$Subject == s])) /
    (length(masterfileEng$Accuracy[masterfileEng$Subject == s])/2) # tot number of yes trials per subject which is the same number of "no" trials  
  #calculate dprime as z(hits) - z(FA)
  dprime <- round(qnorm(table[2,1]) - qnorm(table[1,1]), digits = 3)
  
  #add dprime value for this subject to the dprime vector, in the "n" position
  dprimes[counter] <- dprime
  subject[counter] <- as.character(s)
  
  # update counter
  counter <- counter+1;
};

bySs_dprimes <- data.frame(subject, dprimes)
mean_dprime <- mean(bySs_dprimes$dprimes)

# check if d' value differ from 0
t.test(bySs_dprimes$dprimes, mu= 0)

colnames(bySs_dprimes) <- c('Subject','dprimes')
merge(bySs_dprimes, masterfileEng, by='Subject', all.x = T)-> masterfileEng
summary(masterfileEng)

rm(counter, dprimes, subject, s, sbj.numbers, table, dprime, mean_dprime)
#---------------------------------------------------------------------------------------------------#
#                                                    END                                            #
#---------------------------------------------------------------------------------------------------#
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
datartITA$Relatedness <- as.factor(datartITA$Relatedness)
datartITA$Morphtype <- relevel(datartITA$Morphtype, "OP")

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
italmer5 <- lmer(-1000/rt ~ Relatedness * Morphtype + Logfreq.Zipf.t + (1|Subject) + (1|Target), data= datartITA, REML = F)
anova(italmer4, italmer5)
summary(italmer5)
#5b
italmer5b <- lmer(-1000/rt ~ Relatedness * Morphtype + Logfreq.Zipf.t + (1|Subject) + (1|Target), data= subset(datartITA, abs(scale(resid(italmer5)))<2), REML = F)
summary(italmer5b)
anova(italmer5b)

aa<-plotLMER.fnc(italmer5b, fun = inv, withList = TRUE, pred = "Relatedness", intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, xlabel ="Relatedness" , ylabel = "-1000/rt", main = "ITA")

df <- do.call(rbind, aa$Relatedness)
names(df)[names(df) == "Levels"] <- "Relatedness"
df$Morphtype <- rep(c("OR", "OP", "TR"), each =2)
# plot using ggplot
limitsforgraph <- summaryBy(rt ~ Morphtype + Relatedness, data=subset(datartITA, abs(scale(resid(italmer5)))<2), FUN=c(length,mean,sd))
names(limitsforgraph)[names(limitsforgraph)=="rt.length"] <- "N"
limitsforgraph$se <- limitsforgraph$rt.sd / sqrt(limitsforgraph$N)
limitsforgraph$mean <- df$Y

dodge <- position_dodge(width = 0.1)
bb  <-ggplot(data = df, aes(x = Relatedness, y = Y, col = Morphtype ,group = Morphtype)) + scale_colour_manual(breaks = c("OR", "OP", "TR"), values = c("#0000e8", "#000000", "#ff0030")) + geom_point(position = dodge, size = 4.5, shape=21, fill="white") + geom_line(position = dodge)+ theme_classic()
bb  <- bb + scale_y_continuous("RT(ms)",limits=c(480,650))
bb  <- bb + geom_errorbar(aes(ymin = limitsforgraph$mean - limitsforgraph$se, ymax = limitsforgraph$mean + limitsforgraph$se), width=0.1, size=1, linetype=1, position = dodge) #FUCK YEAH
bb  <- bb + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))
bb  <- bb + theme(axis.text.y = element_text(angle = 00, hjust = 1, size=13, face = 'bold', colour = 'black'))
bb  <- bb + labs(x = "UNRELATED       RELATED ")
bb  <- bb + theme(axis.title.x = element_text(size = rel(1), face = 'bold'))
bb
ggsave("itaplot.jpg", height=4, width=5, dpi = 1000)

#---------------------------------------------------------------------------------------------------#
#                                                    ENG                                            #
#---------------------------------------------------------------------------------------------------#
datartENG$Relatedness <- as.factor(datartENG$Relatedness)
datartENG$Morphtype <- relevel(datartENG$Morphtype, "OP")
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
datartENG$Relatedness <- as.factor(datartENG$Relatedness)
englmer5 <- lmer(-1000/rt ~ Relatedness * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent +(1|Subject) + (1|Target), data= datartENG, REML = F)
anova(englmer5, englmer4)
summary(englmer5)
#5b
englmer5b <- lmer(-1000/rt ~ Relatedness * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data= subset(datartENG, abs(scale(resid(englmer5)))<2), REML = F)
summary(englmer5b)
anova(englmer5b)

aa<- plotLMER.fnc(englmer5b, fun = inv, withList = TRUE, pred = "Relatedness", intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, xlabel ="Relatedness" , ylabel = "-1000/rt", main = "ENG")

df <- do.call(rbind, aa$Relatedness)
names(df)[names(df) == "Levels"] <- "Relatedness"
df$Morphtype <- rep(c("OR", "OP", "TR"), each =2)
# plot using ggplot
limitsforgraph <- summaryBy(rt ~ Morphtype + Relatedness, data=subset(datartENG, abs(scale(resid(englmer5b)))<2), FUN=c(length,mean,sd))
names(limitsforgraph)[names(limitsforgraph)=="rt.length"] <- "N"
limitsforgraph$se <- limitsforgraph$rt.sd / sqrt(limitsforgraph$N)
limitsforgraph$mean <- df$Y

dodge <- position_dodge(width = 0.1)
gg  <- ggplot(data = df, aes(x = Relatedness, y = Y, col = Morphtype ,group = Morphtype)) + scale_colour_manual(breaks = c("OR", "OP", "TR"), values = c("#0000e8", "#000000", "#ff0030")) + geom_point(position = dodge, size = 4.5, shape=21, fill="white") + geom_line(position = dodge)+ theme_classic()
gg  <- gg + scale_y_continuous("RT(ms)",limits=c(480,650))
gg  <- gg + geom_errorbar(aes(ymin = limitsforgraph$mean - limitsforgraph$se, ymax = limitsforgraph$mean + limitsforgraph$se), width=0.1, size=1, linetype=1, position = dodge) #FUCK YEAH
gg  <- gg + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))
gg  <- gg + theme(axis.text.y = element_text(angle = 00, hjust = 1, size=13, face = 'bold', colour = 'black'))
gg  <- gg + labs(x = "UNRELATED       RELATED ")
gg  <- gg + theme(axis.title.x = element_text(size = rel(1), face = 'bold'))
gg
ggsave("engplot.jpg", height=4, width=5, dpi = 2000)
rm(gg, bb, aa, dodge)
#---------------------------------------------------------------------------------------------------#
#                                     Cross-experiment interaction                                  #
#   in order to be completely sure that pattern of results in ITA and ENG differ,                   #
#   we need to have a three-way interaction here btw Relatedness, MorphType and Language            #
#---------------------------------------------------------------------------------------------------#
rbind(datartENG[,1:43], datartITA) -> crossExp
crossExp$Relatedness <- as.factor(crossExp$Relatedness)
crossExp$Morphtype<- relevel(crossExp$Morphtype,"OR")
languagelmer1 <- lmer(-1000/rt ~ Relatedness * Morphtype * Language + Logfreq.Zipf.t +rcs(TrialCount) + Lent + (1|Subject) + (1|Target), data = crossExp, REML = F)
languagelmer2 <- lmer(-1000/rt ~ Relatedness * Morphtype * Language + Logfreq.Zipf.t +rcs(TrialCount) + Lent + (1|Subject) + (1|Target), data = subset(crossExp, abs(scale(resid(languagelmer1)))<2.5), REML = F)


summary(languagelmer2)
summary(languagelmer1)
anova(languagelmer2) -> temp
#add separate graph for ita and eng
par(mfrow=c(1,2))
ita<-plotLMER.fnc(languagelmer2, fun = inv, pred = "Relatedness", control = list("Languageita", 1),intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, main = "L1 ITALIAN", ylab='RT (ms)', ylim=c(515,640), xlabel = 'Unrelated                                   Related',bty='l')
eng<-plotLMER.fnc(languagelmer2, fun = inv, pred = "Relatedness", control = list("Languageita", 0),intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, main= 'L2 ENGLISH', ylab='RT (ms)', ylim=c(515,640), xlabel = 'Unrelated                                    Related',bty='l')
par(mfrow=c(1,1))

#computing p-values
confint.merMod(languagelmer2, method = "Wald", FUN = f)
round(1-pf(temp[[4]], temp[[1]], 9609-1-sum(temp[[1]])), digits=3) -> temp$pvalues

#---------------------------------------------------------------------------------------------------#
#                                               END                                                 #
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#                                     Language proficiency analysis                                 #
#---------------------------------------------------------------------------------------------------#
#first take a look at variables distributions
proficiencyData <- datartENG[,c('Target', 'Subject','Age','Gender','Handedness','Rotation.x','phoneticFluency', 'phoneticComprehension','morphComprehension','spelling','readingComprehension','vocabulary','oralComprehension','AoA1', 'AoA2', 'AoA3','AoA4','AoA5','AoA6','AoA7','AoA8','AoA9')];
proficiencyData <- unique(proficiencyData);
summary(proficiencyData)

#histogram of overallProficiency
proficiency <- NULL
for(i in unique(masterFile$Subject)){
  proficiency[i] <- unique(masterFile$overallProf[masterFile$Subject==i])
}
jpeg(filename = "C:/Users/Eva Viviani/Documents/overallProficiency.jpg", res=300, height=1654, width=2229)
hist(proficiency, breaks = 10, xlim = c(30, 120), main = '', xlab = 'Proficiency scores', 
     ylab = 'Subjects', cex.lab=1.5, cex.axis=1.5, cex.sub=1.5)
dev.off()
shapiro.test(proficiency) #normal distribution
rm(proficiency, i)


#histrograms of Proficiency's subtests
jpeg(filename = "C:/Users/Eva Viviani/Documents/ProficiencySubtests.jpg", res=300, height=1654, width=3339)
par(mfrow=c(2,4))
hist(unique(proficiencyData$phoneticFluency), breaks = seq(0,50,5), 
     main = 'Fluency', xlab = 'Scores', ylab = 'Subjects', cex.lab=1.5, cex.axis=1.5, cex.sub=1.5) 
hist(proficiencyData$phoneticComprehension, breaks = seq(0,15,1),
     main = 'Phonetic comprehension', xlab = 'Scores', ylab = 'Subjects', cex.lab=1.5, cex.axis=1.5, cex.sub=1.5)
hist(proficiencyData$morphComprehension, breaks = seq(0,10,1),
     main = 'Morphologic comprehension', xlab = 'Scores', ylab = 'Subjects', cex.lab=1.5, cex.axis=1.5, cex.sub=1.5) 
hist(proficiencyData$spelling, breaks = seq(0,20,2),
     main = 'Spelling', xlab = 'Scores', ylab = 'Subjects', cex.lab=1.5, cex.axis=1.5, cex.sub=1.5) 
hist(proficiencyData$readingComprehension, breaks = seq(0,7,1),
     main = 'Reading comprehension', xlab = 'Scores', ylab = 'Subjects', cex.lab=1.5, cex.axis=1.5, cex.sub=1.5) 
hist(proficiencyData$vocabulary, breaks = seq(0,20,2),
     main = 'Vocabulary', xlab = 'Scores', ylab = 'Subjects', cex.lab=1.5, cex.axis=1.5, cex.sub=1.5) 
hist(proficiencyData$oralComprehension, breaks = seq(0,6,1),
     main = 'Oral comprehension', xlab = 'Scores', ylab = 'Subjects', cex.lab=1.5, cex.axis=1.5, cex.sub=1.5) 
par(mfrow=c(1,1))
dev.off()

#correlation
round(cor(proficiencyData[,6:12]), digits = 2)
collin.fnc(proficiencyData[,6:12]) #see baayen clustering, condition number K
library(corrplot)
corrplot(cor(proficiencyData[,6:12]), type = "lower", order = "hclust", diag = T, method = "circle", outline = T, addgrid.col = F, tl.col = "black", tl.pos = "n")
corrplot(cor(proficiencyData[,6:12]), order = "hclust")

#tentative of clustering
plot(varclus(as.matrix(proficiencyData[,6:12])))

#mixed-models of Proficiency with one variable at time 

datartENG$Relatedness <- factor(datartENG$Relatedness);
#Only english dataset:
#phoneticFluency
proficiencylmer0 <- lmer(-1000/rt ~ Relatedness * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
proficiencylmer1 <- lmer(-1000/rt ~ Relatedness * Morphtype * phoneticFluency + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
anova(proficiencylmer0, proficiencylmer1)
anova(proficiencylmer1)
plotLMER.fnc(proficiencylmer1, fun = inv, pred = "Relatedness",intr = list("phoneticFluency", quantile(datartENG$phoneticFluency), "end"), addlines = T)
#not interesting, yet..

#phoneticComprehension
proficiencylmer2 <- lmer(-1000/rt ~ Relatedness *  Morphtype * phoneticComprehension + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
anova(proficiencylmer0, proficiencylmer2)
anova(proficiencylmer2)
plotLMER.fnc(proficiencylmer2, fun = inv, pred = "Morphtype",intr = list("phoneticComprehension", quantile(datartENG$phoneticComprehension), "end"), addlines = T)
#need to rethink

#morphComprehension
proficiencylmer3 <- lmer(-1000/rt ~ Relatedness *  Morphtype * morphComprehension + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
anova(proficiencylmer0,proficiencylmer3) 
anova(proficiencylmer3)
#nothing significant

#spelling
proficiencylmer4 <- lmer(-1000/rt ~ Relatedness * Morphtype * spelling + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
anova(proficiencylmer0, proficiencylmer4) 
anova(proficiencylmer4)
plotLMER.fnc(proficiencylmer4, fun = inv, pred = "Morphtype",intr = list("spelling", quantile(datartENG$spelling), "end"), addlines = T)
#very confusing, we don't know how to interpret this effect because there is no difference between Relatedness 1 e 0 but only on morphtype

#readingComprehension
proficiencylmer5 <- lmer(-1000/rt ~ Relatedness * Morphtype * readingComprehension + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
anova(proficiencylmer0, proficiencylmer5) 
anova(proficiencylmer5)
plotLMER.fnc(proficiencylmer5, fun = inv, pred = "Morphtype",intr = list("readingComprehension", quantile(datartENG$readingComprehension), "end"), addlines = T)
#same as spelling

#vocabulary
proficiencylmer6 <- lmer(-1000/rt ~ Relatedness * Morphtype * vocabulary + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
anova(proficiencylmer0,proficiencylmer6) #vocabulary*relatedness significativo.vocabulary*MorphtypeTR significativo
anova(proficiencylmer6)
plotLMER.fnc(proficiencylmer6, fun = inv, pred = "Relatedness",intr = list("vocabulary", quantile(datartENG$vocabulary), "end"), addlines = T)
plotLMER.fnc(proficiencylmer6, fun = inv, pred = "Morphtype",intr = list("vocabulary", quantile(datartENG$vocabulary), "end"), addlines = T)
#seems interesting! although the effects are quite "contro-intuitive"

#oralComprehension
proficiencylmer7 <- lmer(-1000/rt ~ Relatedness * Morphtype * oralComprehension + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
anova(proficiencylmer0,proficiencylmer7) 
anova(proficiencylmer7)

#mixed model with morphtype * overallProf e Relatedness * overallProf + 3way interaction
proficiencylmer8 <- lmer(-1000/rt ~ Relatedness * Morphtype * overallProf + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG);
anova(proficiencylmer0, proficiencylmer8); #ok, overall proficiency works nicely. Let's check how:
anova(proficiencylmer8); #mainly through interaction with morphtype; but close to significance in interaction with relatedness too. Let see what role outliers play here:

proficiencylmer8b <- lmer(-1000/rt ~ Relatedness * Morphtype * overallProf + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer8)))<2));
anova(proficiencylmer8b); #wow, huge change! there must be many outliers, and really quite atypical. Which may be ok, it's L2 after all. If this is the story, cutting a little higher, say 2.5SD, should give p values half way btw here and the original model

proficiencylmer8c <- lmer(-1000/rt ~ Relatedness * Morphtype * overallProf + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer8)))<2.5));
anova(proficiencylmer8c); #yeah, exactly as expected. So, I surely trust prof-by-morphtype, which is very reliable; and probably also prof-by-relatedness, which resist some outliers. The three way, I'm not sure, it really seems to be destroyed by just a few outliers. So, let's check out the nature of the effects:

#prof-by-morphtype
plotLMER.fnc(proficiencylmer8c, fun = inv, pred = "Morphtype",intr = list("overallProf", quantile(datartENG$overallProf), "end"), addlines = T, ylab='RT(ms)');
#prof-by-relatedness
plotLMER.fnc(proficiencylmer8c, fun = inv, pred = "Relatedness",intr = list("overallProf", quantile(datartENG$overallProf), "end"), addlines = T, ylab='RT(ms)');

#the three way
plotLMER.fnc(languagelmer2, fun = inv, pred = "Relatedness", control = list("Languageita", 1),intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, main = "ITA", ylab='RT (ms)', ylim=c(515,640))

jpeg(filename = "C:/Users/Eva Viviani/Documents/GitHub/M2-maskedprimingBilinguals/Rplot.jpg", res=300, height=1654, width=3339)
par(mfrow=c(1,4));
a<- plotLMER.fnc(proficiencylmer8b, withList = TRUE, fun = inv, pred = "Relatedness",control = list("overallProf", quantile(datartENG$overallProf, .25)), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='VERY LOW PROFICIENCY', ylimit = c(570,640), bty='l'); 
b<- plotLMER.fnc(proficiencylmer8b, withList = TRUE, fun = inv, pred = "Relatedness",control = list("overallProf", quantile(datartENG$overallProf, .50)), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='LOW PROFICIENCY', ylimit = c(570,640), bty='l');
c<- plotLMER.fnc(proficiencylmer8b, withList = TRUE, fun = inv, pred = "Relatedness",control = list("overallProf", quantile(datartENG$overallProf, .75)), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='HIGH PROFICIENCY', ylimit = c(570,640), bty='l');
d<- plotLMER.fnc(proficiencylmer8b, withList = TRUE, fun = inv, pred = "Relatedness",control = list("overallProf", quantile(datartENG$overallProf, 1)), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='VERY HIGH PROFICIENCY', ylimit = c(570,640), bty='l');
par(mfrow=c(1,1));
dev.off()
#ah ah, bingo here!!!
#heavily modulated

#Primo quartile
df <- do.call(rbind, a$Relatedness)
names(df)[names(df) == "Levels"] <- "Relatedness"
df$Morphtype <- rep(c("OR", "OP", "TR"), each =2)
# plot using ggplot
limitsforgraph <- summaryBy(rt ~ Morphtype + Relatedness, data=subset(datartENG, abs(scale(resid(proficiencylmer8)))<2), FUN=c(length,mean,sd))
names(limitsforgraph)[names(limitsforgraph)=="rt.length"] <- "N"
limitsforgraph$se <- limitsforgraph$rt.sd / sqrt(limitsforgraph$N)
limitsforgraph$mean <- df$Y
dodge <- position_dodge(width = 0.1)
a <-ggplot(data = df, aes(x = Relatedness, y = Y, col = Morphtype ,group = Morphtype)) + scale_colour_manual(breaks = c("OR", "OP", "TR"), values = c("#0000e8", "#000000", "#ff0030")) + geom_point(position = dodge, size = 4.5, shape=21, fill="white") + geom_line(position = dodge)+ theme_classic()
a <- a + scale_y_continuous("RT(ms)",limits=c(550,650))
a <- a + geom_errorbar(aes(ymin = limitsforgraph$mean - limitsforgraph$se, ymax = limitsforgraph$mean + limitsforgraph$se), width=0.1, size=1, linetype=1, position = dodge) #FUCK YEAH
a <- a + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))
a <- a + theme(axis.text.y = element_text(angle = 00, hjust = 1, size=13, face = 'bold', colour = 'black'))
a <- a + labs(x = "UNRELATED       RELATED ")
a <- a + theme(axis.title.x = element_text(size = rel(1), face = 'bold'))
a
ggsave("firsquartileProficiency.jpg", height=6, width=6, dpi = 2000)

#Secondo quartile
df <- do.call(rbind, b$Relatedness)
names(df)[names(df) == "Levels"] <- "Relatedness"
df$Morphtype <- rep(c("OR", "OP", "TR"), each =2)
# plot using ggplot
limitsforgraph <- summaryBy(rt ~ Morphtype + Relatedness, data=subset(datartENG, abs(scale(resid(proficiencylmer8)))<2), FUN=c(length,mean,sd))
names(limitsforgraph)[names(limitsforgraph)=="rt.length"] <- "N"
limitsforgraph$se <- limitsforgraph$rt.sd / sqrt(limitsforgraph$N)
limitsforgraph$mean <- df$Y
dodge <- position_dodge(width = 0.1)
b <-ggplot(data = df, aes(x = Relatedness, y = Y, col = Morphtype ,group = Morphtype)) + scale_colour_manual(breaks = c("OR", "OP", "TR"), values = c("#0000e8", "#000000", "#ff0030")) + geom_point(position = dodge, size = 4.5, shape=21, fill="white") + geom_line(position = dodge)+ theme_classic()
b <- b + scale_y_continuous("RT(ms)",limits=c(550,650))
b <- b + geom_errorbar(aes(ymin = limitsforgraph$mean - limitsforgraph$se, ymax = limitsforgraph$mean + limitsforgraph$se), width=0.1, size=1, linetype=1, position = dodge) #FUCK YEAH
b <- b + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))
b <- b + theme(axis.text.y = element_text(angle = 00, hjust = 1, size=13, face = 'bold', colour = 'black'))
b <- b + labs(x = "UNRELATED       RELATED ")
b <- b + theme(axis.title.x = element_text(size = rel(1), face = 'bold'))
b
ggsave("secondquartileProficiency.jpg", height=6, width=6, dpi = 2000)

#Terzo quartile
df <- do.call(rbind, c$Relatedness)
names(df)[names(df) == "Levels"] <- "Relatedness"
df$Morphtype <- rep(c("OR", "OP", "TR"), each =2)
# plot using ggplot
limitsforgraph <- summaryBy(rt ~ Morphtype + Relatedness, data=subset(datartENG, abs(scale(resid(proficiencylmer8)))<2), FUN=c(length,mean,sd))
names(limitsforgraph)[names(limitsforgraph)=="rt.length"] <- "N"
limitsforgraph$se <- limitsforgraph$rt.sd / sqrt(limitsforgraph$N)
limitsforgraph$mean <- df$Y
dodge <- position_dodge(width = 0.1)
c <-ggplot(data = df, aes(x = Relatedness, y = Y, col = Morphtype ,group = Morphtype)) + scale_colour_manual(breaks = c("OR", "OP", "TR"), values = c("#0000e8", "#000000", "#ff0030")) + geom_point(position = dodge, size = 4.5, shape=21, fill="white") + geom_line(position = dodge)+ theme_classic()
c <- c + scale_y_continuous("RT(ms)",limits=c(550,650))
c <- c + geom_errorbar(aes(ymin = limitsforgraph$mean - limitsforgraph$se, ymax = limitsforgraph$mean + limitsforgraph$se), width=0.1, size=1, linetype=1, position = dodge) #FUCK YEAH
c <- c + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))
c <- c + theme(axis.text.y = element_text(angle = 00, hjust = 1, size=13, face = 'bold', colour = 'black'))
c <- c + labs(x = "UNRELATED       RELATED ")
c <- c + theme(axis.title.x = element_text(size = rel(1), face = 'bold'))
c
ggsave("thirdquartileProficiency.jpg", height=6, width=6, dpi = 2000)

#quarto quartile
df <- do.call(rbind, d$Relatedness)
names(df)[names(df) == "Levels"] <- "Relatedness"
df$Morphtype <- rep(c("OR", "OP", "TR"), each =2)
# plot using ggplot
limitsforgraph <- summaryBy(rt ~ Morphtype + Relatedness, data=subset(datartENG, abs(scale(resid(proficiencylmer8)))<2), FUN=c(length,mean,sd))
names(limitsforgraph)[names(limitsforgraph)=="rt.length"] <- "N"
limitsforgraph$se <- limitsforgraph$rt.sd / sqrt(limitsforgraph$N)
limitsforgraph$mean <- df$Y
dodge <- position_dodge(width = 0.1)
d <-ggplot(data = df, aes(x = Relatedness, y = Y, col = Morphtype ,group = Morphtype)) + scale_colour_manual(breaks = c("OR", "OP", "TR"), values = c("#0000e8", "#000000", "#ff0030")) + geom_point(position = dodge, size = 4.5, shape=21, fill="white") + geom_line(position = dodge)+ theme_classic()
d <- d + scale_y_continuous("RT(ms)",limits=c(550,650))
d <- d + geom_errorbar(aes(ymin = limitsforgraph$mean - limitsforgraph$se, ymax = limitsforgraph$mean + limitsforgraph$se), width=0.1, size=1, linetype=1, position = dodge) #FUCK YEAH
d <- d + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))
d <- d + theme(axis.text.y = element_text(angle = 00, hjust = 1, size=13, face = 'bold', colour = 'black'))
d <- d + labs(x = "UNRELATED       RELATED ")
d <- d + theme(axis.title.x = element_text(size = rel(1), face = 'bold'))
d
ggsave("fourthquartileProficiency.jpg", height=6, width=6, dpi = 2000)

#Let's see how many subjects were contained in these proficiency quantiles
subset(datartENG, datartENG$overallProf<=54)-> firstQ
unique(firstQ$Subject)-> firstQ #10 subjects
subset(datartENG, datartENG$overallProf>54 & datartENG$overallProf<70)-> secondQ
unique(secondQ$Subject)-> secondQ #9 subjects

subset(datartENG, datartENG$overallProf>=70 & datartENG$overallProf<80)-> thirdQ
unique(thirdQ$Subject)-> thirdQ #9 subjects

subset(datartENG, datartENG$overallProf>=80)-> fourthQ
unique(fourthQ$Subject)-> fourthQ #9 subjects

rm(dodge, aa, df)
#---------------------------------------------------------------------------------------------------#
#                                               END                                                 #
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#                                               AoA                                                 #
#---------------------------------------------------------------------------------------------------#
AoA <- NULL
for(i in unique(masterFile$Subject)){
  AoA[i] <- unique(masterFile$AoA1[masterFile$Subject==i])
}
jpeg(filename = "C:/Users/Eva Viviani/Documents/AoA.jpg", res=300, height=1654, width=2229)
hist(AoA, main = '', xlab = 'Age of acquisition', 
     ylab = 'Subjects', cex.lab=1.5, cex.axis=1.5, cex.sub=1.5)
dev.off()
shapiro.test(proficiency) #normal distribution
rm(proficiency, i)

#AoA1 "A che et? hai iniziato ad essere esposto alla lingua inglese?"
proficiencylmer9 <- lmer(-1000/rt ~ Relatedness * AoA1 * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
proficiencylmer10 <- lmer(-1000/rt ~ Relatedness * AoA1  * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer9)))<2))
anova(proficiencylmer0, proficiencylmer9) #relatedness*AoA1 significativo
anova(proficiencylmer9) #anche qui senza filtrare c'? solo l'interazione tra relatedness*AoA1
anova(proficiencylmer10) #qui invece anche la 3way interaction
plotLMER.fnc(proficiencylmer10, fun = inv, pred = "Relatedness",intr = list("AoA1", quantile(datartENG$AoA1), "end"), addlines = T)
plotLMER.fnc(proficiencylmer10, fun = inv, pred = "Morphtype",intr = list("AoA1", quantile(datartENG$AoA1), "end"), addlines = T)

fivenum(datartENG$AoA1)
par(mfrow=c(2,2));
plotLMER.fnc(proficiencylmer10, fun = inv, pred = "Relatedness",control = list("AoA1", quantile(datartENG$AoA1, .01)), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', main='.01');
plotLMER.fnc(proficiencylmer10, fun = inv, pred = "Relatedness",control = list("AoA1", quantile(datartENG$AoA1, .25)), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', main='.25', ylimit = c(570,630));
plotLMER.fnc(proficiencylmer10, fun = inv, pred = "Relatedness",control = list("AoA1", quantile(datartENG$AoA1, .50)), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', main='.50', ylimit = c(570,630));
plotLMER.fnc(proficiencylmer10, fun = inv, pred = "Relatedness",control = list("AoA1", quantile(datartENG$AoA1, .75)), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', main='.75', ylimit = c(570,630));
par(mfrow=c(1,1));

#semb

#AoA2 "quanto usi l'inglese nella tua vita quotidiana da 1 a 5?" 
proficiencylmer11 <- lmer(-1000/rt ~ Relatedness * AoA2 * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
proficiencylmer11b <- lmer(-1000/rt ~ Relatedness * AoA2 * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer11)))<2))
anova(proficiencylmer0, proficiencylmer11)
anova(proficiencylmer11) #qui significativo morphtype*AoA2 e relatedness*AoA2 separatamente. No 3way interaction.
anova(proficiencylmer11b) #qui scompare morphtype*AoA2, ma compare una 3way tra relatedness:Aoa2:morphtype, perch???
plotLMER.fnc(proficiencylmer11, fun = inv, pred = "Relatedness",intr = list("AoA2", quantile(datartENG$AoA2), "end"), addlines = T)
plotLMER.fnc(proficiencylmer11, fun = inv, pred = "Morphtype",intr = list("AoA2", quantile(datartENG$AoA2), "end"), addlines = T)
#da considerare come proficiency

par(mfrow=c(2,2));
plotLMER.fnc(proficiencylmer11, fun = inv, pred = "Relatedness",control = list("AoA2", 1), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', main='.25');
plotLMER.fnc(proficiencylmer11, fun = inv, pred = "Relatedness",control = list("AoA2", 3), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', main='.50');
plotLMER.fnc(proficiencylmer11, fun = inv, pred = "Relatedness",control = list("AoA2", 5), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', main='.75');
par(mfrow=c(1,1));

#AoA3 "In quale contesto hai iniziato ad essere esposto alla lingua inglese? Casa o scuola?"
proficiencylmer12 <- lmer(-1000/rt ~ Relatedness * AoA3 * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer11)))<2))
anova(proficiencylmer0, proficiencylmer12) 
anova(proficiencylmer12)
summary(datartENG$AoA3) #too unbalanced

#AoA5 "Sei cresciuta/o in un ambiente dove si parlano pi? lingue? 1: s? 2: no"
proficiencylmer13 <- lmer(-1000/rt ~ Relatedness * AoA5 * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
anova(proficiencylmer0, proficiencylmer13) 
anova(proficiencylmer13) 
#nothing significant here

#AoA6 "Se parli più lingue, qual è la lingua che conosci meglio dopo la tua madrelingua? 1: eng 2: altro"
summary(datartENG$AoA6)
proficiencylmer14 <- lmer(-1000/rt ~ Relatedness * as.factor(AoA6) * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
anova(proficiencylmer0, proficiencylmer14) 
anova(proficiencylmer14) 

#AoA7 "Come valuteresti il livello di conoscenza della tua seconda lingua su una scala da 1 (base) a 5 (avanzato)?"
proficiencylmer15 <- lmer(-1000/rt ~ Relatedness * AoA7 * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
proficiencylmer15b <- lmer(-1000/rt ~ Relatedness * AoA7 * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer15)))<2))
anova(proficiencylmer0, proficiencylmer15) 
anova(proficiencylmer15b) #useless

#AoA8 "Qual ? la terza lingua che conosci meglio dopo la tua madrelingua? 1: eng 2: altro 3: nessun'altra"
summary(as.factor(datartENG$AoA8))
proficiencylmer16 <- lmer(-1000/rt ~ Relatedness * as.factor(AoA8) * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
proficiencylmer16b <- lmer(-1000/rt ~ Relatedness * as.factor(AoA8) * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer16)))<2))
anova(proficiencylmer0, proficiencylmer16) 
anova(proficiencylmer16) #same as AoA7, nonsense this analysis
anova(proficiencylmer16b) #same as AoA7, nonsense this analysis


#AoA9 "Come valuteresti il livello di conoscenza della tua terza lingua su una scala da 1 (base) a 5 (avanzato)?"
proficiencylmer17 <- lmer(-1000/rt ~ as.factor(Relatedness) * AoA9 * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
anova(proficiencylmer0, proficiencylmer17) 
anova(proficiencylmer17) #nothing significant here

#---------------------------------------------------------------------------------------------------#
#                                               END                                                 #
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#                                     LMM tra OSC || dprime ~ rt                                    #
#---------------------------------------------------------------------------------------------------#
#                                             27 aprile 2017                                        #
#First, let's recap:
#rt ~ morphtype * overallProf e Relatedness * overallProf + 3way interaction
proficiencylmer8 <- lmer(-1000/rt ~ Relatedness * Morphtype * overallProf + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG);
anova(proficiencylmer8);
proficiencylmer8b <- lmer(-1000/rt ~ Relatedness * Morphtype * overallProf + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer8)))<2));
anova(proficiencylmer8b); 
proficiencylmer8c <- lmer(-1000/rt ~ Relatedness * Morphtype * overallProf + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer8)))<2.5));
anova(proficiencylmer8c);

par(mfrow=c(1,4));
plotLMER.fnc(proficiencylmer8, withList = TRUE, fun = inv, pred = "Relatedness",control = list("overallProf", quantile(datartENG$overallProf, .25)), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='VERY LOW PROFICIENCY', ylimit = c(570,640), bty='l'); 
plotLMER.fnc(proficiencylmer8, withList = TRUE, fun = inv, pred = "Relatedness",control = list("overallProf", quantile(datartENG$overallProf, .50)), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='LOW PROFICIENCY', ylimit = c(570,640), bty='l');
plotLMER.fnc(proficiencylmer8, withList = TRUE, fun = inv, pred = "Relatedness",control = list("overallProf", quantile(datartENG$overallProf, .75)), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='HIGH PROFICIENCY', ylimit = c(570,640), bty='l');
plotLMER.fnc(proficiencylmer8, withList = TRUE, fun = inv, pred = "Relatedness",control = list("overallProf", quantile(datartENG$overallProf, 1)), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='VERY HIGH PROFICIENCY', ylimit = c(570,640), bty='l');
par(mfrow=c(1,1));

#controllo dell'assunto classico: 'Stems taken from the transparent sets have >OSC than OP or OR sets.'
#ok!
tapply(datartENG$OSC_Target, datartENG$Morphtype, FUN = fivenum)

#rt ~ morphtype * overallProf SENZA Relatedness e OSC. 
proficiencylmer9 <- lmer(-1000/rt ~ Morphtype * overallProf + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG);
anova(proficiencylmer9)
proficiencylmer9b <- lmer(-1000/rt ~ Morphtype * overallProf + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer9)))<2));
anova(proficiencylmer9b)

#rt ~ OSC_Primes * overallProf SENZA Relatedness e morphtype 
proficiencylmer10 <- lmer(-1000/rt ~ overallProf * OSC_Target + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG);
anova(proficiencylmer10)
proficiencylmer10b <- lmer(-1000/rt ~ overallProf * OSC_Target + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer10)))<2));
anova(proficiencylmer10b)
#morphtype e OSC correlano molto con proficiency
#goodness of fit
round(cor(fitted(proficiencylmer10), -1000/datartENG$rt[!is.na(datartENG$OSC_Target)])^2, digits=3)
round(cor(fitted(proficiencylmer9), -1000/datartENG$rt)^2, digits=3)

plotLMER.fnc(proficiencylmer10b, fun = inv, pred = "OSC_Target", intr = list("overallProf", quantile(datartENG$overallProf, c(.25,.50,.75,1)), "end"), addlines = T, ylab='RT(ms)', bty='l'); 

gam1 <- gam(-1000/rt ~ s(OSC_Target, by = overallProf) + s(TrialCount) + s(Logfreq.Zipf.t) + s(Subject, bs = 're') + s(Target, bs = 're'), data = datartENG)

jpeg(filename = "C:/Users/Eva Viviani/Documents/Rplot.jpg", res=300, height=1654, width=3339)
vis.gam(gam1, view=c("OSC_Target","overallProf"), type="response", plot.type="contour", main="", too.far=.1, xlab='OSC', ylab='Proficiency scores');
dev.off()

par(mfrow=c(1,2))
vis.gam(gam1, view=c("Logfreq.Zipf.t", "Logfreq.Zipf.t"), type="response", plot.type="contour", main="  Trialcount", too.far=.1);
vis.gam(gam1, view=c("TrialCount"), type="response", plot.type="contour", main=" LogFreq of targets", too.far=.1);
par(mfrow=c(1,1))

