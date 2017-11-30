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
library(effects)
library(plyr)
library(corrplot)
library(ggpubr)


inv <- function(x) { -1000/x}

subset(masterFile, Language=="ita")-> masterfileIta

# from here, run only if you want to take a look at the output of the 'diagnostics.R',
# otherwise, go directly to line n. 36
sbj.id<- masterfileIta$Subject
acc<- masterfileIta$Accuracy
lexicality<- tolower(masterfileIta$Lexicality)
target<- masterfileIta$Target
rt<- masterfileIta$rt

source("Diagnostics.R") #I've uploaded this function into the git folder, so that everyone can import it into R, and use it. 
diagnostics.f(rt = rt, acc = acc, sbj.id = sbj.id, target = target, lexicality = lexicality, "ita")
#ok, end!

#--------------------------------------------------------------------------------------------------------#
#                                 Results of the diagnostics.R:                                          #
#--------------------------------------------------------------------------------------------------------#
#Subj. 16 has reported seeing clearly the prime, for this reason will be taken out of the analysis, in addition to the words that scored less than 70% of accuracy. 
# We filter also Rts from 250 to 1600ms
subset(target.diagnostics, acc<0.6)-> parolebrutte #.giusto per sapere quali sono
subset(masterfileIta, masterfileIta$rt>300 & masterfileIta$Subject!=15 & masterfileIta$Subject!=2 & masterfileIta$Subject!=31  & masterfileIta$Subject!=43
        & masterfileIta$Target!= "guano" & masterfileIta$Target!= "uggia" & masterfileIta$Target!= "vello" & masterfileIta$Lexicality=="WORD") -> dataAccITA
#Then, we select only right answers
subset(dataAccITA, dataAccITA$Accuracy==1)-> datartITA
summary(datartITA)

#First look at the means
round(xtabs(datartITA$rt ~ datartITA$Morphtype + datartITA$Primetype) / xtabs(~datartITA$Morphtype + datartITA$Primetype), digits = 0)
stdErr <- function(x) {sd(x)/ sqrt(length(x))}
round(tapply(datartITA$rt, list(datartITA$Morphtype, datartITA$Primetype), sd), digits = 0)
round(tapply(datartITA$rt, list(datartITA$Morphtype, datartITA$Primetype), stdErr), digits = 1)


#---------------------------------------------------------------------------------------------------#
#                                         ENG f.Diagnostics                                         #
#---------------------------------------------------------------------------------------------------#
subset(masterFile, Language=="eng")-> masterfileEng

# from here, run only if you want to take a look at the output of the 'diagnostics.R',
# otherwise, go to line n. 63
sbj.id<- masterfileEng$Subject
acc<- masterfileEng$Accuracy
lexicality<- masterfileEng$Lexicality
lexicality<- tolower(masterfileEng$Lexicality)
target<-masterfileEng$Target
rt<-masterfileEng$rt

diagnostics.f(rt = rt, acc = acc, sbj.id = sbj.id, target = target, lexicality = lexicality, "eng")

#Subj. 26 *likely* confused the YES/NO buttons
#the correlation below shows a frequency effect, so he didn't answer just by chance:
cor(masterfileEng[masterfileEng$Subject==26 & masterfileEng$Lexicality=='WORD', 
                  c('rt','Logfreq.Zipf.t')], use = 'pairwise.complete.obs')

masterFile$Accuracy[masterFile$Subject==26 & masterFile$Accuracy==1] <- 2;
masterFile$Accuracy[masterFile$Subject==26 & masterFile$Accuracy==0] <- 3;

masterFile$Accuracy[masterFile$Subject==26 & masterFile$Accuracy==2] <- 0;
masterFile$Accuracy[masterFile$Subject==26 & masterFile$Accuracy==3] <- 1;



#Subjs 15 and 43 saw the prime. Sbjs 22 will be taken out because their performance is very far from the others and the accuracy less than 40%, see the output graph from the diagnostics function.
# We filter also Rts from 350 to 1800ms
subset(masterfileEng, masterfileEng$rt>350 & masterfileEng$rt<1800 
       & masterfileEng$Subject!=15 & masterfileEng$Subject!=22 & masterfileEng$Subject!=43 
       & masterfileEng$Lexicality=="WORD") -> dataAccENG
#Then, we select only right answers
subset(dataAccENG, dataAccENG$Accuracy==1)-> datartENG
summary(datartENG)
#First look at the means
round(xtabs(datartENG$rt ~ datartENG$Morphtype + datartENG$Primetype) / xtabs(~datartENG$Morphtype + datartENG$Primetype), digits = 0)
round(tapply(datartENG$rt, list(datartENG$Morphtype, datartENG$Primetype), sd), digits = 0)

#clean up the workspace
rm(rt, acc, sbj.id, target, lexicality, masterfileIta, masterfileEng);

#---------------------------------------------------------------------------------------------------#
#                                         d primes analysis                                         #
#---------------------------------------------------------------------------------------------------#
masterfileEng <- subset(masterFile, masterFile$Language=='eng')
xtabs( ~ masterfileEng$Resp + masterfileEng$Subject) #look at response given by sbjs

#I decided to remove the no responses from the subset
masterfileEng <- subset(masterfileEng, masterfileEng$Resp != 0)


sbj.numbers <- subset(masterfileEng, masterfileEng$Subject!= "15" & masterfileEng$Subject!= "22") #remove sbj 16 as his d' value is NAN
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

hist(bySs_dprimes$dprimes)

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
datartITA$Morphtype <- relevel(datartITA$Morphtype, "OR")

italmer1 <- lmer(-1000/rt ~ TrialCount + Rotation.x + (1|Subject) + (1|Target), data= datartITA, REML = F)
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

plotLMER.fnc(italmer5b, fun = inv, withList = TRUE, pred = "Relatedness", intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, xlabel ="Relatedness" , ylabel = "-1000/rt", main = "ITA")

df <- effect("Relatedness:Morphtype",italmer5b) 
df <- as.data.frame(df)
df$fit <- inv(df$fit)
df$lower <- inv(df$lower)
df$upper <- inv(df$upper)

dodge <- position_dodge(width = 0.1)
bb  <-ggplot(data = df, aes(x = Relatedness, y = fit, col = Morphtype ,group = Morphtype)) + scale_colour_manual(breaks = c("OR", "OP", "TR"), values = c("#0000e8", "#000000", "#ff0030")) + geom_point(position = dodge, size = 3.5, shape=21, fill="white") + geom_line(position = dodge)+ theme_classic()
bb  <- bb + geom_errorbar(aes(ymin = df$lower, ymax = df$upper), width=0.1, size=1, linetype=1, position = dodge) #FUCK YEAH
bb  <- bb + scale_y_continuous("RT(ms)",limits=c(510,675))
bb  <- bb + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))
bb  <- bb + theme(axis.text.y = element_text(angle = 00, hjust = 1, size=10, colour = 'black'))
bb  <- bb + theme(axis.text.x = element_blank())
bb  <- bb + labs(x = "unrelated                      related ")
bb  <- bb + theme(axis.title.x = element_text(size = rel(1)))
bb <- bb + labs(title='L1 - Italian')
bb <- bb + theme(plot.title= element_text(angle = 00, hjust=0.5, size=15, face = 'bold', colour = 'black'))
bb <- bb + theme(legend.title = element_text(size = 8))
bb <- bb + theme(legend.text = element_text(size = 8))
bb
ggsave("itaplot.jpg")

#---------------------------------------------------------------------------------------------------#
#                                                    ENG                                            #
#---------------------------------------------------------------------------------------------------#
datartENG$Relatedness <- as.factor(datartENG$Relatedness)
datartENG$Morphtype <- relevel(datartENG$Morphtype, "OP")
englmer1 <- lmer(-1000/rt ~ TrialCount + Rotation.x + (1|Subject) + (1|Target), data= datartENG, REML = F)
summary(englmer1)
#2
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
englmer5b <- lmer(-1000/rt ~ Relatedness * Morphtype + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data= subset(datartENG, abs(scale(resid(englmer5)))<2), REML = F)
summary(englmer5b)
anova(englmer5b)

plotLMER.fnc(englmer5b, fun = inv, withList = TRUE, pred = "Relatedness", intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, xlabel ="Relatedness" , ylabel = "-1000/rt", main = "ENG")

df <- effect("Relatedness:Morphtype",englmer5b) 
df <- as.data.frame(df)
df$fit <- inv(df$fit)
df$lower <- inv(df$lower)
df$upper <- inv(df$upper)

dodge <- position_dodge(width = 0.1)
gg  <-ggplot(data = df, aes(x = Relatedness, y = fit, col = Morphtype ,group = Morphtype)) + scale_colour_manual(breaks = c("OR", "OP", "TR"), values = c("#0000e8", "#000000", "#ff0030")) + geom_point(position = dodge, size = 3.5, shape=21, fill="white") + geom_line(position = dodge)+ theme_classic()
gg  <- gg + geom_errorbar(aes(ymin = df$lower, ymax = df$upper), width=0.1, size=1, linetype=1, position = dodge) #FUCK YEAH
gg  <- gg + scale_y_continuous("RT(ms)",limits=c(510,675))
gg  <- gg + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))
gg  <- gg + theme(axis.text.y = element_text(angle = 00, hjust = 1, size=10, colour = 'black'))
gg  <- gg + theme(axis.text.x = element_blank())
gg  <- gg + labs(x = "unrelated                      related ")
gg  <- gg + theme(axis.title.x = element_text(size = rel(1)))
gg <- gg + labs(title='L2 - English')
gg <- gg + theme(plot.title= element_text(angle = 00, hjust=0.5, size=15, face = 'bold', colour = 'black'))
gg <- gg + theme(legend.title = element_text(size = 8))
gg <- gg + theme(legend.text = element_text(size = 8))
gg
ggsave("engplot.jpg")
rm(gg, bb, aa, dodge)
#---------------------------------------------------------------------------------------------------#
#                                     Cross-experiment interaction                                  #
#   in order to be completely sure that pattern of results in ITA and ENG differ,                   #
#   we need to have a three-way interaction here btw Relatedness, MorphType and Language            #
#---------------------------------------------------------------------------------------------------#
rbind(datartENG[,1:44], datartITA) -> crossExp
crossExp$Relatedness <- as.factor(crossExp$Relatedness)
crossExp$Morphtype<- relevel(crossExp$Morphtype,"OR")
languagelmer1 <- lmer(-1000/rt ~ Relatedness * Morphtype * Language + Logfreq.Zipf.t +rcs(TrialCount) + Lent + (1|Subject) + (1|Target), data = crossExp, REML = F)
languagelmer2 <- lmer(-1000/rt ~ Relatedness * Morphtype * Language + Logfreq.Zipf.t +rcs(TrialCount) + Lent + (1|Subject) + (1|Target), data = subset(crossExp, abs(scale(resid(languagelmer1)))<2.5), REML = F)


summary(languagelmer2)
summary(languagelmer1)
#add separate graph for ita and eng
par(mfrow=c(1,2))
ita<-plotLMER.fnc(languagelmer2, fun = inv, pred = "Relatedness", control = list("Languageita", 1),intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, main = "L1 ITALIAN", ylab='RT (ms)', ylim=c(515,700), xlabel = 'Unrelated                                   Related',bty='l')
eng<-plotLMER.fnc(languagelmer2, fun = inv, pred = "Relatedness", control = list("Languageita", 0),intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, main= 'L2 ENGLISH', ylab='RT (ms)', ylim=c(515,700), xlabel = 'Unrelated                                    Related',bty='l')
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
summary(proficiencyData) 

#histogram of overallProficiency
proficiency <- NULL
for(i in unique(masterFile$Subject)){
  proficiency[i] <- unique(masterFile$overallProf[masterFile$Subject==i])
}
jpeg(filename = "C:/Users/eva_v/Documents/overallProficiency.jpg", res=300, height=2500, width=3229)
hist(proficiency, breaks = 15, xlim = c(30, 120), ylim = c(0,14), main = 'Proficiency', 
     xlab = 'score', ylab = '', cex.lab=2, cex.axis=2, cex.sub=4, cex.main=3)
title(ylab = 'Subjects', cex.lab=1.5)
dev.off()
shapiro.test(proficiency) #normal distribution
rm(proficiency, i)

phoneticFluency <- NULL
for (i in unique(proficiencyData$Subject)){
  phoneticFluency[i] <- unique(proficiencyData$phoneticFluency[proficiencyData$Subject==i])
}

phoneticComprehension <- NULL
for (i in unique(proficiencyData$Subject)){
phoneticComprehension[i] <- unique(proficiencyData$phoneticComprehension[proficiencyData$Subject==i])
}

morphComprehension <- NULL
for (i in unique(proficiencyData$Subject)){
  morphComprehension[i] <- unique(proficiencyData$morphComprehension[proficiencyData$Subject==i])
}

spelling <- NULL
for (i in unique(proficiencyData$Subject)){
  spelling[i] <- unique(proficiencyData$spelling[proficiencyData$Subject==i])
}

readingComprehension <- NULL
for (i in unique(proficiencyData$Subject)){
  readingComprehension[i] <- unique(proficiencyData$readingComprehension[proficiencyData$Subject==i])
}

vocabulary <- NULL
for (i in unique(proficiencyData$Subject)){
  vocabulary[i] <- unique(proficiencyData$vocabulary[proficiencyData$Subject==i])
}

oralComprehension <- NULL
for (i in unique(proficiencyData$Subject)){
  oralComprehension[i] <- unique(proficiencyData$oralComprehension[proficiencyData$Subject==i])
}

#Data Proficiency normalization:
datartENG$z.overallProf <-scale(datartENG[28:34], scale = T);
datartENG$scale.overallProf <- apply(datartENG$z.overallProf,1,FUN = sum);


#histrograms of Proficiency's subtests
jpeg(filename = "C:/Users/eva_v/Documents/ProficiencySubtests.jpg", res=300, height=2200, width=4339)
par(mfrow=c(2,4))
hist(phoneticFluency, breaks = seq(0,50,5), ylim = c(0,20),
     main = 'Fluency', xlab = 'Scores', ylab = '', cex.lab=2, cex.axis=2, cex.sub=4, cex.main=2) 
title(ylab = 'Subjects', cex.lab=1.5)
hist(phoneticComprehension, breaks = seq(0,15,1),ylim = c(0,20),
     main = 'Phonemic comprehension', xlab = 'Scores', ylab = '', cex.lab=2, cex.axis=2, cex.sub=4, cex.main=2)
title(ylab = 'Subjects', cex.lab=1.5)
 
hist(morphComprehension, breaks = seq(0,10,1), ylim = c(0,40),
     main = 'Morphologic comprehension', xlab = 'Scores', ylab = '', cex.lab=2, cex.axis=2, cex.sub=4, cex.main=2) 
title(ylab = 'Subjects', cex.lab=1.5)
 
hist(spelling, breaks = seq(0,20,2),ylim = c(0,20),
     main = 'Spelling', xlab = 'Scores', ylab = '', cex.lab=2, cex.axis=2, cex.sub=4, cex.main=2) 
title(ylab = 'Subjects', cex.lab=1.5)
 
hist(readingComprehension, breaks = seq(0,7,1),ylim = c(0,25),
     main = 'Reading comprehension', xlab = 'Scores', ylab = '', cex.lab=2, cex.axis=2, cex.sub=4, cex.main=2) 
title(ylab = 'Subjects', cex.lab=1.5)
 
hist(vocabulary, breaks = seq(0,20,2),ylim = c(0,40),
     main = 'Vocabulary', xlab = 'Scores', ylab = '', cex.lab=2, cex.axis=2, cex.sub=4, cex.main=2) 
title(ylab = 'Subjects', cex.lab=1.5)
 
hist(oralComprehension, breaks = seq(0,6,1),ylim = c(0,40),
     main = 'Oral comprehension', xlab = 'Scores', ylab = '', cex.lab=2, cex.axis=2, cex.sub=4, cex.main=2) 
title(ylab = 'Subjects', cex.lab=1.5)
 
par(mfrow=c(1,1))
dev.off()

#correlation
round(cor(proficiencyData[,7:13]), digits = 2)
collin.fnc(proficiencyData[,7:13]) #see baayen clustering, condition number K
corrplot(cor(proficiencyData[,7:13]), type = "lower", order = "hclust", diag = T, method = "circle", outline = T, addgrid.col = F, tl.col = "black", tl.pos = "n")
corrplot(cor(proficiencyData[,7:13]), order = "hclust")

#tentative of clustering
plot(varclus(as.matrix(proficiencyData[,7:13])))

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
proficiencylmer8 <- lmer(-1000/rt ~ Relatedness * Morphtype * scale.overallProf + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG);
anova(proficiencylmer0, proficiencylmer8); #ok, overall proficiency works nicely. Let's check how:
anova(proficiencylmer8); #mainly through interaction with morphtype; but close to significance in interaction with relatedness too. Let see what role outliers play here:

proficiencylmer8b <- lmer(-1000/rt ~ Relatedness * Morphtype * scale.overallProf + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer8)))<2));
proficiencylmer8b <- lmer(-1000/rt ~ Relatedness * Morphtype * overallProf + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer8)))<2));
summary(proficiencylmer8b)
anova(proficiencylmer8b); #wow, huge change! there must be many outliers, and really quite atypical. Which may be ok, it's L2 after all. If this is the story, cutting a little higher, say 2.5SD, should give p values half way btw here and the original model

proficiencylmer8c <- lmer(-1000/rt ~ Relatedness * Morphtype * scale.overallProf + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer8)))<2.5));
anova(proficiencylmer8c); #yeah, exactly as expected. So, I surely trust prof-by-morphtype, which is very reliable; and probably also prof-by-relatedness, which resist some outliers. The three way, I'm not sure, it really seems to be destroyed by just a few outliers. So, let's check out the nature of the effects:

#prof-by-morphtype
plotLMER.fnc(proficiencylmer8c, fun = inv, pred = "Morphtype",intr = list("overallProf", quantile(datartENG$overallProf), "end"), addlines = T, ylab='RT(ms)');
#prof-by-relatedness
plotLMER.fnc(proficiencylmer8c, fun = inv, pred = "Relatedness",intr = list("overallProf", quantile(datartENG$overallProf), "end"), addlines = T, ylab='RT(ms)');

#the three way
plotLMER.fnc(languagelmer2, fun = inv, pred = "Relatedness", control = list("Languageita", 1),intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, main = "ITA", ylab='RT (ms)', ylim=c(515,640))
plotLMER.fnc(languagelmer2, fun = inv, pred = "Relatedness", control = list("Languageita", 2),intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, main = "ENG", ylab='RT (ms)')


jpeg(filename = "C:/Users/Eva Viviani/Documents/GitHub/M2-maskedprimingBilinguals/Rplot.jpg", res=300, height=1654, width=3339)
par(mfrow=c(1,3));
a<- plotLMER.fnc(proficiencylmer8b, withList = TRUE, fun = inv, pred = "Relatedness",control = list("overallProf", quantile(datartENG$overallProf, .1)), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='VERY LOW PROFICIENCY', ylimit = c(570,700), bty='l'); 
b<- plotLMER.fnc(proficiencylmer8b, withList = TRUE, fun = inv, pred = "Relatedness",control = list("overallProf", quantile(datartENG$overallProf, .5)), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='LOW PROFICIENCY', ylimit = c(570,700), bty='l');
#c<- plotLMER.fnc(proficiencylmer8b, withList = TRUE, fun = inv, pred = "Relatedness",control = list("overallProf", quantile(datartENG$overallProf, .6)), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='HIGH PROFICIENCY', ylimit = c(570,700), bty='l');
d<- plotLMER.fnc(proficiencylmer8b, withList = TRUE, fun = inv, pred = "Relatedness",control = list("overallProf", quantile(datartENG$overallProf, .9)), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='VERY HIGH PROFICIENCY', ylimit = c(570,700), bty='l');
par(mfrow=c(1,1));
dev.off()
#ah ah, bingo here!!!
#heavily modulated

#Primo quartile
df <- effect("Relatedness:Morphtype:overallProf",proficiencylmer8b) 
df <- as.data.frame(df)
df$fit <- inv(df$fit)
df$lower <- inv(df$lower)
df$upper <- inv(df$upper)

subset(df, df$overallProf<=40)-> verylowProf
# plot using ggplot
dodge <- position_dodge(width = 0.1)
a <-ggplot(data = verylowProf, aes(x = Relatedness, y = fit, col = Morphtype ,group = Morphtype)) + scale_colour_manual(breaks = c("OR", "OP", "TR"), values = c("#0000e8", "#000000", "#ff0030")) + geom_point(position = dodge, size = 4.5, shape=21, fill="white") + geom_line(position = dodge)+ theme_classic()
a <- a + geom_errorbar(aes(ymin = verylowProf$lower, ymax =verylowProf$upper), width=0.1, size=1, linetype=1, position = dodge) #FUCK YEAH
a <- a + scale_y_continuous("RT(ms)",limits=c(530,710))
a <- a + theme(axis.title.y = element_text(size = rel(1.5), angle = 90)) + ggtitle('Very low proficiency \n1st quartile') + theme(plot.title = element_text(hjust = 0.5))
a <- a + theme(axis.text.y = element_text(angle = 00, hjust = 1, size=13, face = 'bold', colour = 'black'))
a <- a + labs(x = "UNRELATED       RELATED ") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
a <- a + theme(axis.title.x = element_text(size = rel(1), face = 'bold')) +  theme(legend.position="none")
a
ggsave("firsquartileProficiency.jpg", height=6, width=6, dpi = 2000)

#Secondo quartile
subset(df, df$overallProf> 40 & df$overallProf<=60)-> lowProf

dodge <- position_dodge(width = 0.1)
b <-ggplot(data = lowProf, aes(x = Relatedness, y = fit, col = Morphtype ,group = Morphtype)) + scale_colour_manual(breaks = c("OR", "OP", "TR"), values = c("#0000e8", "#000000", "#ff0030")) + geom_point(position = dodge, size = 4.5, shape=21, fill="white") + geom_line(position = dodge)+ theme_classic()
b <- b + geom_errorbar(aes(ymin = lowProf$lower, ymax = lowProf$upper), width=0.1, size=1, linetype=1, position = dodge) #FUCK YEAH
b <- b + scale_y_continuous(limits=c(530,710)) + ylab(NULL) + theme(axis.title.y=element_blank(), axis.text.y=element_blank())
b <- b + theme(axis.title.y = element_text(size = rel(1.5), angle = 90)) + ggtitle('Low proficiency \n2nd quartile') + theme(plot.title = element_text(hjust = 0.5))
b <- b + theme(axis.text.y = element_text(angle = 00, hjust = 1, size=13, face = 'bold', colour = 'black'))
b <- b + labs(x = "UNRELATED       RELATED ") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
b <- b + theme(axis.title.x = element_text(size = rel(1), face = 'bold')) +  theme(legend.position="none")
b
ggsave("secondquartileProficiency.jpg", height=6, width=6, dpi = 2000)

#Terzo quartile
subset(df, df$overallProf>60 & df$overallProf<=80)-> highProf

dodge <- position_dodge(width = 0.1)
c <-ggplot(data = highProf, aes(x = Relatedness, y = fit, col = Morphtype ,group = Morphtype)) + scale_colour_manual(breaks = c("OR", "OP", "TR"), values = c("#0000e8", "#000000", "#ff0030")) + geom_point(position = dodge, size = 4.5, shape=21, fill="white") + geom_line(position = dodge)+ theme_classic()
c <- c + geom_errorbar(aes(ymin = highProf$lower, ymax = highProf$upper), width=0.1, size=1, linetype=1, position = dodge) #FUCK YEAH
c <- c + scale_y_continuous(limits=c(530,710)) + ylab(NULL) + theme(axis.title.y=element_blank(), axis.text.y=element_blank())
c <- c + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))  + ggtitle('High proficiency \n3rd quartile') + theme(plot.title = element_text(hjust = 0.5))
c <- c + theme(axis.text.y = element_text(angle = 00, hjust = 1, size=13, face = 'bold', colour = 'black'))
c <- c + labs(x = "UNRELATED       RELATED ") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
c <- c + theme(axis.title.x = element_text(size = rel(1), face = 'bold')) +  theme(legend.position="none")
c
ggsave("thirdquartileProficiency.jpg", height=6, width=6, dpi = 2000)

#quarto quartile
subset(df, df$overallProf>80)-> veryhighProf

dodge <- position_dodge(width = 0.1)
d <-ggplot(data = veryhighProf, aes(x = Relatedness, y = fit, col = Morphtype ,group = Morphtype)) + scale_colour_manual(breaks = c("OR", "OP", "TR"), values = c("#0000e8", "#000000", "#ff0030")) + geom_point(position = dodge, size = 4.5, shape=21, fill="white") + geom_line(position = dodge)+ theme_classic()
d <- d + geom_errorbar(aes(ymin = veryhighProf$lower, ymax = veryhighProf$upper), width=0.1, size=1, linetype=1, position = dodge) #FUCK YEAH
d <- d + scale_y_continuous(limits=c(530,710)) + ylab(NULL) + theme(axis.title.y=element_blank(), axis.text.y=element_blank())
d <- d + theme(axis.title.y = element_text(size = rel(1.5), angle = 90)) + ggtitle('Very high proficiency \n4th quartile') + theme(plot.title = element_text(hjust = 0.5))
d <- d + theme(axis.text.y = element_text(angle = 00, hjust = 1, size=13, face = 'bold', colour = 'black'))
d <- d + labs(x = "UNRELATED       RELATED ") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
d <- d + theme(axis.title.x = element_text(size = rel(1), face = 'bold')) +  theme(legend.position="none")
d
ggsave("fourthquartileProficiency.jpg", height=6, width=6, dpi = 2000)

#plot all together
a<-ggarrange(a, b, c , d + rremove("x.text"),
          ncol = 4, nrow = 1, common.legend = TRUE, legend = "bottom")
ggplot() + aes(proficiency) + geom_histogram(binwidth= 4.5,bins = 17, colour="black", fill="white") + xlim(30,120) + ylim(0,14)


ggsave("overallProf.jpg", height=8, width=15)

#Let's see how many subjects were contained in these proficiency quantiles + post-hoc analysis
datartENG$Morphtype <- relevel(datartENG$Morphtype, "OR")
subset(datartENG, datartENG$overallProf<=quantile(datartENG$overallProf, .25))-> firstQ
unique(firstQ$Subject) #10 subjects
lmerFirstq <- lmer(-1000/rt ~ Relatedness * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = firstQ);
lmerFirstq1 <- lmer(-1000/rt ~ Relatedness * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(firstQ, abs(scale(resid(lmerFirstq)))<2));
summary(lmerFirstq1)

subset(datartENG, datartENG$overallProf>quantile(datartENG$overallProf, .25) & datartENG$overallProf<=quantile(datartENG$overallProf, .50))-> secondQ
unique(secondQ$Subject) #10 subjects
lmersecondQ <- lmer(-1000/rt ~ Relatedness * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = secondQ);
lmersecondQ1 <- lmer(-1000/rt ~ Relatedness * Morphtype  + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(secondQ, abs(scale(resid(lmersecondQ)))<2));
summary(lmersecondQ1)

subset(datartENG, datartENG$overallProf>quantile(datartENG$overallProf, .50) & datartENG$overallProf<=quantile(datartENG$overallProf, .75))-> thirdQ
unique(thirdQ$Subject) #9 subjects
lmerthirdQ <- lmer(-1000/rt ~ Relatedness * Morphtype * overallProf + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = thirdQ);
lmerthirdQ1 <- lmer(-1000/rt ~ Relatedness * Morphtype * overallProf + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(thirdQ, abs(scale(resid(lmerthirdQ)))<2));
summary(lmerthirdQ1)

subset(datartENG, datartENG$overallProf>quantile(datartENG$overallProf, .75))-> fourthQ
unique(fourthQ$Subject) #8 subjects
lmerfourthQ <- lmer(-1000/rt ~ Relatedness * Morphtype * overallProf + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = fourthQ);
lmerfourthQ1 <- lmer(-1000/rt ~ Relatedness * Morphtype * overallProf + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(thirdQ, abs(scale(resid(lmerfourthQ)))<2));
summary(lmerfourthQ1)

rm(dodge, aa, df)
rm(firstQ, lmerFirstq1, lmerFirstq, secondQ, lmersecondQ, lmersecondQ1, thirdQ, lmerthirdQ, lmerthirdQ1, fourthQ, lmerfourthQ, lmerfourthQ1)

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
hist(AoA, main = 'Age of acquisition', xlab = 'years', 
     ylab = 'Subjects', cex.lab=1.5, cex.axis=2, cex.sub=2)
dev.off()
shapiro.test(AoA) #normal distribution
rm(AoA, i)

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
proficiencylmer12 <- lmer(-1000/rt ~ Relatedness * AoA3 * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
proficiencylmer12b <- lmer(-1000/rt ~ Relatedness * AoA3 * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer12)))<2))
anova(proficiencylmer0, proficiencylmer12) 
anova(proficiencylmer12)
summary(datartENG$AoA3) #too unbalanced

#AoA5 "Sei cresciuta/o in un ambiente dove si parlano pi? lingue? 1: s? 2: no"
proficiencylmer13 <- lmer(-1000/rt ~ Relatedness * AoA5 * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
anova(proficiencylmer0, proficiencylmer13) 
anova(proficiencylmer13) 
#nothing significant here

#AoA6 "Se parli pi? lingue, qual ? la lingua che conosci meglio dopo la tua madrelingua? 1: eng 2: altro"
summary(datartENG$AoA6)
proficiencylmer14 <- lmer(-1000/rt ~ Relatedness * as.factor(AoA6) * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
anova(proficiencylmer0, proficiencylmer14) 
anova(proficiencylmer14) 

#AoA7 "Come valuteresti il livello di conoscenza della tua seconda lingua su una scala da 1 (base) a 5 (avanzato)?"
proficiencylmer15 <- lmer(-1000/rt ~ Relatedness * AoA7 * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
proficiencylmer15b <- lmer(-1000/rt ~ Relatedness * AoA7 * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer15)))<2))
anova(proficiencylmer0, proficiencylmer15) 
anova(proficiencylmer15b) #useless

#AoA8 "Qual è la terza lingua che conosci meglio dopo la tua madrelingua? 1: eng 2: altro 3: nessun'altra"
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
plotLMER.fnc(proficiencylmer8, withList = TRUE, fun = inv, pred = "Relatedness",control = list("overallProf", quantile(datartENG$overallProf, .25)), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='VERY LOW PROFICIENCY', ylimit = c(570,700), bty='l'); 
plotLMER.fnc(proficiencylmer8, withList = TRUE, fun = inv, pred = "Relatedness",control = list("overallProf", quantile(datartENG$overallProf, .50)), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='LOW PROFICIENCY', ylimit = c(570,700), bty='l');
plotLMER.fnc(proficiencylmer8, withList = TRUE, fun = inv, pred = "Relatedness",control = list("overallProf", quantile(datartENG$overallProf, .75)), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='HIGH PROFICIENCY', ylimit = c(570,700), bty='l');
plotLMER.fnc(proficiencylmer8, withList = TRUE, fun = inv, pred = "Relatedness",control = list("overallProf", quantile(datartENG$overallProf, 1)), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='VERY HIGH PROFICIENCY', ylimit = c(570,700), bty='l');
par(mfrow=c(1,1));

# run these lines to get the datartENG with the OSC paramenter:
subset(masterfileEng_OSC, masterfileEng_OSC$rt>350 & masterfileEng_OSC$rt<1800 & masterfileEng_OSC$Subject!=15 & masterfileEng_OSC$Subject!=22 & masterfileEng_OSC$Subject!=43 & masterfileEng_OSC$Lexicality=="WORD") -> dataAccENG
#Then, we select only right answers
subset(dataAccENG, dataAccENG$Accuracy==1)-> datartENG
#ok!

#controllo dell'assunto classico: 'Stems taken from the transparent sets have >OSC than OP or OR sets.'
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

#-----------------------------------------------------------------------------#
#                         GAM GRAPHs                                          #
#-----------------------------------------------------------------------------#
#from 648 to 653, I changed some feature of gam to get the gray scale of color, nothing important
# first get the definition of vis.gam
source(paste(localGitDir,'/data/analysis/mod.vis.gam.R', sep = ''))
newDef <- deparse(vis.gam)
# change the line defining the direction of the grey gradient
newDef[grep("gray\\(seq\\(",newDef)] <- "            pal <- gray(seq(0.9, 0.1, length = nCol))"
# then define a new function with this new definition
vis.gam2 <- eval(parse(text=newDef))

#plot con rt normali
gam1 <- gam(rt ~ s(OSC_Target, by = overallProf) + s(TrialCount) + s(Logfreq.Zipf.t) + s(Subject, bs = 're') + s(Target, bs = 're'), data = datartENG)
summary(gam1);
vis.gam2(gam1, view=c("OSC_Target","overallProf"), type="response", plot.type="contour", color="gray", main="", too.far=.1, xlab='OSC', ylab='Proficiency scores');

#plot con -1000/rt
gam2 <- gam(-1000/rt ~ s(OSC_Target, by = overallProf) + s(TrialCount) + s(Logfreq.Zipf.t) + s(Subject, bs = 're') + s(Target, bs = 're'), data = datartENG);
summary(gam2);
vis.gam2(gam2, view=c("OSC_Target","overallProf"), type="response", plot.type="contour", color="gray", main="", too.far=.1, xlab='OSC', ylab='Proficiency scores');


#For poster presentation
jpeg(filename = "C:/Users/eva_v/Documents/GitHub/M2-maskedprimingBilinguals/GAMplot.jpg", res=300, height=1654, width=3339)
mod.vis.gam(gam2, view=c("OSC_Target","overallProf"), type="response", plot.type="contour", color="gaypride", main="", 
            too.far=.1, xlab='OSC', ylab='PROFICIENCY', lwd = 2, font = 2, font.lab = 2, font.axis = 2, cex.axis =1.5,
            cex.lab=1.5);
dev.off()




