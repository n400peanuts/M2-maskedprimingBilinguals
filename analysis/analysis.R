# Analysis file for a masked priming experiment  #
# with bilingual ppts (L1 Italian, L2 English).  #
# The experiment is part of a paper titled X,    #  
# submitted for publication to  Y by Eva Viviani #
# and Davide Crepaldi, SISSA, July 2018.         #

# This script works on the outcome of preProcessing.R,
# which you can upload here:
rm(list = ls());
read.delim(); #fix this appropriately to import the pre-processed data, Eva, as per my email

head(masterFile);
summary(masterFile);

#set your local working directory
localGitDir <- '~/Google Drive File Stream/My Drive/research/misc/m2-maskedMorphPrimingBilinguals/git/M2-maskedprimingBilinguals/';
setwd(localGitDir);

#### participants ####
temp <- unique(masterFile[,c('Subject','Age','Gender','Handedness')]);
length(unique(temp$Subject)); #n of participants
summary(temp);
rm(temp);

#### stimuli ####
temp <- unique(masterFile[,c('Target','Prime.x','Lexicality','Morphtype','Primetype','Logfreq.Zipf.t', 'Lent', 'Lenp','Logfreq.Zipf.p','Nt','Np', 'Language')]);
summary(temp);
rm(temp);

# Eva, please add here the code for the stimuli features that we report in the paper

# as per my email, distribute these commands down
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

# this should also go down, where relevant
inv <- function(x) {-1000/x};

#### outliers trimming, ita ####
subset(masterFile, Language=="ita")-> masterfileIta;

# the following code generates target and sbj means and SDs, and the outlier graphs in the file 'ita.jpg'
sbj.id <- masterfileIta$Subject;
acc <- masterfileIta$Accuracy;
lexicality <- tolower(masterfileIta$Lexicality);
target <- masterfileIta$Target;
rt <- masterfileIta$rt;

source("diagnostics.R"); 
outlierGraphStore <- '~/Desktop/';
diagnostics.f(rt = rt, acc = acc, sbj.id = sbj.id, target = target, lexicality = lexicality, paste(outlierGraphStore, "ita", sep=""));
rm(outlierGraphStore, rt, target, lexicality, acc, sbj.id);

# based on the graphs in 'ita.jpg': we exclude sbj 2 and 31 for an abnormal error rate on nonwords (<80%); target words with abnormally low accuracy (<60%).
# sbj 15 is excluded because s/he reported having seen the primes.
# individual RTs distribution seems fine, but let's check the tails more carefully:
hist(masterfileIta$rt[masterfileIta$rt<500], breaks=seq(0,500,20));
hist(masterfileIta$rt[masterfileIta$rt>1500], breaks=seq(1500,3000,50));
# based on these graph we cut distributions at 2500ms and 280ms

#we don't analyse accuracy data, right? So we should take this out, I think.
dataAccITA <- subset(masterfileIta, masterfileIta$rt>280 & masterfileIta$rt<2500 & masterfileIta$Subject!=15 & masterfileIta$Subject!=2 & masterfileIta$Subject!=31 & masterfileIta$Target!= "guano" & masterfileIta$Target!= "uggia" & masterfileIta$Target!= "vello" & masterfileIta$Lexicality=="WORD");
summary(dataAccITA);

datartITA <- subset(dataAccITA, dataAccITA$Accuracy==1);
summary(datartITA);
nrow(datartITA);

#### outliers trimming, eng ####
subset(masterFile, Language=="eng")-> masterfileEng;

# the following code generates target and sbj means and SDs, and the outlier graphs in the file 'ita.jpg'
sbj.id <- masterfileEng$Subject
acc <- masterfileEng$Accuracy
lexicality <- masterfileEng$Lexicality
lexicality <- tolower(masterfileEng$Lexicality)
target <- masterfileEng$Target
rt <- masterfileEng$rt

diagnostics.f(rt = rt, acc = acc, sbj.id = sbj.id, target = target, lexicality = lexicality, paste(outlierGraphStore, "eng", sep=""));
rm(outlierGraphStore, rt, target, lexicality, acc, sbj.id);

# sbj 26 likely confused YES/NO buttons. Let's check the frequency effect, just to confirm:
cor(masterfileEng[masterfileEng$Subject==26 & masterfileEng$Lexicality=='WORD', c('rt','Logfreq.Zipf.t')], use = 'pairwise.complete.obs'); #unlikely s/he responded randomly. So, let's fix this:
library(car);
masterfileEng$Accuracy[masterfileEng$Subject==26] <- recode(masterfileEng$Accuracy[masterfileEng$Subject==26], "1=0;0=1");
# rerun diagnostics:
sbj.id <- masterfileEng$Subject
acc <- masterfileEng$Accuracy
lexicality <- masterfileEng$Lexicality
lexicality <- tolower(masterfileEng$Lexicality)
target <- masterfileEng$Target
rt <- masterfileEng$rt

diagnostics.f(rt = rt, acc = acc, sbj.id = sbj.id, target = target, lexicality = lexicality, paste(outlierGraphStore, "eng", sep=""));
rm(outlierGraphStore, rt, target, lexicality, acc, sbj.id);

# based on the graphs in 'eng.jpg': we exclude sbj 22 for a very atypical performance (average RT<200ms). We would exclude no target, even though some of them do elicit bad performance--the distribution is very continuous, no sign of glaring outliers. Plus, this is L2, so low performance is to be expected.
# sbj 15 and 43 reported having seen the primes.
# individual RTs distribution seems fine, but let's check the tails more carefully:
hist(masterfileEng$rt[masterfileEng$rt<500], breaks=seq(0,500,20)); #very continuous towards zero; perhaps some technical problem with the response box? Weird though, only in English. Anyway, deflection in the curve around 300ms, so let's cut there
hist(masterfileEng$rt[masterfileEng$rt>1500], breaks=seq(1500,5500,50)); #clear outliers over 2000ms

dataAccENG <- subset(masterfileEng, masterfileEng$rt>300 & masterfileEng$rt<2000 & masterfileEng$Subject!=15 & masterfileEng$Subject!=22 & masterfileEng$Subject!=43 & masterfileEng$Lexicality=="WORD");
summary(dataAccENG);

#Then, we select only right answers
datartENG <- subset(dataAccENG, dataAccENG$Accuracy==1);
summary(datartENG);
nrow(datartENG);

#clean up the workspace
rm(masterfileIta, masterfileEng, diagnostics.f, sbj.diagnostics, target.diagnostics);

#### raw means ####
mean(dataAccITA$Accuracy); mean(datartITA$rt);
mean(dataAccENG$Accuracy); mean(datartENG$rt);

aggregate(rt ~ Primetype + Morphtype, FUN=mean, data=datartITA);
aggregate(rt ~ Primetype + Morphtype, FUN=mean, data=datartENG);

#### modelling, ita #####
library(lmerTest);
datartITA$Morphtype <- relevel(datartITA$Morphtype, "OR");
contrasts(datartITA$Relatedness);
contrasts(datartITA$Morphtype);

italmer1 <- lmer(rt ~ TrialCount + Rotation.x + (1|Subject) + (1|Target), data= datartITA, REML = F);
summary(italmer1); #no real effect of Rotation, TrialCount is borderline. For simplicity, we take 'em out both

italmer1 <- lmer(rt ~ Logfreq.Zipf.t + Lent + Nt + (1|Subject) + (1|Target), data= datartITA, REML = F)
summary(italmer1); #only frequency seems to matter, take the length and N out
italmer2 <- lmer(rt ~ Logfreq.Zipf.t + (1|Subject) + (1|Target), data= datartITA, REML = F)
anova(italmer2, italmer1); #all fine

italmer3 <- lmer(rt ~ Relatedness * Morphtype + Logfreq.Zipf.t + (1|Subject) + (1|Target), data= datartITA, REML = F)
anova(italmer3, italmer2); #rel and morphType contribute to goodness of fit
#we then refit with REML=T:
italmer3 <- lmer(rt ~ Relatedness * Morphtype + Logfreq.Zipf.t + (1|Subject) + (1|Target), data= datartITA, REML = T)
anova(italmer3); #the driving factor is rel-by-morphType interaction
summary(italmer3); #residuals are very non-normal; some authors believe mixed models are robust to this (e.g., Gelman and Hill, 2007), but normality of residuals is a general assumption of mixed models. So, just to stay on the safe side:
italmer3a <- lmer(-1000/rt ~ Relatedness * Morphtype + Logfreq.Zipf.t + (1|Subject) + (1|Target), data= datartITA, REML = T)
anova(italmer3a); 
summary(italmer3a); #residuals are now symmetrical, and the pattern remains the same -- clean bill of health

#resistant to outliers?
italmer3b <- lmer(-1000/rt ~ Relatedness * Morphtype + Logfreq.Zipf.t + (1|Subject) + (1|Target), data=subset(datartITA, abs(scale(resid(italmer3a)))<2), REML = T);
anova(italmer3b); 
summary(italmer3b); #no problem here too 

#transparent versus opaque condition:
datartITA$Morphtype <- relevel(datartITA$Morphtype, "OP");
contrasts(datartITA$Morphtype);
italmer3c <- lmer(-1000/rt ~ Relatedness * Morphtype + Logfreq.Zipf.t + (1|Subject) + (1|Target), data= datartITA, REML = T)
summary(italmer3c);

#we report in the paper the complete model, with symmetric residuals:
anova(italmer3a); #here we get the overall significance of the interaction btw prime type and relatedness
summary(italmer3a); #here we get the parameters for contrasts across individual conditions
summary(italmer3c); #here we get the contrast between transparent and opaque pairs

#### modelling, eng ####
datartENG$Morphtype <- relevel(datartENG$Morphtype, "OR");
contrasts(datartENG$Relatedness);
contrasts(datartENG$Morphtype);

englmer1 <- lmer(rt ~ TrialCount + Rotation.x + (1|Subject) + (1|Target), data= datartENG, REML = F)
summary(englmer1) #very similar to Italian. Again, for simplicity, we take out TrialCount too

englmer1 <- lmer(rt ~ Logfreq.Zipf.t + Lent + Nt + (1|Subject) + (1|Target), data= datartENG, REML = F)
summary(englmer1) #both frequency and length (length is interesting, it may indicate more serial, letter-based processing in L2)
englmer2 <- lmer(rt ~ Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data= datartENG, REML = F)
anova(englmer1, englmer2); #all fine

englmer3 <- lmer(rt ~ Relatedness * Morphtype + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data= datartENG, REML = F)
anova(englmer3, englmer2) #Rel and MorphType yield goodness of fit
#we thus refit with REML=T
englmer3 <- lmer(rt ~ Relatedness * Morphtype + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data= datartENG, REML = T)
anova(englmer3); #the driving factor is again the rel-by-morphType interaction, but now much less strong
summary(englmer3); #residuals are very non-normal here too; so, we refit on inverse transformed RTs:
englmer3a <- lmer(-1000/rt ~ Relatedness * Morphtype + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data= datartENG, REML = T)
anova(englmer3a); 
summary(englmer3a); #residuals are now more symmetrical (more healthy model), and the interaction effect becomes much stronger. Let see what outliers do:
englmer3b <- lmer(-1000/rt ~ Relatedness * Morphtype + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data=subset(datartENG, abs(scale(resid(englmer3a)))<2.5), REML = T);
anova(englmer3b); 
summary(englmer3b); #the pattern remains the same, but the effect is even stronger -- quite unsurprinsgly for L2, there seem to be outliers that  

#transparent versus opaque condition:
datartENG$Morphtype <- relevel(datartENG$Morphtype, "OP");
contrasts(datartENG$Morphtype);
englmer3c <- lmer(-1000/rt ~ Relatedness * Morphtype + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data=datartENG, REML = T)
summary(italmer3c);

#as per Italian, we report in the paper the complete model, with symmetric residuals:
anova(englmer3a); #here we get the overall significance of the interaction btw prime type and relatedness
summary(englmer3a); #here we get the parameters for contrasts across individual conditions
summary(englmer3c); #here we get the contrast between transparent and opaque pairs

#### estimated RTs/plots ####
# we have to rethink about this figure, Eva (as per my email)
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

#### cross language interaction ####
rbind(datartENG[,1:44], datartITA) -> crossExp;
summary(crossExp);

crosslmer <- lmer(-1000/rt ~ Relatedness * Morphtype * Language + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = crossExp, REML = T);
anova(crosslmer);
crosslmerb <- lmer(-1000/rt ~ Relatedness * Morphtype * Language + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(crossExp, abs(scale(resid(crosslmer)))<2.5), REML = T);
anova(crosslmerb);

#### proficiency scores, correlation and distribution ####
#create a database with one line per ppt
pptFeatures <- unique(datartENG[,c('Subject','Age','Gender','Handedness','Rotation.x','phoneticFluency', 'phoneticComprehension','morphComprehension','spelling','readingComprehension','vocabulary','oralComprehension','AoA1', 'AoA2', 'AoA3','AoA4','AoA5','AoA6','AoA7','AoA8','AoA9')]);
summary(pptFeatures);

#correlation between the individual scores
round(cor(pptFeatures[,c(6:12)], use='pairwise.complete.obs'), digits=2);
sort( round(cor(pptFeatures[,c(6:12)], use='pairwise.complete.obs'), digits=2) );

#compute overall proficiency by summing by-sbj each individual score, standardized 
pptFeatures$overallProf <- apply(scale(pptFeatures[,6:12]), 1, FUN=mean);

attach(pptFeatures);

#check score distributions
jpeg(filename = paste(localGitDir,'paper/fig_proficiencyScores.jpg', res=300, height=2200, width=4339); #why 4339, Eva? Just go 4400
     
par(mfrow=c(2,4));
par(mar=c(5,5,4,.5)+.1);
par(lwd=2);

hist(phoneticFluency, breaks = seq(0,50,5), main = '(a) Phon Fluency', cex.main=2, xlab = 'Scores', ylab = 'N of participants', ylim=c(0,50), cex.lab=2, axes=F, col=grey(.80), border=grey(0));
axis(1, cex.axis=2);
axis(2, at=c(0,50), cex.axis=2, las=1);

hist(phoneticComprehension, breaks = seq(0,13,1), main = '(b) Phon Comprehension', cex.main=2, xlab = 'Scores', ylab = 'N of participants', ylim=c(0,50), cex.lab=2, axes=F, col=grey(.80), border=grey(0));
axis(1, cex.axis=2);
axis(2, at=c(0,50), cex.axis=2, las=1);

hist(morphComprehension, breaks = seq(0,10,1), main = '(c) Morph Awareness', cex.main=2, xlab = 'Scores', ylab = 'N of participants', ylim=c(0,50), cex.lab=2, axes=F, col=grey(.80), border=grey(0));
axis(1, cex.axis=2);
axis(2, at=c(0,50), cex.axis=2, las=1);

hist(spelling, breaks = seq(0,20,2), main = '(d) Spelling', cex.main=2, xlab = 'Scores', ylab = 'N of participants', ylim=c(0,50), cex.lab=2, axes=F, col=grey(.80), border=grey(0));
axis(1, cex.axis=2);
axis(2, at=c(0,50), cex.axis=2, las=1);

hist(readingComprehension, breaks = seq(0,7,1), main = '(e) Read Comprehension', cex.main=2, xlab = 'Scores', ylab = 'N of participants', ylim=c(0,50), cex.lab=2, axes=F, col=grey(.80), border=grey(0));
axis(1, cex.axis=2);
axis(2, at=c(0,50), cex.axis=2, las=1);

hist(vocabulary, breaks = seq(0,20,2), main = '(f) Vocabulary', cex.main=2, xlab = 'Scores', ylab = 'N of participants', ylim=c(0,50), cex.lab=2, axes=F, col=grey(.80), border=grey(0));
axis(1, cex.axis=2);
axis(2, at=c(0,50), cex.axis=2, las=1);

hist(oralComprehension, breaks = seq(0,6,1), main = '(g) Oral comprehension', cex.main=2, xlab = 'Scores', ylab = 'N of participants', ylim=c(0,50), cex.lab=2, axes=F, col=grey(.80), border=grey(0));
axis(1, cex.axis=2);
axis(2, at=c(0,50), cex.axis=2, las=1);

hist(overallProf, breaks = seq(-2.5,2,.5), main = '(h) Overall Proficiency', cex.main=2, xlab = 'Scores', ylab = 'N of participants', ylim=c(0,50), cex.lab=2, axes=F, col=grey(.80), border=grey(0));
axis(1, cex.axis=2);
axis(2, at=c(0,50), cex.axis=2, las=1);

par(mfrow=c(1,1));

dev.off();

detach(pptFeatures);

#### proficiency modelling ####
#we put overallProf in the main dataset
datartENG <- merge(datartENG, pptFeatures[,c('Subject','overallProf')], by='Subject');

#phonemicFluency
proficiencylmer0 <- lmer(-1000/rt ~ Relatedness * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG, REML=F);
proficiencylmer1 <- lmer(-1000/rt ~ Relatedness * Morphtype * phoneticFluency + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG, REML=F);
anova(proficiencylmer0, proficiencylmer1)
anova(proficiencylmer1)

#check the effect of outliers
proficiencylmer1b <- lmer(-1000/rt ~ Relatedness * Morphtype * phoneticFluency + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer1)))<2), REML=F);
anova(proficiencylmer1b); #wow, huge change! There must be many outliers, and really quite atypical. Which may be ok, it's L2 after all. Let see if the pattern holds with the other indexes, and then we'll take a closer look.

#phonemicComprehension
proficiencylmer2 <- lmer(-1000/rt ~ Relatedness *  Morphtype * phoneticComprehension + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG, REML=F)
anova(proficiencylmer0, proficiencylmer2)
anova(proficiencylmer2)

#check the effect of outliers
proficiencylmer2b <- lmer(-1000/rt ~ Relatedness *  Morphtype * phoneticComprehension + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer2)))<2), REML=F)
anova(proficiencylmer2b)

#morphComprehension
proficiencylmer3 <- lmer(-1000/rt ~ Relatedness *  Morphtype * morphComprehension + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG, REML=F)
anova(proficiencylmer0,proficiencylmer3) 
anova(proficiencylmer3)

#check the effect of outliers
proficiencylmer3b <- lmer(-1000/rt ~ Relatedness *  Morphtype * morphComprehension + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer3)))<2), REML=F)
anova(proficiencylmer3b)

#spelling
proficiencylmer4 <- lmer(-1000/rt ~ Relatedness * Morphtype * spelling + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG, REML=F)
anova(proficiencylmer0, proficiencylmer4) 
anova(proficiencylmer4)

#check the effect of outliers
proficiencylmer4b <- lmer(-1000/rt ~ Relatedness * Morphtype * spelling + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer4)))<2), REML=F)
anova(proficiencylmer4b)

#readingComprehension
proficiencylmer5 <- lmer(-1000/rt ~ Relatedness * Morphtype * readingComprehension + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG, REML=F)
anova(proficiencylmer0, proficiencylmer5) 
anova(proficiencylmer5)

#check the effect of outliers
proficiencylmer5b <- lmer(-1000/rt ~ Relatedness * Morphtype * readingComprehension + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer5)))<2), REML=F)
anova(proficiencylmer5b)

#vocabulary
proficiencylmer6 <- lmer(-1000/rt ~ Relatedness * Morphtype * vocabulary + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG, REML=F)
anova(proficiencylmer0,proficiencylmer6)
anova(proficiencylmer6)

#check the effect of outliers
proficiencylmer6b <- lmer(-1000/rt ~ Relatedness * Morphtype * vocabulary + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer6)))<2), REML=F)
anova(proficiencylmer6b)

#oralComprehension
proficiencylmer7 <- lmer(-1000/rt ~ Relatedness * Morphtype * oralComprehension + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG, REML=F)
anova(proficiencylmer0,proficiencylmer7) 
anova(proficiencylmer7)

#check the effect of outliers
proficiencylmer7b <- lmer(-1000/rt ~ Relatedness * Morphtype * oralComprehension + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer7)))<2), REML=F)
anova(proficiencylmer7b)

#overall proficiency
proficiencylmer8 <- lmer(-1000/rt ~ Relatedness * Morphtype * overallProf.y + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG, REML=F);
anova(proficiencylmer0, proficiencylmer8);
anova(proficiencylmer8);

#check the effect of outliers
proficiencylmer8b <- lmer(-1000/rt ~ Relatedness * Morphtype * overallProf.y + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer8)))<2));
anova(proficiencylmer8b); #huge change here too. Let's take a closer look:
#it's about many outliers?
summary(italmer3a)[[3]];
summary(italmer3b)[[3]]; #in the ITA set, we lost ~550 datapoints, ~5%
summary(proficiencylmer8)[[3]];
summary(proficiencylmer8b)[[3]]; #in these models, we lose ~400 datapoints, which is again ~5%. So, no more outliers here than in the ITA data

#is it about where in the fitted RT distribution these outliers are?
par(mfrow=c(1,2));
plot(fitted(italmer3a), scale(resid(italmer3a)), pch=19, ylim=c(-6,4));
plot(fitted(proficiencylmer8), scale(resid(proficiencylmer8)), pch=19, ylim=c(-6,4));
par(mfrow=c(1,1)); #not really -- the graphs look quite similar across languages

#so, it seems this is genuinely about some outliers killing the effect.

#### plot of the priming modulation by proficiency ####
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
df <- effect("Relatedness:Morphtype:overallProf.y",proficiencylmer8b) 
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




