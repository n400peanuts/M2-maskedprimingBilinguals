#-----------------------------------------------------------------------------------------------#
# Data analysis                                                                                 #
# Morphological masked priming experiment on L1-ITA, L2-ENG bilingual speakers/readers          #
# Paper titled 'L2 form priming turns into morphological facilitation with growing proficiency' #
# Submitted to JML, September 2018                                                              #  
# Eva Viviani and Davide Crepaldi, SISSA                                                        #
#-----------------------------------------------------------------------------------------------#

#This script takes preprocessed data and produces all the analyses that are reported in the paper.

#clean the workspace
rm(list = ls());

#set your local working directory. This should be (and is assumed to be in the rest of the code) the highest point in your local gitHub folder:
localGitDir <- 'C:/Users/eva_v/Documents/GitHub/M2-maskedprimingBilinguals';
#localGitDir <- '~/Google Drive File Stream/My Drive/research/misc/m2-maskedMorphPrimingBilinguals/git/M2-maskedprimingBilinguals/';
setwd(localGitDir);

# This script works on the outcome of preProcessing.R, which you can upload here:
read.table(paste(localGitDir, '/preprocessedData.txt', sep = ''), header = T, sep='\t', dec='.') -> masterFile; 

head(masterFile);
summary(masterFile);

#--------------------#
#### participants ####
#--------------------#
temp <- unique(masterFile[,c('subject','age','gender','handedness')]);
length(unique(temp$subject)); #n of participants
summary(temp); #age, education and handedness
rm(temp);

#---------------#
#### stimuli ####
#---------------#
temp <- unique(masterFile[,c('target','prime','lexicality','morphType','relatedness','freqTarget','freqPrime','lengthTarget','lengthPrime','nTarget','nPrime','language')]); #we need to add orthographic overlap here
summary(temp);
rm(temp);

#target features, ita
aggregate(freqTarget ~ morphType, FUN=mean, data=subset(temp, lexicality=='word' & language=='ita'));
aggregate(freqTarget ~ morphType, FUN=sd, data=subset(temp, lexicality=='word' & language=='ita'));
#[...] complete with the code for the other variables, Eva
#prime features, ita
aggregate(freqPrime ~ relatedness+morphType, FUN=mean, data=subset(temp, lexicality=='word' & language=='ita'));
aggregate(freqPrime ~ relatedness+morphType, FUN=sd, data=subset(temp, lexicality=='word' & language=='ita'));
#[...] complete with the code for the other variables, Eva

# as per my email, distribute these commands down
library(languageR);
library(lmerTest);
library(ggplot2);
library(rms);
library(doBy);
library(mgcv);
library(effects);
library(plyr);
library(corrplot);
library(ggpubr);

# this should also go down, where relevant
inv <- function(x) {-1000/x};

#------------------------------#
#### outliers trimming, ita ####
#------------------------------#
subset(masterFile, language=="ita") -> masterFileIta;

# the following code generates target and sbj means and SDs, and the outlier graphs in the file 'ita.jpg'
sbj.id <- masterFileIta$subject;
acc <- masterFileIta$accuracy;
lexicality <- tolower(masterFileIta$lexicality);
target <- masterFileIta$target;
rt <- masterFileIta$rt;

source(paste(localGitDir, "tools/diagnostics.R", sep='')); 
outlierGraphStore <- '~/Desktop/';
diagnostics.f(rt = rt, acc = acc, sbj.id = sbj.id, target = target, lexicality = lexicality, paste(outlierGraphStore, "ita", sep=""));
rm(outlierGraphStore, rt, target, lexicality, acc, sbj.id);

# based on the graphs in 'ita.jpg': we exclude sbj 2 and 31 for an abnormal error rate on nonwords (<80%); and target words GUANO, UGGIA and VELLO with abnormally low accuracy (<60%).
# sbj 15 is excluded because s/he reported having seen the primes.
# individual RTs distribution seems fine, but let's check the tails more carefully:
hist(masterFileIta$rt[masterFileIta$rt<500], breaks=seq(0,500,20));
hist(masterFileIta$rt[masterFileIta$rt>1500], breaks=seq(1500,3000,50));
# based on these graph we cut distributions at 2500ms and 280ms

dataIta <- subset(masterFileIta, masterFileIta$rt>280 & masterFileIta$rt<2500 & masterFileIta$subject!=15 & masterFileIta$subject!=2 & masterFileIta$subject!=31 & masterFileIta$target!= "guano" & masterFileIta$target!= "uggia" & masterFileIta$target!= "vello" & masterFileIta$lexicality=="WORD" & masterFileIta$accuracy==1);
summary(dataIta);
nrow(dataIta);

#------------------------------#
#### outliers trimming, eng ####
#------------------------------#
subset(masterFile, language=="eng")-> masterFileEng;

# the following code generates target and sbj means and SDs, and the outlier graphs in the file 'ita.jpg'
sbj.id <- masterFileEng$subject;
acc <- masterFileEng$accuracy;
lexicality <- masterFileEng$lexicality;
lexicality <- tolower(masterFileEng$lexicality);
target <- masterFileEng$target;
rt <- masterFileEng$rt;

diagnostics.f(rt = rt, acc = acc, sbj.id = sbj.id, target = target, lexicality = lexicality, paste(outlierGraphStore, "eng", sep=""));
rm(outlierGraphStore, rt, target, lexicality, acc, sbj.id);

# sbj 26 likely confused YES/NO buttons. Let's check the frequency effect, just to confirm:
cor(masterFileEng[masterFileEng$subject==26 & masterFileEng$lexicality=='WORD', c('rt','freqTarget')], use = 'pairwise.complete.obs'); #unlikely s/he responded randomly. So, let's fix this:
library(car);
masterFileEng$accuracy[masterFileEng$subject==26] <- recode(masterFileEng$accuracy[masterFileEng$subject==26], "1=0;0=1");

# ok, we can now rerun diagnostics:
sbj.id <- masterFileEng$subject;
acc <- masterFileEng$accuracy;
lexicality <- masterFileEng$lexicality;
lexicality <- tolower(masterFileEng$lexicality);
target <- masterFileEng$target;
rt <- masterFileEng$rt;

diagnostics.f(rt = rt, acc = acc, sbj.id = sbj.id, target = target, lexicality = lexicality, paste(outlierGraphStore, "eng", sep=""));
rm(outlierGraphStore, rt, target, lexicality, acc, sbj.id);

# based on the graphs in 'eng.jpg': we exclude sbj 22 for a very atypical performance (average RT<200ms). We would exclude no target, even though some of them do elicit bad performance--the distribution is very continuous, no sign of glaring outliers. Plus, this is L2, so low performance is to be expected.
# sbj 15 and 43 reported having seen the primes.
# individual RTs distribution seems fine, but let's check the tails more carefully:
hist(masterFileEng$rt[masterFileEng$rt<500], breaks=seq(0,500,20)); #very continuous towards zero; perhaps some technical problem with the response box? Weird though, only in English. Anyway, deflection in the curve around 300ms, so let's cut there
hist(masterFileEng$rt[masterFileEng$rt>1500], breaks=seq(1500,5500,50)); #clear outliers over 2000ms

dataEng <- subset(masterFileEng, masterFileEng$rt>300 & masterFileEng$rt<2000 & masterFileEng$subject!=15 & masterFileEng$subject!=22 & masterFileEng$subject!=43 & masterFileEng$lexicality=="word" & masterFileEng$accuracy==1);
summary(dataEng);
nrow(dataEng);

#clean up the workspace
rm(masterFileIta, masterFileEng, diagnostics.f, sbj.diagnostics, target.diagnostics);

#-----------------#
#### raw means ####
#-----------------#
mean(masterFileIta$accuracy); mean(dataIta$rt);
mean(masterFileEng$accuracy); mean(dataEng$rt);

aggregate(rt ~ relatedness + morphType, FUN=mean, data=dataIta);
aggregate(rt ~ relatedness + morphType, FUN=mean, data=dataEng);

#-----------------------#
#### modelling, ita #####
#-----------------------#
library(lmerTest);
dataIta$morphType <- relevel(dataIta$morphType, "OR");
contrasts(dataIta$relatedness);
contrasts(dataIta$morphType);

italmer1 <- lmer(rt ~ trialCount + rotation + (1|subject) + (1|target), data= dataIta, REML = F);
summary(italmer1); #no real effect of totation, trialCount is borderline. For simplicity, we take 'em out both

italmer1 <- lmer(rt ~ freqTarget + lengthTarget + nT + (1|subject) + (1|target), data= dataIta, REML = F);
summary(italmer1); #only frequency seems to matter, take the length and N out
italmer2 <- lmer(rt ~ freqTarget + (1|subject) + (1|target), data= dataIta, REML = F);
anova(italmer2, italmer1); #all fine

italmer3 <- lmer(rt ~ relatedness * morphType + freqTarget + (1|subject) + (1|target), data= dataIta, REML = F);
anova(italmer3, italmer2); #rel and morphType contribute to goodness of fit
#we then refit with REML=T:
italmer3 <- lmer(rt ~ relatedness * morphType + freqTarget + (1|subject) + (1|target), data= dataIta, REML = T);
anova(italmer3); #the driving factor is rel-by-morphType interaction
summary(italmer3); #residuals are very non-normal; some authors believe mixed models are robust to this (e.g., Gelman and Hill, 2007), but normality of residuals is a general assumption of mixed models. So, just to stay on the safe side:
italmer3a <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + (1|subject) + (1|target), data= dataIta, REML = T);
anova(italmer3a); 
summary(italmer3a); #residuals are now symmetrical, and the pattern remains the same -- clean bill of health

#resistant to outliers?
italmer3b <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + (1|subject) + (1|target), data=subset(dataIta, abs(scale(resid(italmer3a)))<2), REML = T);
anova(italmer3b); 
summary(italmer3b); #no problem here too 

#transparent versus opaque condition:
dataIta$morphType <- relevel(dataIta$morphType, "OP");
contrasts(dataIta$morphType);
italmer3c <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + (1|subject) + (1|target), data= dataIta, REML = T);
summary(italmer3c);

#we report in the paper the complete model, with symmetric residuals:
anova(italmer3a); #here we get the overall significance of the interaction btw prime type and relatedness
summary(italmer3a); #here we get the parameters for contrasts across individual conditions
summary(italmer3c); #here we get the contrast between transparent and opaque pairs

#----------------------#
#### modelling, eng ####
#----------------------#
dataEng$morphType <- relevel(dataEng$morphType, "OR");
contrasts(dataEng$relatedness);
contrasts(dataEng$morphType);

englmer1 <- lmer(rt ~ trialCount + rotation + (1|subject) + (1|target), data= dataEng, REML = F);
summary(englmer1); #very similar to Italian. Again, for simplicity, we take out trialCount too

englmer1 <- lmer(rt ~ freqTarget + lengthTarget + nT + (1|subject) + (1|target), data= dataEng, REML = F);
summary(englmer1); #both frequency and length (length is interesting, it may indicate more serial, letter-based processing in L2)
englmer2 <- lmer(rt ~ freqTarget + lengthTarget + (1|subject) + (1|target), data= dataEng, REML = F);
anova(englmer1, englmer2); #all fine

englmer3 <- lmer(rt ~ relatedness * morphType + freqTarget + lengthTarget + (1|subject) + (1|target), data= dataEng, REML = F);
anova(englmer3, englmer2); #Rel and MorphType yield goodness of fit
#we thus refit with REML=T
englmer3 <- lmer(rt ~ relatedness * morphType + freqTarget + lengthTarget + (1|subject) + (1|target), data= dataEng, REML = T);
anova(englmer3); #the driving factor is again the rel-by-morphType interaction, but now much less strong
summary(englmer3); #residuals are very non-normal here too; so, we refit on inverse transformed RTs:
englmer3a <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + lengthTarget + (1|subject) + (1|target), data= dataEng, REML = T);
anova(englmer3a); 
summary(englmer3a); #residuals are now more symmetrical (more healthy model), and the interaction effect becomes much stronger. Let see what outliers do:
englmer3b <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + lengthTarget + (1|subject) + (1|target), data=subset(dataEng, abs(scale(resid(englmer3a)))<2.5), REML = T);
anova(englmer3b); 
summary(englmer3b); #the pattern remains the same, but the effect is even stronger -- quite unsurprinsgly for L2, there seem to be outliers that  

#transparent versus opaque condition:
dataEng$morphType <- relevel(dataEng$morphType, "OP");
contrasts(dataEng$morphType);
englmer3c <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + lengthTarget + (1|subject) + (1|target), data=dataEng, REML = T);
summary(italmer3c);

#as per Italian, we report in the paper the complete model, with symmetric residuals:
anova(englmer3a); #here we get the overall significance of the interaction btw prime type and relatedness
summary(englmer3a); #here we get the parameters for contrasts across individual conditions
summary(englmer3c); #here we get the contrast between transparent and opaque pairs

#---------------------------#
#### estimated RTs/plots ####
#---------------------------#
# we have to rethink about this figure, Eva (as per my email)

df <- effect("relatedness:morphType",italmer3b) ;
df <- as.data.frame(df);
df$fit <- inv(df$fit);
df$lower <- inv(df$lower);
df$upper <- inv(df$upper);
revalue(df$relatedness, c("ctrl"="unrelated"))-> df$relatedness
revalue(df$relatedness, c("rel"="related"))-> df$relatedness


dodge <- position_dodge(width = 0.1)
bb  <-ggplot(data = df, aes(x = relatedness, y = fit,group = morphType)) + geom_point(size = 2, position = dodge) + geom_line(aes(linetype=morphType), position = dodge)+ scale_linetype_manual(values=c("solid", "dotted", "dashed")) + theme_classic();
bb  <- bb + geom_pointrange(aes(ymin = df$lower, ymax = df$upper), position = dodge) 
bb  <- bb + scale_y_continuous("RT(ms)", limits = c(515,600)) 
bb  <- bb + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))
bb  <- bb + theme(axis.text.y = element_text(angle = 00, hjust = 1, size=10, colour = 'black'))
bb  <- bb + theme(axis.title.x = element_text(size = rel(1)))
bb <- bb + labs(title='L1 - Italian')
bb <- bb + theme(plot.title= element_text(angle = 00, hjust=0.5, size=15, face = 'bold', colour = 'black'))
bb <- bb + theme(legend.title = element_text(size = 8))
bb <- bb + theme(legend.text = element_text(size = 8))
bb
ggsave("itaplot.jpg")


df <- effect("relatedness:morphType",englmer3b) 
df <- as.data.frame(df)
df$fit <- inv(df$fit)
df$lower <- inv(df$lower)
df$upper <- inv(df$upper)
revalue(df$relatedness, c("ctrl"="unrelated"))-> df$relatedness
revalue(df$relatedness, c("rel"="related"))-> df$relatedness

dodge <- position_dodge(width = 0.1)
gg  <-ggplot(data = df, aes(x = relatedness, y = fit,group = morphType)) + geom_point(size = 2, position = dodge) + geom_line(aes(linetype=morphType), position = dodge)+ scale_linetype_manual(values=c("solid", "dotted", "dashed")) + theme_classic();
gg  <- gg + geom_pointrange(aes(ymin = df$lower, ymax = df$upper), position = dodge)
gg  <- gg + scale_y_continuous("RT(ms)", limits = c(515,675)) 
gg  <- gg + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))
gg  <- gg + theme(axis.text.y = element_text(angle = 00, hjust = 1, size=10, colour = 'black'))
gg  <- gg + theme(axis.title.x = element_text(size = rel(1)))
gg <- gg + labs(title='L2 - English')
gg <- gg + theme(plot.title= element_text(angle = 00, hjust=0.5, size=15, face = 'bold', colour = 'black'))
gg <- gg + theme(legend.title = element_text(size = 8))
gg <- gg + theme(legend.text = element_text(size = 8))
gg
ggsave("engplot.jpg")
rm(gg, bb, aa, dodge)

#----------------------------------#
#### cross language interaction ####
#----------------------------------#
rbind(dataEng, dataIta) -> crossExp;
summary(crossExp);

crosslmer <- lmer(-1000/rt ~ relatedness * morphType * language + freqTarget + lengthTarget + (1|subject) + (1|target), data = crossExp, REML = T);
anova(crosslmer);
crosslmerb <- lmer(-1000/rt ~ relatedness * morphType * language + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(crossExp, abs(scale(resid(crosslmer)))<2.5), REML = T);
anova(crosslmerb);

#--------------------------------------------------------#
#### proficiency scores, correlation and distribution ####
#--------------------------------------------------------#
#create a database with one line per ppt
pptFeatures <- unique(dataEng[,c('subject','age','gender','handedness','rotation','phonemicFluency', 'phonemicComprehension','morphComprehension','spelling','readingComprehension','vocabulary','oralComprehension','aoa1', 'aoa2', 'aoa3','aoa4','aoa5','aoa6')]);
summary(pptFeatures);

#correlation between the individual scores
round(cor(pptFeatures[,c(6:12)], use='pairwise.complete.obs'), digits=2);
sort( round(cor(pptFeatures[,c(6:12)], use='pairwise.complete.obs'), digits=2) );

#### New edit of the proficiency scores -- check it out! ####
#compute overall proficiency by summing by-sbj each individual score, standardized 
#here we standardize for each subtest and then we take the mean out of it. This procedure guarantees the same weight for every subtest.
pptFeatures$overallProf <- apply(scale(pptFeatures[,6:12]), 1, FUN=mean); 
#however a look at the distributions of the single subtests from lines (357:400) in this script reveals a non normal distributions.
#let's test it with the Shapiro-Wilk normality test that compares a normal distribution against ours.
#p-value > 0.05 implies that the distribution of the data are not significantly different from normal distribution. In other words, we can assume the normality.
library(dplyr);
shapiro.test(pptFeatures$phonemicFluency); #normal distribution
shapiro.test(pptFeatures$phonemicComprehension); #non-normal distribution
shapiro.test(pptFeatures$morphComprehension); #non-normal distribution
shapiro.test(pptFeatures$spelling); #non-normal distribution
shapiro.test(pptFeatures$readingComprehension); #non-normal distribution
shapiro.test(pptFeatures$vocabulary); #non-normal distribution
shapiro.test(pptFeatures$oralComprehension); #non-normal distribution
#5 out of 6 subtests are not normal. Z-scores transformation cannot be performed.

#other two measures can be used that are essentially the same: 
pptFeatures$overallProf2 <- rowSums(pptFeatures[,6:12]); #sum the scores per subj across all subtests
pptFeatures$overallProf3 <- scale(pptFeatures$overallProf2); #standardize the abovementioned sum
#we can test it separately in the three way model lmer to see the change
attach(pptFeatures);
par(mfrow=c(1,2));
hist(overallProf2, main = 'sum of the subtests', breaks = seq(0,112,8), cex.main=2, cex.lab=2, xlab = 'Scores', ylab = 'N participants', axes=F, col=grey(.80), border=grey(0));
hist(overallProf3, main = 'Z-scores of sum of the subtests', cex.main=2, cex.lab=2, xlab = 'Z-Scores', ylab = 'N participants', axes=F, col=grey(.80), border=grey(0));
par(mfrow=c(1,1));
detach(pptFeatures);

dataEng <- merge(dataEng, pptFeatures[,c('subject','overallProf', 'overallProf2', 'overallProf3')], by='subject');

#let's test these different Proficiency measures in the model
proficiencylmer8 <- lmer(-1000/rt ~ relatedness* morphType * overallProf + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML=F);
anova(proficiencylmer8);
#cut at 2 SD
proficiencylmer8a <- lmer(-1000/rt ~ relatedness* morphType * overallProf + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer8)))<2), REML=F);
anova(proficiencylmer8a);

proficiencylmer8b <- lmer(-1000/rt ~ relatedness * morphType * overallProf2 + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML=F);
anova(proficiencylmer8, proficiencylmer8b);
anova(proficiencylmer8b); 
#cut at 2 SD
proficiencylmer8c <- lmer(-1000/rt ~ relatedness* morphType * overallProf2 + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer8b)))<2), REML=F);
anova(proficiencylmer8c);
anova(proficiencylmer8c, proficiencylmer8a); #ah! lunghezze campionarie diverse
#this means that the cut at 2SD affects differently the 2 distributions. 

#this last check is silly because is the same distribution of 8b but standardized
#let's do it just to be sure that nothing changed
proficiencylmer8d <- lmer(-1000/rt ~ relatedness * morphType * overallProf3 + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML=F);
anova(proficiencylmer8, proficiencylmer8d);
anova(proficiencylmer8d); 
#cut at 2 SD
proficiencylmer8e <- lmer(-1000/rt ~ relatedness* morphType * overallProf3 + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer8d)))<2), REML=F);
anova(proficiencylmer8e);
#perfect

attach(pptFeatures);

#check score distributions
jpeg(filename = paste(localGitDir,'/fig_proficiencyScores2.jpg', sep = ''), res=300, height=2200, width=4400); #why 4339, Eva? Just go 4400
     
par(mfrow=c(2,4));
par(mar=c(5,5,4,.5)+.1);
par(lwd=2);

hist(phonemicFluency, breaks = seq(0,50,5), main = '(a) Phon Fluency', cex.main=2, xlab = 'Scores', ylab = 'N of participants', ylim=c(0,50), cex.lab=2, axes=F, col=grey(.80), border=grey(0));
axis(1, cex.axis=2);
axis(2, at=c(0,50), cex.axis=2, las=1);

hist(phonemicComprehension, breaks = seq(0,13,1), main = '(b) Phon Comprehension', cex.main=2, xlab = 'Scores', ylab = 'N of participants', ylim=c(0,50), cex.lab=2, axes=F, col=grey(.80), border=grey(0));
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


#-----------------------------#
#### proficiency modelling ####
#-----------------------------#
#we put overallProf in the main dataset
dataEng <- merge(dataEng, pptFeatures[,c('subject','overallProf3')], by='subject');

#phonemicFluency
proficiencylmer0 <- lmer(-1000/rt ~ relatedness * morphType + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML=F);
proficiencylmer1 <- lmer(-1000/rt ~ relatedness * morphType * phonemicFluency + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML=F);
anova(proficiencylmer0, proficiencylmer1);
anova(proficiencylmer1);

#check the effect of outliers
proficiencylmer1b <- lmer(-1000/rt ~ relatedness * morphType * phonemicFluency + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer1)))<2), REML=F);
anova(proficiencylmer1b); #wow, huge change! There must be many outliers, and really quite atypical. Which may be ok, it's L2 after all. Let see if the pattern holds with the other indexes, and then we'll take a closer look.

#phonemicComprehension
proficiencylmer2 <- lmer(-1000/rt ~ relatedness *  morphType * phonemicComprehension + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML=F);
anova(proficiencylmer0, proficiencylmer2);
anova(proficiencylmer2);

#check the effect of outliers
proficiencylmer2b <- lmer(-1000/rt ~ relatedness *  morphType * phonemicComprehension + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer2)))<2), REML=F);
anova(proficiencylmer2b);

#morphComprehension
proficiencylmer3 <- lmer(-1000/rt ~ relatedness *  morphType * morphComprehension + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML=F);
anova(proficiencylmer0,proficiencylmer3) ;
anova(proficiencylmer3);

#check the effect of outliers
proficiencylmer3b <- lmer(-1000/rt ~ relatedness *  morphType * morphComprehension + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer3)))<2), REML=F);
anova(proficiencylmer3b);

#spelling
proficiencylmer4 <- lmer(-1000/rt ~ relatedness * morphType * spelling + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML=F);
anova(proficiencylmer0, proficiencylmer4) ;
anova(proficiencylmer4);

#check the effect of outliers
proficiencylmer4b <- lmer(-1000/rt ~ relatedness * morphType * spelling + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer4)))<2), REML=F);
anova(proficiencylmer4b);

#readingComprehension
proficiencylmer5 <- lmer(-1000/rt ~ relatedness * morphType * readingComprehension + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML=F);
anova(proficiencylmer0, proficiencylmer5); 
anova(proficiencylmer5);

#check the effect of outliers
proficiencylmer5b <- lmer(-1000/rt ~ relatedness * morphType * readingComprehension + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer5)))<2), REML=F);
anova(proficiencylmer5b);

#vocabulary
proficiencylmer6 <- lmer(-1000/rt ~ relatedness * morphType * vocabulary + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML=F);
anova(proficiencylmer0,proficiencylmer6)
anova(proficiencylmer6);

#check the effect of outliers
proficiencylmer6b <- lmer(-1000/rt ~ relatedness * morphType * vocabulary + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer6)))<2), REML=F);
anova(proficiencylmer6b);

#oralComprehension
proficiencylmer7 <- lmer(-1000/rt ~ relatedness * morphType * oralComprehension + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML=F);
anova(proficiencylmer0,proficiencylmer7) ;
anova(proficiencylmer7);

#check the effect of outliers
proficiencylmer7b <- lmer(-1000/rt ~ relatedness * morphType * oralComprehension + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer7)))<2), REML=F);
anova(proficiencylmer7b);

#overall proficiency
proficiencylmer8 <- lmer(-1000/rt ~ relatedness * morphType * overallProf3 + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML=F);
anova(proficiencylmer0, proficiencylmer8);
anova(proficiencylmer8);

#check the effect of outliers
proficiencylmer8b <- lmer(-1000/rt ~ relatedness * morphType * overallProf3 + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer8)))<2));
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

#-----------------------------------------------------#
#### plot of the priming modulation by proficiency ####
#-----------------------------------------------------#
jpeg(filename = "C:/Users/Eva Viviani/Documents/GitHub/M2-maskedprimingBilinguals/Rplot.jpg", res=300, height=1654, width=3339)
par(mfrow=c(1,4));
a<- plotLMER.fnc(proficiencylmer8b, withList = TRUE, fun = inv, pred = "relatedness",control = list("overallProf", quantile(dataEng$overallProf, .1)), intr = list("morphType", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='VERY LOW PROFICIENCY', ylimit = c(570,700), bty='l'); 
b<- plotLMER.fnc(proficiencylmer8b, withList = TRUE, fun = inv, pred = "relatedness",control = list("overallProf", quantile(dataEng$overallProf, .5)), intr = list("morphType", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='LOW PROFICIENCY', ylimit = c(570,700), bty='l');
c<- plotLMER.fnc(proficiencylmer8b, withList = TRUE, fun = inv, pred = "relatedness",control = list("overallProf", quantile(dataEng$overallProf, .6)), intr = list("morphType", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='HIGH PROFICIENCY', ylimit = c(570,700), bty='l');
d<- plotLMER.fnc(proficiencylmer8b, withList = TRUE, fun = inv, pred = "relatedness",control = list("overallProf", quantile(dataEng$overallProf, .9)), intr = list("morphType", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='VERY HIGH PROFICIENCY', ylimit = c(570,700), bty='l');
par(mfrow=c(1,1));
dev.off()
#ah ah, bingo here!!!
#heavily modulated

#Primo quartile
df <- effect("relatedness:morphType:overallProf.y",proficiencylmer8b) 
df <- as.data.frame(df)
df$fit <- inv(df$fit)
df$lower <- inv(df$lower)
df$upper <- inv(df$upper)

subset(df, df$overallProf<=40)-> verylowProf
# plot using ggplot
dodge <- position_dodge(width = 0.1)
a <-ggplot(data = verylowProf, aes(x = relatedness, y = fit, col = morphType ,group = morphType)) + scale_colour_manual(breaks = c("OR", "OP", "TR"), values = c("#0000e8", "#000000", "#ff0030")) + geom_point(position = dodge, size = 4.5, shape=21, fill="white") + geom_line(position = dodge)+ theme_classic()
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
b <-ggplot(data = lowProf, aes(x = relatedness, y = fit, col = morphType ,group = morphType)) + scale_colour_manual(breaks = c("OR", "OP", "TR"), values = c("#0000e8", "#000000", "#ff0030")) + geom_point(position = dodge, size = 4.5, shape=21, fill="white") + geom_line(position = dodge)+ theme_classic()
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
c <-ggplot(data = highProf, aes(x = relatedness, y = fit, col = morphType ,group = morphType)) + scale_colour_manual(breaks = c("OR", "OP", "TR"), values = c("#0000e8", "#000000", "#ff0030")) + geom_point(position = dodge, size = 4.5, shape=21, fill="white") + geom_line(position = dodge)+ theme_classic()
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
d <-ggplot(data = veryhighProf, aes(x = relatedness, y = fit, col = morphType ,group = morphType)) + scale_colour_manual(breaks = c("OR", "OP", "TR"), values = c("#0000e8", "#000000", "#ff0030")) + geom_point(position = dodge, size = 4.5, shape=21, fill="white") + geom_line(position = dodge)+ theme_classic()
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
dataEng$morphType <- relevel(dataEng$morphType, "OR")
subset(dataEng, dataEng$overallProf<=quantile(dataEng$overallProf, .25))-> firstQ
unique(firstQ$subject) #10 subjects
lmerFirstq <- lmer(-1000/rt ~ relatedness * morphType + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = firstQ);
lmerFirstq1 <- lmer(-1000/rt ~ relatedness * morphType + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(firstQ, abs(scale(resid(lmerFirstq)))<2));
summary(lmerFirstq1)

subset(dataEng, dataEng$overallProf>quantile(dataEng$overallProf, .25) & dataEng$overallProf<=quantile(dataEng$overallProf, .50))-> secondQ
unique(secondQ$subject) #10 subjects
lmersecondQ <- lmer(-1000/rt ~ relatedness * morphType + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = secondQ);
lmersecondQ1 <- lmer(-1000/rt ~ relatedness * morphType  + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(secondQ, abs(scale(resid(lmersecondQ)))<2));
summary(lmersecondQ1)

subset(dataEng, dataEng$overallProf>quantile(dataEng$overallProf, .50) & dataEng$overallProf<=quantile(dataEng$overallProf, .75))-> thirdQ
unique(thirdQ$subject) #9 subjects
lmerthirdQ <- lmer(-1000/rt ~ relatedness * morphType * overallProf + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = thirdQ);
lmerthirdQ1 <- lmer(-1000/rt ~ relatedness * morphType * overallProf + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(thirdQ, abs(scale(resid(lmerthirdQ)))<2));
summary(lmerthirdQ1)

subset(dataEng, dataEng$overallProf>quantile(dataEng$overallProf, .75))-> fourthQ
unique(fourthQ$subject) #8 subjects
lmerfourthQ <- lmer(-1000/rt ~ relatedness * morphType * overallProf + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = fourthQ);
lmerfourthQ1 <- lmer(-1000/rt ~ relatedness * morphType * overallProf + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(thirdQ, abs(scale(resid(lmerfourthQ)))<2));
summary(lmerfourthQ1)

rm(dodge, aa, df)
rm(firstQ, lmerFirstq1, lmerFirstq, secondQ, lmersecondQ, lmersecondQ1, thirdQ, lmerthirdQ, lmerthirdQ1, fourthQ, lmerfourthQ, lmerfourthQ1)

#---------------------------------------------------------------------------------------------------#
#                                               END                                                 #
#---------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------#
#                                               aoa                                                 #
#---------------------------------------------------------------------------------------------------#
aoa <- NULL
for(i in unique(masterFile$subject)){
  aoa[i] <- unique(masterFile$aoa1[masterFile$subject==i])
}
jpeg(filename = "C:/Users/Eva Viviani/Documents/aoa.jpg", res=300, height=1654, width=2229)
hist(aoa, main = 'Age of acquisition', xlab = 'years', 
     ylab = 'subjects', cex.lab=1.5, cex.axis=2, cex.sub=2)
dev.off()
shapiro.test(aoa) #normal distribution
rm(aoa, i)

#aoa1 "A che et? hai iniziato ad essere esposto alla lingua inglese?"
proficiencylmer9 <- lmer(-1000/rt ~ relatedness * aoa1 * morphType + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng)
proficiencylmer10 <- lmer(-1000/rt ~ relatedness * aoa1  * morphType + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer9)))<2))
anova(proficiencylmer0, proficiencylmer9) #relatedness*aoa1 significativo
anova(proficiencylmer9) #anche qui senza filtrare c'? solo l'interazione tra relatedness*aoa1
anova(proficiencylmer10) #qui invece anche la 3way interaction
plotLMER.fnc(proficiencylmer10, fun = inv, pred = "relatedness",intr = list("aoa1", quantile(dataEng$aoa1), "end"), addlines = T)
plotLMER.fnc(proficiencylmer10, fun = inv, pred = "morphType",intr = list("aoa1", quantile(dataEng$aoa1), "end"), addlines = T)

fivenum(dataEng$aoa1)
par(mfrow=c(2,2));
plotLMER.fnc(proficiencylmer10, fun = inv, pred = "relatedness",control = list("aoa1", quantile(dataEng$aoa1, .01)), intr = list("morphType", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', main='.01');
plotLMER.fnc(proficiencylmer10, fun = inv, pred = "relatedness",control = list("aoa1", quantile(dataEng$aoa1, .25)), intr = list("morphType", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', main='.25', ylimit = c(570,630));
plotLMER.fnc(proficiencylmer10, fun = inv, pred = "relatedness",control = list("aoa1", quantile(dataEng$aoa1, .50)), intr = list("morphType", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', main='.50', ylimit = c(570,630));
plotLMER.fnc(proficiencylmer10, fun = inv, pred = "relatedness",control = list("aoa1", quantile(dataEng$aoa1, .75)), intr = list("morphType", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', main='.75', ylimit = c(570,630));
par(mfrow=c(1,1));

#semb

#aoa2 "quanto usi l'inglese nella tua vita quotidiana da 1 a 5?" 
proficiencylmer11 <- lmer(-1000/rt ~ relatedness * aoa2 * morphType + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng)
proficiencylmer11b <- lmer(-1000/rt ~ relatedness * aoa2 * morphType + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer11)))<2))
anova(proficiencylmer0, proficiencylmer11)
anova(proficiencylmer11) #qui significativo morphtype*aoa2 e relatedness*aoa2 separatamente. No 3way interaction.
anova(proficiencylmer11b) #qui scompare morphtype*aoa2, ma compare una 3way tra relatedness:Aoa2:morphtype, perch???
plotLMER.fnc(proficiencylmer11, fun = inv, pred = "relatedness",intr = list("aoa2", quantile(dataEng$aoa2), "end"), addlines = T)
plotLMER.fnc(proficiencylmer11, fun = inv, pred = "morphType",intr = list("aoa2", quantile(dataEng$aoa2), "end"), addlines = T)
#da considerare come proficiency

par(mfrow=c(2,2));
plotLMER.fnc(proficiencylmer11, fun = inv, pred = "relatedness",control = list("aoa2", 1), intr = list("morphType", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', main='.25');
plotLMER.fnc(proficiencylmer11, fun = inv, pred = "relatedness",control = list("aoa2", 3), intr = list("morphType", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', main='.50');
plotLMER.fnc(proficiencylmer11, fun = inv, pred = "relatedness",control = list("aoa2", 5), intr = list("morphType", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', main='.75');
par(mfrow=c(1,1));

#aoa3 "In quale contesto hai iniziato ad essere esposto alla lingua inglese? Casa o scuola?"
proficiencylmer12 <- lmer(-1000/rt ~ relatedness * aoa3 * morphType + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng)
proficiencylmer12b <- lmer(-1000/rt ~ relatedness * aoa3 * morphType + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer12)))<2))
anova(proficiencylmer0, proficiencylmer12) 
anova(proficiencylmer12)
summary(dataEng$aoa3) #too unbalanced

#aoa5 "Sei cresciuta/o in un ambiente dove si parlano pi? lingue? 1: s? 2: no"
proficiencylmer13 <- lmer(-1000/rt ~ relatedness * aoa5 * morphType + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng)
anova(proficiencylmer0, proficiencylmer13) 
anova(proficiencylmer13) 
#nothing significant here

#aoa6 "Se parli pi? lingue, qual ? la lingua che conosci meglio dopo la tua madrelingua? 1: eng 2: altro"
summary(dataEng$aoa6)
proficiencylmer14 <- lmer(-1000/rt ~ relatedness * as.factor(aoa6) * morphType + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng)
anova(proficiencylmer0, proficiencylmer14) 
anova(proficiencylmer14) 

#aoa7 "Come valuteresti il livello di conoscenza della tua seconda lingua su una scala da 1 (base) a 5 (avanzato)?"
proficiencylmer15 <- lmer(-1000/rt ~ relatedness * aoa7 * morphType + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng)
proficiencylmer15b <- lmer(-1000/rt ~ relatedness * aoa7 * morphType + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer15)))<2))
anova(proficiencylmer0, proficiencylmer15) 
anova(proficiencylmer15b) #useless

#aoa8 "Qual ? la terza lingua che conosci meglio dopo la tua madrelingua? 1: eng 2: altro 3: nessun'altra"
summary(as.factor(dataEng$aoa8))
proficiencylmer16 <- lmer(-1000/rt ~ relatedness * as.factor(aoa8) * morphType + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng)
proficiencylmer16b <- lmer(-1000/rt ~ relatedness * as.factor(aoa8) * morphType + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer16)))<2))
anova(proficiencylmer0, proficiencylmer16) 
anova(proficiencylmer16) #same as aoa7, nonsense this analysis
anova(proficiencylmer16b) #same as aoa7, nonsense this analysis


#aoa9 "Come valuteresti il livello di conoscenza della tua terza lingua su una scala da 1 (base) a 5 (avanzato)?"
proficiencylmer17 <- lmer(-1000/rt ~ as.factor(relatedness) * aoa9 * morphType + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng)
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
#rt ~ morphtype * overallProf e relatedness * overallProf + 3way interaction
proficiencylmer8 <- lmer(-1000/rt ~ relatedness * morphType * overallProf + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng);
anova(proficiencylmer8);
proficiencylmer8b <- lmer(-1000/rt ~ relatedness * morphType * overallProf + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer8)))<2));
anova(proficiencylmer8b); 
proficiencylmer8c <- lmer(-1000/rt ~ relatedness * morphType * overallProf + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer8)))<2.5));
anova(proficiencylmer8c);

par(mfrow=c(1,4));
plotLMER.fnc(proficiencylmer8, withList = TRUE, fun = inv, pred = "relatedness",control = list("overallProf", quantile(dataEng$overallProf, .25)), intr = list("morphType", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='VERY LOW PROFICIENCY', ylimit = c(570,700), bty='l'); 
plotLMER.fnc(proficiencylmer8, withList = TRUE, fun = inv, pred = "relatedness",control = list("overallProf", quantile(dataEng$overallProf, .50)), intr = list("morphType", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='LOW PROFICIENCY', ylimit = c(570,700), bty='l');
plotLMER.fnc(proficiencylmer8, withList = TRUE, fun = inv, pred = "relatedness",control = list("overallProf", quantile(dataEng$overallProf, .75)), intr = list("morphType", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='HIGH PROFICIENCY', ylimit = c(570,700), bty='l');
plotLMER.fnc(proficiencylmer8, withList = TRUE, fun = inv, pred = "relatedness",control = list("overallProf", quantile(dataEng$overallProf, 1)), intr = list("morphType", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='VERY HIGH PROFICIENCY', ylimit = c(570,700), bty='l');
par(mfrow=c(1,1));

#controllo dell'assunto classico: 'Stems taken from the transparent sets have >OSC than OP or OR sets.'
tapply(dataEng$oscTarget, dataEng$morphType, FUN = fivenum)

#rt ~ morphtype * overallProf SENZA relatedness e OSC. 
proficiencylmer9 <- lmer(-1000/rt ~ morphType * overallProf + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng);
anova(proficiencylmer9)
proficiencylmer9b <- lmer(-1000/rt ~ morphType * overallProf + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer9)))<2));
anova(proficiencylmer9b)

#rt ~ OSC_Primes * overallProf SENZA relatedness e morphtype 
proficiencylmer10 <- lmer(-1000/rt ~ overallProf * oscTarget + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng);
anova(proficiencylmer10)
proficiencylmer10b <- lmer(-1000/rt ~ overallProf * oscTarget + rcs(trialCount) + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer10)))<2));
anova(proficiencylmer10b)
#morphtype e OSC correlano molto con proficiency
#goodness of fit
round(cor(fitted(proficiencylmer10), -1000/dataEng$rt[!is.na(dataEng$oscTargetarget)])^2, digits=3)
round(cor(fitted(proficiencylmer9), -1000/dataEng$rt)^2, digits=3)

plotLMER.fnc(proficiencylmer10b, fun = inv, pred = "oscTarget", intr = list("overallProf", quantile(dataEng$overallProf, c(.25,.50,.75,1)), "end"), addlines = T, ylab='RT(ms)', bty='l'); 

#-----------------------------------------------------------------------------#
#                         GAM GRAPHs                                          #
#-----------------------------------------------------------------------------#
#from 648 to 653, I changed some feature of gam to get the gray scale of color, nothing important
# first get the definition of vis.gam
dataEng$overallProf2 <- apply(dataEng[29:35],1,FUN = sum); 

source(paste(localGitDir,'/analysis/mod.vis.gam.R', sep = ''))
newDef <- deparse(vis.gam)
# change the line defining the direction of the grey gradient
newDef[grep("gray\\(seq\\(",newDef)] <- "            pal <- gray(seq(0.9, 0.1, length = nCol))"
# then define a new function with this new definition
visGam <- eval(parse(text=newDef))

#plot con rt normali
gam1 <- gam(-1000/rt ~ s(oscTarget, by = overallProf) + s(trialCount) + s(freqTarget) + s(subject, bs = 're') + s(target, bs = 're'), data = dataEng)
summary(gam1);
vis.gam(gam1, view=c("oscTarget","overallProf"), type="response", plot.type="contour")
visGam(gam1, view=c("oscTarget","overallProf"), type="response", plot.type="contour", color="gray", main="", too.far=.1, xlab='OSC', ylab='Proficiency scores');

#plot con -1000/rt
gam2 <- gam(-1000/rt ~ s(oscTarget, by = overallProf2) + s(trialCount) + s(freqTarget) + s(subject, bs = 're') + s(target, bs = 're'), data = dataEng);
summary(gam2);
vis.gam(gam2, view=c("oscTarget","overallProf2"), type="response", plot.type="contour", color="gray", main="", too.far=.1, xlab='OSC', ylab='Proficiency scores');


#For poster presentation
jpeg(filename = "C:/Users/eva_v/Documents/GitHub/M2-maskedprimingBilinguals - Copia di Eva/GAMplot_NEW.jpg", res=300, height=1654, width=3339)
mod.vis.gam(gam2, view=c("oscTarget","overallProf2"), type="response", plot.type="contour", color="gaypride", main="", 
            too.far=.1, xlab='OSC', ylab='PROFICIENCY', lwd = 2, font = 2, font.lab = 2, font.axis = 2, cex.axis =1.5,
            cex.lab=1.5);
dev.off()


#tables for papers
library(tables)
#frequency
tabular( freqTarget+freqPrime ~ morphType * (mean+sd), data=masterFile[masterFile$language=='ita' & masterFile$relatedness=='ctrl' & masterFile$lexicality=='word',] );
tabular( nT+nP ~ morphType * (mean+sd), data=masterFile[masterFile$language=='ita' & masterFile$relatedness=='ctrl',] );

aggregate(rt ~ morphType + relatedness, data = masterFile[masterFile$language=='eng',], FUN = 'mean' )
