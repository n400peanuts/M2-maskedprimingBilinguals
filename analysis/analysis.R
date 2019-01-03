#-----------------------------------------------------------------------------------------------#
# Data analysis                                                                                 #
# Morphological masked priming experiment on L1-ITA, L2-ENG bilingual speakers/readers          #
# Paper titled 'L2 form priming turns into morphological facilitation with growing proficiency' #
# Submitted to JML, September 2018                                                              #  
# Eva Viviani and Davide Crepaldi, SISSA                                                        #
#-----------------------------------------------------------------------------------------------#

#This script takes preprocessed data and produces all the analyses that are reported in the paper.

#--------------------------------------#
#### clean WS, set WD and load data ####
#--------------------------------------#
rm(list = ls());

#set your local working directory. This should be (and is assumed to be in the rest of the code) the highest point in your local gitHub folder:
localGitDir <- 'C:/Users/eva_v/Documents/GitHub/M2-maskedprimingBilinguals';
#localGitDir <- '~/Google Drive File Stream/My Drive/research/misc/m2-maskedMorphPrimingBilinguals/git/M2-maskedprimingBilinguals/';
setwd(localGitDir);

# This script works on the outcome of preProcessing.R, which you can upload here:
read.table(paste(localGitDir, '/preprocessedData.txt', sep = ''), header = T, sep='\t', dec='.') -> masterFile; 

head(masterFile);
summary(masterFile);

#------------------------------------------#
#### load packages and create functions ####
#------------------------------------------#
library(languageR);
library(ggplot2);
library(rms);
library(doBy);
library(mgcv);
library(effects);
library(plyr);
library(corrplot);

inv <- function(x) {-1000/x};

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
temp <- unique(masterFile[,c('target','prime','lexicality','morphType','relatedness','freqTarget','freqPrime','lengthTarget','lengthPrime','nTarget','nPrime','language')]); #WE NEED TO ADD ORTHOGRAPHIC OVERLAP HERE
summary(temp);

#target features, ita
aggregate(freqTarget ~ morphType, FUN=mean, data=subset(temp, lexicality=='word' & language=='ita'));
aggregate(freqTarget ~ morphType, FUN=sd, data=subset(temp, lexicality=='word' & language=='ita'));
aggregate(lengthTarget ~ morphType, FUN=mean, data=subset(temp, lexicality=='word' & language=='ita'));
aggregate(lengthTarget ~ morphType, FUN=sd, data=subset(temp, lexicality=='word' & language=='ita'));
aggregate(nTarget ~ morphType, FUN=mean, data=subset(temp, lexicality=='word' & language=='ita'));
aggregate(nTarget ~ morphType, FUN=sd, data=subset(temp, lexicality=='word' & language=='ita'));

#prime features, ita
aggregate(freqPrime ~ relatedness+morphType, FUN=mean, data=subset(temp, lexicality=='word' & language=='ita'));
aggregate(freqPrime ~ relatedness+morphType, FUN=sd, data=subset(temp, lexicality=='word' & language=='ita'));
aggregate(lengthPrime ~ relatedness+morphType, FUN=mean, data=subset(temp, lexicality=='word' & language=='ita'));
aggregate(lengthPrime ~ relatedness+morphType, FUN=sd, data=subset(temp, lexicality=='word' & language=='ita'));
aggregate(nPrime ~ relatedness+morphType, FUN=mean, data=subset(temp, lexicality=='word' & language=='ita'));
aggregate(nPrime ~ relatedness+morphType, FUN=sd, data=subset(temp, lexicality=='word' & language=='ita'));

rm(temp);
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

dataItaAcc <- subset(masterFileIta, lexicality=="word");
dataItaTemp <- subset(dataItaAcc, accuracy==1);
dataIta <- subset(dataItaTemp, rt>280 & rt<2500 & subject!=15 & subject!=2 & subject!=31 & target!= "guano" & target!= "uggia" & target!= "vello");
nrow(dataItaTemp)-nrow(dataIta);
(nrow(dataItaTemp)-nrow(dataIta)) / nrow(dataItaTemp);
nrow(dataIta)

summary(dataIta);

#------------------------------#
#### outliers trimming, eng ####
#------------------------------#
subset(masterFile, language=="eng") -> masterFileEng;

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
cor(masterFileEng[masterFileEng$subject==26 & masterFileEng$lexicality=='word', c('rt','freqTarget')], use = 'pairwise.complete.obs'); #unlikely s/he responded randomly. So, let's fix this:
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

dataEngAcc <- subset(masterFileEng, lexicality=="word");
dataEngTemp <- subset(dataEngAcc, accuracy==1);
dataEng <- subset(dataEngTemp, rt>300 & rt<2000 & subject!=15 & subject!=22 & subject!=43);
nrow(dataEngTemp)-nrow(dataEng);
(nrow(dataEngTemp)-nrow(dataEng)) / nrow(dataEngTemp);
nrow(dataEng);

summary(dataEng);

#clean up the workspace
rm(masterFileIta, masterFileEng, diagnostics.f, sbj.diagnostics, target.diagnostics);

#-----------------#
#### raw means ####
#-----------------#
mean(dataItaAcc$accuracy); mean(dataIta$rt);
mean(dataEngAcc$accuracy); mean(dataEng$rt);

aggregate(rt ~ relatedness + morphType, FUN=mean, data=dataIta);
aggregate(rt ~ relatedness + morphType, FUN=mean, data=dataEng);

#-----------------------#
#### modelling, ita #####
#-----------------------#
library(lmerTest);
dataIta$morphType <- relevel(dataIta$morphType, "or");
contrasts(dataIta$relatedness);
contrasts(dataIta$morphType);

italmer0 <- lmer(-1000/rt ~ 1 + (1|subject) + (1|target), data= dataIta, REML = F);

italmer1 <- lmer(-1000/rt ~ trialCount + rotation + (1|subject) + (1|target), data= dataIta, REML = F);
anova(italmer0, italmer1); #no effect here

italmer1 <- lmer(-1000/rt ~ freqTarget + lengthTarget + nTarget + (1|subject) + (1|target), data= dataIta, REML = F);
anova(italmer0, italmer1); #strong improvement in GoF
anova(italmer1); #to which only freq seems to contribute

#we introduce the varibles of interest now
italmer2 <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + (1|subject) + (1|target), data= dataIta, REML = T);
summary(italmer2); #residuals are rather symmetrical -- clean bill of health
#outliers trimming, a la Baayen (2008)
italmer2b <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + (1|subject) + (1|target), data=subset(dataIta, abs(scale(resid(italmer2)))<2.5), REML = T);
anova(italmer2b); #here we get the overall significance of the interaction btw prime type and relatedness
summary(italmer2b);#here we get the parameters for contrasts across individual conditions

#transparent versus opaque condition:
dataIta$morphType <- relevel(dataIta$morphType, "op");
italmer2c <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + (1|subject) + (1|target), data=dataIta, REML = T);
italmer2d <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + (1|subject) + (1|target), data=subset(dataIta, abs(scale(resid(italmer2c)))<2.5), REML = T);
summary(italmer2d); #here we get the contrast between transparent and opaque pairs

#----------------------#
#### modelling, eng ####
#----------------------#
dataEng$morphType <- relevel(dataEng$morphType, "or");
contrasts(dataEng$relatedness);
contrasts(dataEng$morphType);

englmer0 <- lmer(-1000/rt ~ 1 + (1|subject) + (1|target), data= dataEng, REML = F);

englmer1 <- lmer(-1000/rt ~ trialCount + rotation + (1|subject) + (1|target), data= dataEng, REML = F);
anova(englmer0, englmer1); #no effect here

englmer1 <- lmer(-1000/rt ~ freqTarget + lengthTarget + nTarget + (1|subject) + (1|target), data= dataEng, REML = F);
anova(englmer0, englmer1); #strong improvement in GoF
anova(englmer1); #frequency and length contribute

englmer2 <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + lengthTarget + (1|subject) + (1|target), data= dataEng, REML = T);
summary(englmer2); #residuals are quite symmetrical
#outliers trimming, a la Baayen (2008)
englmer2b <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + lengthTarget + (1|subject) + (1|target), data=subset(dataEng, abs(scale(resid(englmer2)))<2.5), REML = T);
anova(englmer2b); #here we get the overall significance of the interaction btw prime type and relatedness
summary(englmer2b); #here we get the parameters for contrasts across individual conditions

#transparent versus opaque condition:
dataEng$morphType <- relevel(dataEng$morphType, "op");
englmer2c <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + lengthTarget + (1|subject) + (1|target), data=dataEng, REML = T);
englmer2d <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + lengthTarget + (1|subject) + (1|target), data=subset(dataEng, abs(scale(resid(englmer2c)))<2.5), REML = T);
summary(englmer2d); #here we get the contrast between transparent and opaque pairs

#------------------------------#
#### plots of estimated RTs ####
#------------------------------#

# figure for the paper
require(cowplot); #for plotting on multiple pages

df <- effect("relatedness:morphType",italmer2b) ;
df <- as.data.frame(df);
df$fit <- inv(df$fit);
df$lower <- inv(df$lower);
df$upper <- inv(df$upper);
revalue(df$relatedness, c("ctrl"="unrelated"))-> df$relatedness
revalue(df$relatedness, c("rel"="related"))-> df$relatedness


dodge <- position_dodge(width = 0.1)
bb  <-ggplot(data = df, aes(x = relatedness, y = fit,group = morphType)) + geom_point(size = 2, position = dodge) + geom_line(aes(linetype=morphType), position = dodge)+ scale_linetype_manual(values=c("solid", "dotted", "dashed")) + theme_classic();
bb  <- bb + geom_pointrange(aes(ymin = df$lower, ymax = df$upper), position = dodge) 
bb  <- bb + scale_y_continuous("RT(ms)", limits = c(515,650)) 
bb  <- bb + theme(axis.title.y = element_text(size = rel(1), angle = 90))
bb  <- bb + theme(axis.text.y = element_text(angle = 00, hjust = 1, size=10, colour = 'black'))
bb  <- bb + theme(axis.title.x = element_blank())
bb <- bb + labs(title='L1 - Italian')
bb <- bb + theme(plot.title= element_text(angle = 00, hjust=0.5, size=15, face = 'bold', colour = 'black'))
bb <- bb + theme(legend.position="none")
bb
ggsave("itaplot.jpg")


df <- effect("relatedness:morphType",englmer2b) 
df <- as.data.frame(df)
df$fit <- inv(df$fit)
df$lower <- inv(df$lower)
df$upper <- inv(df$upper)
revalue(df$relatedness, c("ctrl"="unrelated"))-> df$relatedness
revalue(df$relatedness, c("rel"="related"))-> df$relatedness

dodge <- position_dodge(width = 0.1)
gg  <-ggplot(data = df, aes(x = relatedness, y = fit,group = morphType)) + geom_point(size = 2, position = dodge) + geom_line(aes(linetype=morphType), position = dodge)+ scale_linetype_manual(values=c("solid", "dotted", "dashed")) + theme_classic();
gg  <- gg + geom_pointrange(aes(ymin = df$lower, ymax = df$upper), position = dodge)
gg  <- gg + scale_y_continuous("") 
gg  <- gg + theme(axis.text.y = element_text(angle = 00, hjust = 1, size=10, colour = 'black'))
gg  <- gg + theme(axis.title.x = element_blank())
gg <- gg + labs(title='L2 - English')
gg <- gg + theme(plot.title= element_text(angle = 00, hjust=0.5, size=15, face = 'bold', colour = 'black'))
gg <- gg + theme(legend.position="none")
#gg <- gg + theme(legend.title = element_text(size = 8))
#gg <- gg + theme(legend.text = element_text(size = 8))
gg
ggsave("engplot.jpg")
rm(gg, bb, aa, dodge)

#----------------------------------#
#### cross language interaction ####
#----------------------------------#
rbind(dataEng, dataIta) -> crossExp;
summary(crossExp);

crosslmer <- lmer(-1000/rt ~ relatedness * morphType * language + freqTarget + lengthTarget + (1|subject) + (1|target), data = crossExp, REML = T);
crosslmerb <- lmer(-1000/rt ~ relatedness * morphType * language + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(crossExp, abs(scale(resid(crosslmer)))<2.5), REML = T);
anova(crosslmerb);

#--------------------------------------------------------#
#### proficiency scores, correlation and distribution ####
#--------------------------------------------------------#
#create a database with one line per ppt
pptFeatures <- unique(dataEng[,c('subject','age','gender','handedness','rotation','phonemicFluency', 'phonemicComprehension','morphComprehension','spelling','readingComprehension','vocabulary','oralComprehension','aoa1.Aoa', 'aoa2.usage', 'aoa3.context','aoa4.multLang','aoa5.selfRatedProf','aoa6.multiLing')]);
summary(pptFeatures);

#correlation between the individual scores
round(cor(pptFeatures[,c(6:12)], use='pairwise.complete.obs'), digits=2);
temp <- as.vector(round(cor(pptFeatures[,c(6:12)], use='pairwise.complete.obs'), digits=2));
fivenum(temp[temp!=1]);

#check score distributions
jpeg(filename = paste(localGitDir,'/proficiencyDistribution.jpg', sep = ''), res=300, height=2200, width=4400);
     
par(mfrow=c(2,4));
par(mar=c(5,5,4,.5)+.1);
par(lwd=2);

attach(pptFeatures);

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

#hist(overallProf, breaks = seq(-2.5,2,.5), main = '(h) Overall Proficiency', cex.main=2, xlab = 'Scores', ylab = 'N of participants', ylim=c(0,50), cex.lab=2, axes=F, col=grey(.80), border=grey(0));
#axis(1, cex.axis=2);
#axis(2, at=c(0,50), cex.axis=2, las=1);

detach(pptFeatures);

par(mfrow=c(1,1));

dev.off();
#-----------------------------#
#### proficiency modelling ####
#-----------------------------#
#overall improvement in goodness of fit
proficiencylmer0 <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML=F); #this establishes the baseline model, with no proficiency score

#here we test whether each individual proficiency score guarantees a better overall fit
proficiencylmer1 <- lmer(-1000/rt ~ relatedness * morphType * phonemicFluency + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML=F);
anova(proficiencylmer0, proficiencylmer1);
proficiencylmer2 <- lmer(-1000/rt ~ relatedness *  morphType * phonemicComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML=F);
anova(proficiencylmer0, proficiencylmer2);
proficiencylmer3 <- lmer(-1000/rt ~ relatedness *  morphType * morphComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML=F);
anova(proficiencylmer0,proficiencylmer3);
proficiencylmer4 <- lmer(-1000/rt ~ relatedness * morphType * spelling + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML=F);
anova(proficiencylmer0, proficiencylmer4);
proficiencylmer5 <- lmer(-1000/rt ~ relatedness * morphType * readingComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML=F);
anova(proficiencylmer0, proficiencylmer5); 
proficiencylmer6 <- lmer(-1000/rt ~ relatedness * morphType * vocabulary + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML=F);
anova(proficiencylmer0,proficiencylmer6);
proficiencylmer7 <- lmer(-1000/rt ~ relatedness * morphType * oralComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML=F);
anova(proficiencylmer0,proficiencylmer7);

#does proficiency specifically interact with priming?
proficiencylmer1b <- lmer(-1000/rt ~ relatedness * morphType * phonemicFluency + trialCount + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer1)))<2.5), REML=T);
anova(proficiencylmer1b);

proficiencylmer2b <- lmer(-1000/rt ~ relatedness *  morphType * phonemicComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer2)))<2.5), REML=F);
anova(proficiencylmer2b);

proficiencylmer3b <- lmer(-1000/rt ~ relatedness *  morphType * morphComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer3)))<2.5), REML=F);
anova(proficiencylmer3b);

proficiencylmer4b <- lmer(-1000/rt ~ relatedness * morphType * spelling + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer4)))<2.5), REML=F);
anova(proficiencylmer4b);

proficiencylmer5b <- lmer(-1000/rt ~ relatedness * morphType * readingComprehension + trialCount + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer5)))<2.5), REML=F);
anova(proficiencylmer5b);

proficiencylmer6b <- lmer(-1000/rt ~ relatedness * morphType * vocabulary + trialCount + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer6)))<2.5), REML=F);
anova(proficiencylmer6b);

proficiencylmer7b <- lmer(-1000/rt ~ relatedness * morphType * oralComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer7)))<2.5), REML=F);
anova(proficiencylmer7b);

#what sort of effect this is? [THIS IS WHERE YOUR NEW PLOTS SHOULD COME IN, EVA]
temp <- data.frame(effect('relatedness:morphType:phonemicFluency', proficiencylmer1b, se=list(level=.95), xlevels=4));
temp[,c('fit','lower','upper')] <- inv(temp[,c('fit','lower','upper')]);

jpeg(filename = paste(localGitDir,'/proficiencyModel.jpg', sep = ''), res=300, height=2200, width=4400);

ggplot(data = temp, aes(x=relatedness, y=fit)) +
  #geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
  facet_grid(morphType ~ phonemicFluency);

dev.off();
#it seems the case that transparent priming stay strong and solid across different levels of proficiency, while opaque and orthographic priming tend to shrink with growing phonemic fluency. This suggests we should use transparent primes as our reference level for morphological condition: 
dataEng$morphType <- relevel(dataEng$morphType, "tr");
proficiencylmer1b <- lmer(-1000/rt ~ relatedness * morphType * phonemicFluency + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer1)))<2.5), REML=T);
summary(proficiencylmer1b);

#although only phonemic fluency is frankly significant, we want to check whether the two variables coming close behind shows the same effect, at least qualitatively:
temp <- data.frame(effect('relatedness:morphType:morphComprehension', proficiencylmer3b, se=list(level=.95), xlevels=4));
temp[,c('fit','lower','upper')] <- inv(temp[,c('fit','lower','upper')]);
ggplot(data = temp, aes(x=relatedness, y=fit)) +
  #geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
  facet_grid(morphType ~ morphComprehension);

temp <- data.frame(effect('relatedness:morphType:spelling', proficiencylmer4b, se=list(level=.95), xlevels=4));
temp[,c('fit','lower','upper')] <- inv(temp[,c('fit','lower','upper')]);
ggplot(data = temp, aes(x=relatedness, y=fit)) +
  #geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
  facet_grid(morphType ~ spelling);

#--------------------------------#
#### THIS WILL LIKELY GO AWAY ####
#--------------------------------#
#plot of the priming modulation by proficiency
jpeg(filename = "C:/Users/Eva Viviani/Documents/GitHub/M2-maskedprimingBilinguals/Rplot.jpg", res=300, height=1654, width=3339)
par(mfrow=c(1,4));
a<- plotLMER.fnc(proficiencylmer8b, withList = TRUE, fun = inv, pred = "relatedness",control = list("overallProf", quantile(dataEng$overallProf, .1)), intr = list("morphType", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='VERY LOW PROFICIENCY', ylimit = c(570,700), bty='l'); 
b<- plotLMER.fnc(proficiencylmer8b, withList = TRUE, fun = inv, pred = "relatedness",control = list("overallProf", quantile(dataEng$overallProf, .5)), intr = list("morphType", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='LOW PROFICIENCY', ylimit = c(570,700), bty='l');
c<- plotLMER.fnc(proficiencylmer8b, withList = TRUE, fun = inv, pred = "relatedness",control = list("overallProf", quantile(dataEng$overallProf, .6)), intr = list("morphType", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='HIGH PROFICIENCY', ylimit = c(570,700), bty='l');
d<- plotLMER.fnc(proficiencylmer8b, withList = TRUE, fun = inv, pred = "relatedness",control = list("overallProf", quantile(dataEng$overallProf, .9)), intr = list("morphType", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', xlabel = "Unrelated         Related", main='VERY HIGH PROFICIENCY', ylimit = c(570,700), bty='l');
par(mfrow=c(1,1));
dev.off()
#heavily modulated

#New plot of proficiency - check it out
library(effects);
df <- effect("relatedness:morphType:overallProf", proficiencylmer8b); #effect extract the data from the model
df <- as.data.frame(df);
df$fit <- inv(df$fit) #inversion of the rt
df$lower <- inv(df$lower)
df$upper <- inv(df$upper)
rel <- c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30) #here I manually select the rows of the related condition (which are all even)
ctrl <- c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29) #selction of the unrelated
deltaRT  <- NULL #now I compute the difference (deltaRT) between related and unrelated
deltasd <- NULL #and the confidence intervals
for (i in 1:nrow(df)){
  deltaRT[i]<-df$fit[rel[i]] - df$fit[ctrl[i]] #take from the dataset only the corresponding rows and compute the difference
  deltasd[i]<- sqrt((df$se[rel[i]]^2) + (df$se[ctrl[i]]^2))  #this computes the stand err of the mean (to revise, not sure of the formula)
}
proficiency <- df$overallProf[rel] #take the proficiency column (note that we have only 5 breaks: -2 -1 -0.4 0.5 1)
deltaRT<- deltaRT[!is.na(deltaRT)] #remove NA (we have only 15 values)
deltasd<- deltasd[!is.na(deltasd)]
deltaRT1 <- data.frame(deltaRT, deltasd, proficiency) #create a dataframe
names(deltaRT1) <- c('deltart','deltase','proficiency')
deltaRT1$morphtype <- c('or', 'op', 'tr', 'or', 'op', 'tr', 'or', 'op', 'tr', 'or', 'op', 'tr', 'or', 'op', 'tr') #restore the morphtype column
deltaRT <- deltaRT1
rm(deltaRT1)

dodge <- position_dodge(width = 0.1) #parametric value for positioning points
a <-ggplot(data = deltaRT, aes(x = proficiency, y = deltart, col = morphtype ,group = morphtype)) + scale_colour_manual(breaks = c("or", "op", "tr"), values = c("#0000e8", "#000000", "#ff0030")) + geom_point(position = dodge, size = 4.5, shape=21, fill="white") + geom_line(position = dodge)+ theme_classic()
a <- a + scale_y_continuous("priming (ms)")
a
ggsave("proficiencyplot.jpg")

#Primo quartile
subset(df, df$overallProf<=-2)-> verylowProf
# plot using ggplot
dodge <- position_dodge(width = 0.1)
a <-ggplot(data = verylowProf, aes(x = relatedness, y = fit, col = morphType ,group = morphType)) + scale_colour_manual(breaks = c("or", "op", "tr"), values = c("#0000e8", "#000000", "#ff0030")) + geom_point(position = dodge, size = 4.5, shape=21, fill="white") + geom_line(position = dodge)+ theme_classic()
a <- a + geom_errorbar(aes(ymin = verylowProf$lower, ymax =verylowProf$upper), width=0.1, size=1, linetype=1, position = dodge) #FUCK YEAH
a <- a + scale_y_continuous("RT(ms)",limits=c(525,710))
a <- a + theme(axis.title.y = element_text(size = rel(1.5), angle = 90)) + ggtitle('Very low proficiency \n1st quartile') + theme(plot.title = element_text(hjust = 0.5))
a <- a + theme(axis.text.y = element_text(angle = 00, hjust = 1, size=13, face = 'bold', colour = 'black'))
a <- a + labs(x = "UNRELATED       RELATED ") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
a <- a + theme(axis.title.x = element_text(size = rel(1), face = 'bold')) +  theme(legend.position="none")
a
ggsave("firsquartileProficiency.jpg", height=6, width=6, dpi = 2000)

#Secondo quartile
subset(df, df$overallProf> -2.0 & df$overallProf<=-0.4)-> lowProf

dodge <- position_dodge(width = 0.1)
b <-ggplot(data = lowProf, aes(x = relatedness, y = fit, col = morphType ,group = morphType)) + scale_colour_manual(breaks = c("or", "op", "tr"), values = c("#0000e8", "#000000", "#ff0030")) + geom_point(position = dodge, size = 4.5, shape=21, fill="white") + geom_line(position = dodge)+ theme_classic()
b <- b + geom_errorbar(aes(ymin = lowProf$lower, ymax = lowProf$upper), width=0.1, size=1, linetype=1, position = dodge) #FUCK YEAH
b <- b + scale_y_continuous(limits=c(525,710)) + ylab(NULL) + theme(axis.title.y=element_blank(), axis.text.y=element_blank())
b <- b + theme(axis.title.y = element_text(size = rel(1.5), angle = 90)) + ggtitle('Low proficiency \n2nd quartile') + theme(plot.title = element_text(hjust = 0.5))
b <- b + theme(axis.text.y = element_text(angle = 00, hjust = 1, size=13, face = 'bold', colour = 'black'))
b <- b + labs(x = "UNRELATED       RELATED ") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
b <- b + theme(axis.title.x = element_text(size = rel(1), face = 'bold')) +  theme(legend.position="none")
b
ggsave("secondquartileProficiency.jpg", height=6, width=6, dpi = 2000)

#Terzo quartile
subset(df, df$overallProf>=-0.4 & df$overallProf<=0.5)-> highProf

dodge <- position_dodge(width = 0.1)
c <-ggplot(data = highProf, aes(x = relatedness, y = fit, col = morphType ,group = morphType)) + scale_colour_manual(breaks = c("OR", "OP", "TR"), values = c("#0000e8", "#000000", "#ff0030")) + geom_point(position = dodge, size = 4.5, shape=21, fill="white") + geom_line(position = dodge)+ theme_classic()
c <- c + geom_errorbar(aes(ymin = highProf$lower, ymax = highProf$upper), width=0.1, size=1, linetype=1, position = dodge) #FUCK YEAH
c <- c + scale_y_continuous(limits=c(525,710)) + ylab(NULL) + theme(axis.title.y=element_blank(), axis.text.y=element_blank())
c <- c + theme(axis.title.y = element_text(size = rel(1.5), angle = 90))  + ggtitle('High proficiency \n3rd quartile') + theme(plot.title = element_text(hjust = 0.5))
c <- c + theme(axis.text.y = element_text(angle = 00, hjust = 1, size=13, face = 'bold', colour = 'black'))
c <- c + labs(x = "UNRELATED       RELATED ") + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
c <- c + theme(axis.title.x = element_text(size = rel(1), face = 'bold')) +  theme(legend.position="none")
c
ggsave("thirdquartileProficiency.jpg", height=6, width=6, dpi = 2000)

#quarto quartile
subset(df, df$overallProf>=1.0)-> veryhighProf

dodge <- position_dodge(width = 0.1)
d <-ggplot(data = veryhighProf, aes(x = relatedness, y = fit, col = morphType ,group = morphType)) + scale_colour_manual(breaks = c("or", "op", "tr"), values = c("#0000e8", "#000000", "#ff0030")) + geom_point(position = dodge, size = 4.5, shape=21, fill="white") + geom_line(position = dodge)+ theme_classic()
d <- d + geom_errorbar(aes(ymin = veryhighProf$lower, ymax = veryhighProf$upper), width=0.1, size=1, linetype=1, position = dodge) #FUCK YEAH
d <- d + scale_y_continuous(limits=c(525,710)) + ylab(NULL) + theme(axis.title.y=element_blank(), axis.text.y=element_blank())
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

#------------------------------------------------#
#### aoa scores, correlation and distribution ####
#------------------------------------------------#
head(pptFeatures);
summary(pptFeatures);

jpeg(filename = paste(localGitDir,'/aoaScores.jpg', sep = ''), res=300, height=2200, width=4400); 
par(mfrow=c(2,3));
par(mar=c(5,5,4,.5)+.1);
par(lwd=2);

attach(pptFeatures);

hist(aoa1.Aoa, breaks = seq(-.5,15.5,1), main = '(a) Age first exposed', cex.main=2, xlab = 'Scores', ylab = 'N of participants', ylim=c(0,30), cex.lab=2, axes=F, col=grey(.80), border=grey(0));
axis(1, cex.axis=2);
axis(2, at=c(0,30), cex.axis=2, las=1);

hist(aoa2.usage, breaks = seq(.5,5.5,1), main = '(b) Daily use', cex.main=2, xlab = 'Scores', ylab = 'N of participants', ylim=c(0,30), cex.lab=2, axes=F, col=grey(.80), border=grey(0));
axis(1, cex.axis=2);
axis(2, at=c(0,30), cex.axis=2, las=1);

barplot(table(aoa3.context), main = '(c) Where did you learn?', cex.main=2,  ylab = 'N of participants', ylim=c(0,65), cex.lab=2, cex.names=2, axes=F, col=grey(.80), border=grey(0));
axis(2, at=c(0,65), cex.axis=2, las=1);

barplot(table(aoa4.contextMultiling), main = '(d) Multilingual context', cex.main=2, ylab = 'N of participants', ylim=c(0,65), cex.lab=2, cex.names=2, axes=F, col=grey(.80), border=grey(0));
axis(2, at=c(0,65), cex.axis=2, las=1);

hist(aoa5.selfRatedProf, breaks = seq(.5,5.5,1), main = '(e) Self rated proficiency', cex.main=2, xlab = 'Scores', ylab = 'N of participants', ylim=c(0,30), cex.lab=2, axes=F, col=grey(.80), border=grey(0));
axis(1, cex.axis=2);
axis(2, at=c(0,30), cex.axis=2, las=1);

barplot(table(aoa6.otherLang), main = '(f) Additional languages?', cex.main=2, ylab = 'N of participants', ylim=c(0,65), cex.lab=2, cex.names=2, axes=F, col=grey(.80), border=grey(0));
axis(2, at=c(0,65), cex.axis=2, las=1);

detach(pptFeatures);

par(mfrow=c(1,1));
dev.off();

#correlation between the individual scores, and between aoa and proficiency:
round(cor(pptFeatures[,c(13,14,17)], use='pairwise.complete.obs', method='spearman'), digits=2); #aoa2 and aoa5 are quite correlated (unsurprisingly)
round(cor(pptFeatures[,c(6:12, 13)], use='pairwise.complete.obs', method='spearman'), digits=2)[8,];

#-----------------------------#
#### aoa scores, modelling ####
#-----------------------------#

#overall increase in GoF?
aoalmer1 <- lmer(-1000/rt ~ relatedness*morphType*aoa1.Aoa + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng);
anova(proficiencylmer0, aoalmer1);
#priming modulation?
aoalmer1b <- lmer(-1000/rt ~ relatedness*morphType*aoa1.Aoa + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(aoalmer1)))<2.5));
anova(aoalmer1b);

#other AoA questionnaire scores
aoalmer2 <- lmer(-1000/rt ~ relatedness*morphType*aoa2.usage + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng);
anova(proficiencylmer0, aoalmer2);

aoalmer3 <- lmer(-1000/rt ~ relatedness*morphType*aoa3.context + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng);
anova(proficiencylmer0, aoalmer3);

aoalmer4 <- lmer(-1000/rt ~ relatedness*morphType*aoa4.contextMultiling + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng);
anova(proficiencylmer0, aoalmer4);

aoalmer5 <- lmer(-1000/rt ~ relatedness*morphType*aoa5.selfRatedProf + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng);
anova(proficiencylmer0, aoalmer5);

aoalmer6 <- lmer(-1000/rt ~ relatedness*morphType*aoa6.otherLang + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng);
anova(proficiencylmer0, aoalmer6);

#priming modulation?
aoalmer2b <- lmer(-1000/rt ~ relatedness*morphType*aoa2.usage + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(aoalmer2)))<2.5));
anova(aoalmer2b);

aoalmer5b <- lmer(-1000/rt ~ relatedness*morphType*aoa5.selfRatedProf + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(aoalmer5)))<2.5));
anova(aoalmer5b);

#-----------#
#### osc ####
#-----------#
#the best model for English is now proficiencylmer1b

#first, let's try to pit OSC against priming condition -- these two are typically confounded:
temp <- unique(masterFile[masterFile$lexicality=='word' & masterFile$language=='eng',c('target','prime','morphType','relatedness','freqTarget','freqPrime','lengthTarget','lengthPrime','nTarget','nPrime','oscTarget')]);
summary(temp);
aggregate(oscTarget ~ morphType, FUN=fivenum, data=temp); #indeed they are

#this represents this graphically
jpeg(filename = paste(localGitDir,'/oscMorph.jpg', sep = ''), res=300, height=2200, width=4400);
temp$morphType <- factor(temp$morphType, levels=c('or','op','tr')); #this controls the order in which the boxes will appear
boxplot(oscTarget ~ morphType, data=subset(temp, oscTarget>0), bty='l', boxwex=.5, col=grey(.80), ylab='OSC', frame=F, axes=F, ylim=c(0,1));
axis(2, at=seq(0,1,.2));
axis(1, at=1:3, labels=c('Orthographic','Opaque','Transparent'), tick=F);
dev.off();

#and this tests it via NHST
summary(aov(oscTarget~morphType, data=subset(temp, relatedness=='rel')));

#modelling
osc1 <- lmer(-1000/rt ~ relatedness * oscTarget * phonemicFluency + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML=T);
osc1b <- lmer(-1000/rt ~ relatedness * oscTarget * phonemicFluency + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(osc1)))<2.5), REML=T);
anova(osc1b);
summary(osc1b); #very solid 3-way interaction

#let's explore the effect
temp <- data.frame(effect('relatedness:oscTarget:phonemicFluency', osc1b, se=list(level=.95), xlevels=list(oscTarget=c(.20,.80), phonemicFluency=c(10,25,40))));
temp[,c('fit','lower','upper')] <- inv(temp[,c('fit','lower','upper')]);

jpeg(filename = paste(localGitDir,'/oscModel.jpg', sep = ''), res=300, height=2200, width=4400);
ggplot(data = temp, aes(x=relatedness, y=fit)) +
  #geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
  facet_grid(oscTarget ~ phonemicFluency);
dev.off(); #this essentially confirms the picture that we see in the proficiency analysis: at high levels of OSC (that is, in parts of the lexical space where the correspondence between form and meaning is strong; that is again, with transparent items), priming is independent of fluency/proficiency; whereas in parts of the lexical space where the correspondence between form and meaning is loose, the higher the proficiency, the smaller the effect. 

#bonus track: we check whether OSC explains data better than morphological condition
extractAIC(osc1);
extractAIC(proficiencylmer1); #and indeed it does
