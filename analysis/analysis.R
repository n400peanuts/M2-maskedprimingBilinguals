#-----------------------------------------------------------------------------------------------------------#
# Data analysis                                                                                             #
# Morphological masked priming experiment on L1-ITA, L2-ENG bilingual speakers/readers                      #
# Paper titled 'Masked morphological priming tracks the development of a fully mature lexical system in L2' #
# Submitted to JML   June 2019                                                                              #  
# Eva Viviani and Davide Crepaldi, SISSA                                                                    #
#-----------------------------------------------------------------------------------------------------------#

#This script takes preprocessed data and produces all the analyses that are reported in the paper.

#--------------------------------------#
#### clean WS, set WD and load data ####
#--------------------------------------#
rm(list = ls());

#set your local working directory. This should be (and is assumed to be in the rest of the code) the highest point in your local folder:
localGitDir <- 'C:/Users/eva_v/Documents/GitHub/M2-maskedprimingBilinguals'
setwd(localGitDir);

# This script works on the outcome of preProcessing.R, which you can upload here:
read.table(paste(localGitDir, '/preprocessedData.txt', sep = ''), header = T, sep='\t', dec='.') -> masterFile; 

head(masterFile);
summary(masterFile);

#------------------------------------------#
#### load packages and create functions ####
#------------------------------------------#
library(ggplot2);
library(rms);
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

source(paste(localGitDir, "/tools/diagnostics.R", sep='')); 
outlierGraphStore <- 'Desktop';
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
dataIta <- subset(dataItaTemp, rt>280 & rt<2000 & subject!=15 & subject!=2 & subject!=31 & target!= "guano" & target!= "uggia" & target!= "vello");
nrow(dataItaTemp)-nrow(dataIta);
(nrow(dataItaTemp)-nrow(dataIta)) / nrow(dataItaTemp);
nrow(dataIta);

summary(dataIta);

#------------------------------#
#### outliers trimming, eng ####
#------------------------------#
subset(masterFile, language=="eng") -> masterFileEng;

# the following code generates target and sbj means and SDs, and the outlier graphs in the file 'ita.jpg'
outlierGraphStore <- 'Desktop';
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
masterFileEng$accuracy[masterFileEng$subject==26] <- car::recode(masterFileEng$accuracy[masterFileEng$subject==26], "1=0;0=1");


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
aggregate(rt ~ relatedness + morphType, FUN=sd, data=dataEng);

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

qqnorm(dataEng$rt)
MASS::boxcox(dataEng$rt~ 1)

englmer0 <- lmer(-1000/rt ~ 1 + (1|subject) + (1|target), data= dataEng, REML = F);
summary(englmer0)
hist(resid(englmer0))

dataEngClean <- dataEng[abs(scale(resid(englmer0)))<2.5,]
(nrow(dataEng) - nrow(dataEngClean)) / nrow(dataEng)
lattice::dotplot(ranef(englmer0, condVar = T))

englmer1 <- lmer(-1000/rt ~ trialCount + rotation + (1|subject) + (1|target), data= dataEngClean, REML = F);
anova(englmer0, englmer1); #no effect here

englmer1 <- lmer(-1000/rt ~ freqTarget + lengthTarget + nTarget + (1|subject) + (1|target), data= dataEngClean, REML = F);
anova(englmer0, englmer1); #strong improvement in GoF
anova(englmer1); #frequency and length contribute

englmer2 <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + lengthTarget + (1|subject) + (1|target), data= dataEng, REML = T);
summary(englmer2); #residuals are quite symmetrical
#outliers trimming, a la Baayen (2008)
englmer2b <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + lengthTarget + (1|subject) + (1|target), data=subset(dataEng, abs(scale(resid(englmer2)))<2.5), REML = T, na.action = 'na.exclude');
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
# figure 1 of the paper
df <- effect("relatedness:morphType",italmer2b);
df <- as.data.frame(df);
df$fit <- inv(df$fit);
df$lower <- inv(df$lower);
df$upper <- inv(df$upper);
revalue(df$relatedness, c("ctrl"="Unrelated"))-> df$relatedness;
revalue(df$relatedness, c("rel"="Related"))-> df$relatedness;


dodge1 <- position_dodge(width = 0.25);
bb  <-ggplot(data = df, aes(x = relatedness, y = fit,group = morphType)) +
  geom_point(size = 2, position = dodge1) +
  geom_line(aes(linetype=morphType), position = dodge1) + 
  scale_linetype_manual(values=c("dashed", "dotted", "solid")) +
  theme_classic();
bb  <- bb + geom_pointrange(aes(ymin = df$lower, ymax = df$upper), position = dodge1) ;
bb  <- bb + scale_y_continuous("RT(ms)") ;
bb  <- bb + theme(axis.title.y = element_text(size = rel(1), angle = 90));
bb  <- bb + theme(axis.text.y = element_text(angle = 00, hjust = 1, size=10, colour = 'black'));
bb  <- bb + theme(axis.title.x = element_blank()) + theme(axis.text.x = element_text(size=13, colour = 'black'));
bb <- bb + labs(title='L1 - Italian');
bb <- bb + theme(plot.title= element_text(angle = 00, hjust=0.5, size=15, face = 'bold', colour = 'black'));
bb <- bb + theme(legend.position="none");
bb;
ggsave("itaplot.jpg", width = 4, height = 3, dpi = 300);


df <- effect("relatedness:morphType",englmer2b); 
df <- as.data.frame(df);
df$fit <- inv(df$fit);
df$lower <- inv(df$lower);
df$upper <- inv(df$upper);
revalue(df$relatedness, c("ctrl"="Unrelated"))-> df$relatedness;
revalue(df$relatedness, c("rel"="Related"))-> df$relatedness;

dodge <- position_dodge(width = 0.25);
gg  <-ggplot(data = df, aes(x = relatedness, y = fit,group = morphType)) + 
      geom_point(size = 2, position = dodge) + 
      geom_line(aes(linetype=morphType), position = dodge) + 
      scale_linetype_manual(values=c("dashed", "dotted", "solid")) + 
      theme_classic();
gg  <- gg + geom_pointrange(aes(ymin = df$lower, ymax = df$upper), position = dodge);
gg  <- gg + scale_y_continuous("RT (ms)") ;
gg  <- gg + theme(axis.text.y = element_text(angle = 00, hjust = 1, size=10, colour = 'black'));
gg  <- gg + theme(axis.title.x = element_blank()) + theme(axis.text.x = element_text(size=13, colour = 'black'));
gg <- gg + labs(title='L2 - English');
gg <- gg + theme(plot.title= element_text(angle = 00, hjust=0.5, size=15, face = 'bold', colour = 'black'));
gg + theme(legend.position="none");

ggsave("engplot.jpg", width = 4, height = 3, dpi = 300);
rm(gg, bb, aa);

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
pptFeatures <- unique(dataEng[,c('subject','age','gender','handedness','rotation','phonemicFluency', 'phonemicComprehension','morphComprehension','spelling','readingComprehension','vocabulary','oralComprehension','aoa1.Aoa', 'aoa2.usage', 'aoa3.context','aoa4.contextMultling','aoa5.selfRatedProf','aoa6.otherLang')]);
summary(pptFeatures);

#correlation between the individual scores
round(cor(pptFeatures[,c(6:12)], use='pairwise.complete.obs'), digits=2);
temp <- as.vector(round(cor(pptFeatures[,c(6:12)], use='pairwise.complete.obs'), digits=2));
fivenum(temp[temp!=1]);

#check score distributions, figure 2 in the paper
jpeg(filename = paste(localGitDir,'/fig_proficiencyScores.jpg', sep = ''), res=300, height=2200, width=4400);
     
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

dev.off();
detach(pptFeatures);

par(mfrow=c(1,1));

#-----------------------------#
#### proficiency modelling ####
#-----------------------------#
dataEng$morphType <- relevel(dataEng$morphType, "or");

#overall improvement in goodness of fit
proficiencylmer0 <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F); #this establishes the baseline model, with no proficiency score

#here we test whether each individual proficiency score guarantees a better overall fit
proficiencylmer1 <- lmer(-1000/rt ~ relatedness * morphType * phonemicFluency  + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(proficiencylmer0, proficiencylmer1);
proficiencylmer2 <- lmer(-1000/rt ~ relatedness *  morphType * phonemicComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(proficiencylmer0, proficiencylmer2);
proficiencylmer3 <- lmer(-1000/rt ~ relatedness *  morphType * morphComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(proficiencylmer0,proficiencylmer3);
proficiencylmer4 <- lmer(-1000/rt ~ relatedness * morphType * spelling + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(proficiencylmer0, proficiencylmer4);
proficiencylmer5 <- lmer(-1000/rt ~ relatedness * morphType * readingComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(proficiencylmer0, proficiencylmer5); 
proficiencylmer6 <- lmer(-1000/rt ~ relatedness * morphType * vocabulary + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(proficiencylmer0,proficiencylmer6);
proficiencylmer7 <- lmer(-1000/rt ~ relatedness * morphType * oralComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(proficiencylmer0,proficiencylmer7);

#does proficiency specifically interact with priming?
proficiencylmer1b <- lmer(-1000/rt ~ relatedness * morphType * phonemicFluency + trialCount + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer1)))<2.5), REML = T);
anova(proficiencylmer1b);

proficiencylmer2b <- lmer(-1000/rt ~ relatedness *  morphType * phonemicComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer2)))<2.5), REML = F);
anova(proficiencylmer2b);

proficiencylmer3b <- lmer(-1000/rt ~ relatedness *  morphType * morphComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer3)))<2.5), REML = F);
anova(proficiencylmer3b);

proficiencylmer4b <- lmer(-1000/rt ~ relatedness * morphType * spelling + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer4)))<2.5), REML = F);
anova(proficiencylmer4b);

proficiencylmer5b <- lmer(-1000/rt ~ relatedness * morphType * readingComprehension + trialCount + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer5)))<2.5), REML = F);
anova(proficiencylmer5b);

proficiencylmer6b <- lmer(-1000/rt ~ relatedness * morphType * vocabulary + trialCount + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer6)))<2.5), REML = F);
anova(proficiencylmer6b);

proficiencylmer7b <- lmer(-1000/rt ~ relatedness * morphType * oralComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer7)))<2.5), REML = F);
anova(proficiencylmer7b);

#what sort of effect this is? [Figure 3 in the paper]
temp <- data.frame(effect('relatedness:morphType:phonemicFluency', proficiencylmer1b, se=list(level=.95), xlevels=list(phonemicFluency=quantile(dataEng$phonemicFluency, probs=c(.05,.50,.95)))));
temp[,c('fit','lower','upper')] <- inv(temp[,c('fit','lower','upper')]);
revalue(temp$relatedness, c("rel"="Related"))-> temp$relatedness;
revalue(temp$relatedness, c("ctrl"="Unrelated"))-> temp$relatedness;

phonemicFluency_names <- c(
  "10" = "Low Fluency",
  "23" = "Medium Fluency",
  "39" = "High Fluency");

ggplot(data = temp, aes(x=relatedness, y=fit, group=morphType)) + 
  geom_point(size = 2, position = dodge) +
  geom_line(aes(linetype=morphType), position = dodge) + 
  scale_linetype_manual(values=c("dashed", "dotted", "solid")) +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) +
  ylab('RTs (ms)') + xlab('') + 
  theme(axis.text.y = element_text(angle = 00, hjust = 1, size=8, colour = 'black'))+
  theme(axis.text.x = element_text(size=13, colour = 'black'))+
  geom_pointrange(aes(ymin = temp$lower, ymax = temp$upper), position = dodge) +
  facet_grid(~ phonemicFluency, 
             labeller = labeller(phonemicFluency = as_labeller(phonemicFluency_names))) +
  theme(strip.text = element_text(size=12)) + 
  theme(legend.position="none");

ggsave("proficiencyModel.jpg", width = 7, height = 3, dpi = 300)

#it seems the case that transparent priming stay strong and solid across different levels of proficiency, while opaque and orthographic priming tend to shrink with growing phonemic fluency. This suggests we should use transparent primes as our reference level for morphological condition: 
dataEng$morphType <- relevel(dataEng$morphType, "tr");
proficiencylmer1b <- lmer(-1000/rt ~ relatedness * morphType * phonemicFluency + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer1)))<2.5), REML = T);
summary(proficiencylmer1b);

proficiencylmer3b <- lmer(-1000/rt ~ relatedness *  morphType * morphComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer3)))<2.5), REML = T);
summary(proficiencylmer3b);

#although only phonemic fluency is frankly significant, we want to check whether the two variables coming close behind shows the same effect, at least qualitatively:
# [figure 7 in the paper]
temp <- data.frame(effect('relatedness:morphType:morphComprehension', proficiencylmer3b, se=list(level=.95), xlevels=list(morphComprehension=quantile(dataEng$morphComprehension, probs=c(.05,.50,.95)))));
temp[,c('fit','lower','upper')] <- inv(temp[,c('fit','lower','upper')]);
revalue(temp$relatedness, c("rel"="Related"))-> temp$relatedness;
revalue(temp$relatedness, c("ctrl"="Unrelated"))-> temp$relatedness;

morphComprehension_names <- c(
  "6" = "Low Morph Awareness",
  "9" = "Medium Morph Awareness",
  "10" = "High Morph Awareness");

ggplot(data = temp, aes(x=relatedness, y=fit, group=morphType)) + theme_bw()+
  geom_line(aes(linetype=morphType), position = dodge) +
  scale_linetype_manual(values=c("dashed", "dotted", "solid")) +
  geom_point(size = 2, position = dodge) +
  geom_pointrange(aes(ymin = temp$lower, ymax = temp$upper), position = dodge) +
  facet_grid( ~ morphComprehension, labeller = labeller(morphComprehension = as_labeller(morphComprehension_names))) +
  theme(strip.text = element_text(size=12))+ 
  theme(panel.grid.major = element_blank()) +
  ylab('RTs (ms)') + xlab('') + 
  theme(axis.text.y = element_text(angle = 00, hjust = 1, size=8, colour = 'black'))+
  theme(axis.text.x = element_text(size=13, colour = 'black')) +
  theme(legend.position="none");

ggsave("morphAwarenessModel.jpg", width = 7, height = 3, dpi = 300);
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

barplot(table(aoa4.contextMultling), main = '(d) Multilingual context', cex.main=2, ylab = 'N of participants', ylim=c(0,65), cex.lab=2, cex.names=2, axes=F, col=grey(.80), border=grey(0));
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
aoalmer1 <- lmer(-1000/rt ~ relatedness*morphType*aoa1.Aoa + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(proficiencylmer0, aoalmer1);
#priming modulation?
aoalmer1b <- lmer(-1000/rt ~ relatedness*morphType*aoa1.Aoa + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(aoalmer1)))<2.5), REML = F);
anova(aoalmer1b);

#other AoA questionnaire scores
aoalmer2 <- lmer(-1000/rt ~ relatedness*morphType*aoa2.usage + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(proficiencylmer0, aoalmer2);

aoalmer3 <- lmer(-1000/rt ~ relatedness*morphType*aoa3.context + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(proficiencylmer0, aoalmer3);

aoalmer4 <- lmer(-1000/rt ~ relatedness*morphType*aoa4.contextMultling + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(proficiencylmer0, aoalmer4);

aoalmer5 <- lmer(-1000/rt ~ relatedness*morphType*aoa5.selfRatedProf + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(proficiencylmer0, aoalmer5);

aoalmer6 <- lmer(-1000/rt ~ relatedness*morphType*aoa6.otherLang + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(proficiencylmer0, aoalmer6);

#priming modulation?
aoalmer2b <- lmer(-1000/rt ~ relatedness*morphType*aoa2.usage + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(aoalmer2)))<2.5), REML = F);
anova(aoalmer2b);

aoalmer5b <- lmer(-1000/rt ~ relatedness*morphType*aoa5.selfRatedProf + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(aoalmer5)))<2.5), REML = F);
anova(aoalmer5b);

#-----------#
#### osc ####
#-----------#
#the best model for English is now proficiencylmer1b

#first, let's try to pit OSC against priming condition -- these two are typically confounded:
temp <- unique(masterFile[masterFile$lexicality=='word' & masterFile$language=='eng',c('target','prime','morphType','relatedness','freqTarget','freqPrime','lengthTarget','lengthPrime','nTarget','nPrime','oscTarget')]);
summary(temp);
aggregate(oscTarget ~ morphType, FUN=fivenum, data=temp); #indeed they are

#this represents this graphically [figure 4 in the paper]
revalue(temp$morphType, c("or"="Orthographic", 'op'='Opaque', 'tr'='Transparent'))-> temp$morphType;
library(ggpubr);
ggboxplot(subset(temp, oscTarget>0), "morphType", "oscTarget",
          color = "black", fill = grey(.80),
          width = 0.5, ylab = 'OSC', xlab = ''); 

ggsave("oscMorph.jpg", width = 4, height = 3, dpi = 300);

#and this tests it via NHST
summary(aov(oscTarget~morphType, data=subset(temp, relatedness=='rel')));

#modelling
osc1 <- lmer(-1000/rt ~ relatedness *  oscTarget * phonemicFluency + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(osc1);
osc1b <- lmer(-1000/rt ~  relatedness * oscTarget * phonemicFluency + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(osc1)))<2), REML = T);
anova(osc1b);
summary(osc1b); #very solid 3-way interaction

#[figure 6 in the paper]
temp <- data.frame(effect('relatedness:oscTarget:phonemicFluency', osc1b, se=list(level=.95), xlevels=list(oscTarget=c(.20,.80), phonemicFluency=quantile(dataEng$phonemicFluency, probs=c(.05,.50,.95)))));
temp[,c('fit','lower','upper')] <- inv(temp[,c('fit','lower','upper')]);
revalue(temp$relatedness, c("rel"="Related"))-> temp$relatedness;
revalue(temp$relatedness, c("ctrl"="Unrelated"))-> temp$relatedness;

phonemicFluency_names <- c(
  "10" = "Low Fluency",
  "23" = "Medium Fluency",
  "39" = "High Fluency"
);

temp$oscTarget <- as.factor(temp$oscTarget);
ggplot(data = temp, aes(x=relatedness, y=fit, group=oscTarget)) + 
  geom_point(position = dodge) +
  geom_line(aes(linetype = oscTarget), position = dodge) + 
  scale_linetype_manual(values=c("dashed", "solid")) +
  theme_bw() + 
  theme(panel.grid.major = element_blank()) +
  ylab('RTs (ms)') + xlab('') + 
  theme(axis.text.y = element_text(angle = 00, hjust = 1, size=8, colour = 'black'))+
  theme(axis.text.x = element_text(size=13, colour = 'black'))+
  geom_pointrange(aes(ymin = temp$lower, ymax = temp$upper), position = dodge) +
  facet_grid(~ phonemicFluency, 
             labeller = labeller(phonemicFluency = as_labeller(phonemicFluency_names))) +
  theme(strip.text = element_text(size=12))+
  theme(legend.position="none");

ggsave("oscModel.jpg", width = 7, height = 3, dpi = 300);

#bonus track: we check whether OSC explains data better than morphological condition
extractAIC(osc1);
extractAIC(proficiencylmer1); #and indeed it does


#### Analysis of Errors ####
summary(dataEngAcc);
dataEngAcc2 <- subset(dataEngAcc, rt>300 & rt<2000 & subject!=15 & subject!=22 & subject!=43);

#The overall accuracy is 76.7%
mean(dataEngAcc2$accuracy); 

#Q1: how these errors are distributed across the three prime conditions?#
nrow(dataEngAcc2[dataEngAcc2$morphType=='tr' & dataEngAcc2$accuracy==1,]);
nrow(dataEngAcc2[dataEngAcc2$morphType=='op' & dataEngAcc2$accuracy==1,]);
nrow(dataEngAcc2[dataEngAcc2$morphType=='or' & dataEngAcc2$accuracy==1,]);
#more correct datapoints in the transparent condition

aggregate(accuracy ~ morphType, data = dataEngAcc2, mean);
aggregate(accuracy ~ relatedness * morphType, data = dataEngAcc2, mean);
#related datapoints always more accurate than unrelated
#tr more correct, than op and or

dataEngAcc2$morphType <- relevel(dataEngAcc2$morphType, "or");
lmacc <- lmer(accuracy ~ relatedness * morphType + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEngAcc2)
anova(lmacc); # no significant main effect of morphtype, but marginal significance with relatedness
summary(lmacc); #driven by a significant difference between or and op in the related condition, with opaque being more accurate than or 
#no difference between or and tr
dataEngAcc2$morphType <- relevel(dataEngAcc2$morphType, "op");
lmacc2 <- lmer(accuracy ~ relatedness * morphType + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEngAcc2)
summary(lmacc2); 
#no difference between op and tr

#transparent pairs are more correct than opaque, which conversely are more correct than orthographic
#let's modulate it with proficiency metrics

#Q2: inspection of the accuracy might reveal that less proficient L2 speakers make more errors than more proficient#
accproficiencylmer1 <- lmer(accuracy ~ relatedness * morphType * phonemicFluency + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEngAcc2)
anova(accproficiencylmer1); #main effect of phonemic fluency
summary(accproficiencylmer1); #less error overall at higher phonemic fluency but nothing more

accproficiencylmer2 <- lmer(accuracy ~ relatedness * morphType * phonemicComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEngAcc2)
anova(accproficiencylmer2); #morphtype * phonemic comprehension significant, but not three way
summary(accproficiencylmer2);

accproficiencylmer3 <- lmer(accuracy ~ relatedness * morphType * morphComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEngAcc2)
anova(accproficiencylmer3); #morphtype * morphComprehension, but not three way
summary(accproficiencylmer3);

accproficiencylmer4 <- lmer(accuracy ~ relatedness * morphType * spelling + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEngAcc2)
anova(accproficiencylmer4); #nope
summary(accproficiencylmer4);

accproficiencylmer5 <- lmer(accuracy ~ relatedness * morphType * readingComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEngAcc2)
anova(accproficiencylmer5); #nope
summary(accproficiencylmer5);

accproficiencylmer6 <- lmer(accuracy ~ relatedness * morphType * vocabulary + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEngAcc2)
anova(accproficiencylmer6); #nope
summary(accproficiencylmer6);

accproficiencylmer7 <- lmer(accuracy ~ relatedness * morphType * oralComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEngAcc2)
anova(accproficiencylmer7); #nope
summary(accproficiencylmer7);

#so, less proficient speakers make more errors, but it's not interacting with morphType and relatedness

#visualization for the reviewers
library(dplyr);
ss_prop <- dataEngAcc2 %>% 
  group_by(subject, relatedness, morphType, target) %>% 
  summarise(mean_correct = mean(accuracy))

ss_prop <- dataEngAcc2 %>% 
  group_by(subject, relatedness, morphType, target) %>%
  summarise(mean_rt = mean(rt)) %>% 
  left_join(ss_prop)

#plot
#devtools::install_github("langcog/langcog")
library(langcog);

ms <- ss_prop %>%
  group_by(relatedness, morphType) %>% 
  multi_boot_standard(col = "mean_correct") 

p1<-ggplot(aes(x = morphType, y = mean, fill = relatedness), data = ms) +
  geom_bar(stat = "identity", color='white', position=position_dodge(), size=1.2) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width=.15, size=1.5,  
                position = position_dodge(width = 0.9)) +
  geom_hline(yintercept = 0.5, lty = "dashed") +
  xlab(NULL) +
  ylab(" ") +
  ggpubr::theme_pubclean() + 
  scale_fill_grey() +
  theme(legend.position="bottom", legend.title = element_blank()) +
  theme(text = element_text(size=30)) +
  ggtitle("Accuracy");

p1;
ggpubr::ggexport(p1, filename = 'Accuracy.jpg', res = 300, width = 2000, height = 1200);

#Q3: careful considerate how the error data may or may not affect the interpretation of the RT results
#Calculating median reaction times for each condition in the related condition only

ms_rt <- ss_prop %>%
  filter(relatedness == "rel") %>%
  group_by(morphType, mean_correct) %>% 
  multi_boot_standard(col = "mean_rt", na.rm = T, empirical_function = "mean")

#plot
p2<-ggplot(aes(x = as.factor(mean_correct), y = mean, fill = mean_correct), 
           data = ms_rt) + 
  geom_bar(stat = "identity", color='white', position=position_dodge(), size=1.2) + 
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width=.15, size=1.5,
                position = position_dodge(width = 0.9)) +
  facet_grid(morphType~.) +
  theme(text = element_text(size=18)) +
  xlab("Accuracy") +
  ggpubr::theme_pubclean() +
  guides(fill = F) +
  ylab("Mean RT") +
  coord_flip();
p2;
ggpubr::ggexport(p2, filename = 'AccuracyRT.jpg', res = 300, width = 2000, height = 2000);

#let's see whether there is a speed/accuracy trade-off
rt_range <- 2000
n_bins <- 20
break_seq <- seq(0, rt_range, rt_range/n_bins)

timeslice_range <- dataEngAcc2 %>%
  filter(relatedness == "rel") %>%
  mutate(RT_bin = cut(rt, breaks = break_seq)) %>%
  group_by(RT_bin, morphType) %>%
  mutate(RT_bin_avg = mean(rt, na.rm = T));

summary(timeslice_range);

timeslice_range <- timeslice_range %>%
  group_by(RT_bin_avg, morphType, subject) %>% 
  summarise(ss_acc = mean(accuracy, na.rm=T)) %>% 
  group_by(RT_bin_avg, morphType) %>%
  summarise(mean = mean(ss_acc),
            n = n());

p3<-ggplot(aes(x=RT_bin_avg, y=mean, 
           color = morphType, group = morphType), 
       data = timeslice_range) + 
  geom_point(size = 3) +
  geom_smooth(method = "lm", formula = y ~ poly(x,2), se = FALSE) +
  scale_color_solarized() +
  geom_hline(yintercept = 0.5, lty = "dashed") +
  xlab("Average RT") +
  ylab("Proportion Correct");

p3;

mean(dataEngAcc2[dataEngAcc2$morphType=='or' & 
                   dataEngAcc2$relatedness=='rel' & 
                   dataEngAcc2$rt>1600,]$accuracy);

ggpubr::ggexport(p3, filename = 'AccuracyRTradeoff.jpg', res = 300, width = 2000, height = 2000);


#let's see with phonemic fluency
timeslice_range <- dataEngAcc2 %>%
  filter(relatedness == "rel") %>%
  mutate(RT_bin = cut(rt, breaks = break_seq)) %>%
  mutate(prof_bin = cut(phonemicFluency, breaks = 3)) %>%
  group_by(RT_bin, morphType) %>%
  mutate(RT_bin_avg = mean(rt, na.rm = T));

summary(timeslice_range);

timeslice_range <- timeslice_range %>%
  group_by(RT_bin_avg, prof_bin, morphType, target) %>% 
  summarise(ss_acc = mean(accuracy, na.rm=T)) %>% 
  group_by(RT_bin_avg, prof_bin, morphType) %>%
  summarise(mean = mean(ss_acc),
            n = n());

summary(timeslice_range);

labels <- c("(-0.045,15]" = "Low fluency", "(15,30]" = "Medium fluency", "(30,45]" = "High fluency")

p4<-ggplot(aes(x=RT_bin_avg, y=mean, weigh = n,
           color = morphType, group = morphType), 
       data = timeslice_range) + 
  geom_point(aes(size = n)) +
  facet_grid(. ~ prof_bin,
             labeller = labeller(prof_bin = as_labeller(labels))) +
  geom_smooth(method = "lm", formula = y ~ poly(x,2), se = F) +
  scale_color_solarized() +
  geom_hline(yintercept = 0.5, lty = "dashed") +
  xlab("Average RT") +
  ylab("Proportion Correct") +
  ylim(0,1);

p4;
mean(dataEngAcc2[dataEngAcc2$morphType=='tr' & 
                   dataEngAcc2$relatedness=='rel' & 
                   dataEngAcc2$rt>1600 &
                   dataEngAcc2$phonemicFluency>0 &
                   dataEngAcc2$phonemicFluency<15,]$accuracy)

ggpubr::ggexport(p4, filename = 'AccuracyRTradeoff_phonemicFluency.jpg', res = 300, width = 3000, height = 2000);

RTspeedlmer <-lmer(accuracy ~ rt * morphType * phonemicFluency + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEngAcc2[dataEngAcc2$relatedness=='rel',]);
anova(RTspeedlmer);
summary(RTspeedlmer);


#### Reliability analysis ####
#internal consistency of the proficiency measures
pptFeatures <- unique(dataEng[,c('subject','age','gender','handedness','rotation','phonemicFluency', 'phonemicComprehension','morphComprehension','spelling','readingComprehension','vocabulary','oralComprehension','aoa1.Aoa', 'aoa2.usage', 'aoa3.context','aoa4.contextMultling','aoa5.selfRatedProf','aoa6.otherLang')]);

psych::alpha(pptFeatures, check.keys=TRUE)
psych::alpha(pptFeatures, check.keys = TRUE)$total$std.alpha #great value

psych::splitHalf(pptFeatures[,6:12]) #split-half reliability

#split half reliability of the priming
library(data.table);
#in order to run the split half test, we need:
#1. to split in two halves the ENG datasets
subset(dataEng, abs(scale(resid(englmer2)))<2.5) -> temp #eng dataset
subset(dataIta, abs(scale(resid(italmer2)))<2.5) -> temp #ita dataset
dataEngClean -> temp

firsthalfdataEng <- NULL;
secondhalfdataEng <- NULL;

for (i in 1:84){
    if (i %in% unique(temp$subject)){
      print('okay')
      even_indexes <- seq(2, nrow(temp[temp$subject==i ,]), 2)
      odd_indexes <- seq(1, nrow(temp[temp$subject==i ,]), 2)
      temp[temp$subject==i ,] ->s1 
      firsthalfdataEng <- rbind(firsthalfdataEng, s1[even_indexes,])
      secondhalfdataEng <- rbind(secondhalfdataEng, s1[odd_indexes,])
    } 
  else {
      print('skip it')
      next
  }
  print(i)
}
rm(s1)

nrow(temp);
nrow(firsthalfdataEng) + nrow(secondhalfdataEng);

#2. trasform RTs in zRTs
firsthalfdataEng$zrt <- scale(firsthalfdataEng$rt, center = TRUE, scale = FALSE);
secondhalfdataEng$zrt <- scale(secondhalfdataEng$rt, center = TRUE, scale = FALSE);

#3. compute for each subject the priming effect in the two halves separately
#first half, even trials
library(dplyr); library(data.table);
dataEngeven <- NULL;
for (i in 1:84){
  if (i %in% unique(firsthalfdataEng$subject)){
    print(paste0("Subject number: ", i))
    print(paste0('orthographic: ',nrow(firsthalfdataEng[firsthalfdataEng$subject==i & firsthalfdataEng$relatedness=='rel' & firsthalfdataEng$morphType=='or',])))
    print(paste0('opaque: ',nrow(firsthalfdataEng[firsthalfdataEng$subject==i & firsthalfdataEng$relatedness=='rel' & firsthalfdataEng$morphType=='op',])))
    print(paste0('transparent: ',nrow(firsthalfdataEng[firsthalfdataEng$subject==i & firsthalfdataEng$relatedness=='rel' & firsthalfdataEng$morphType=='tr',])))
    s1<-dcast(firsthalfdataEng[firsthalfdataEng$subject==i,],  morphType ~ relatedness , value.var = "rt", mean) %>% 
        mutate(primingeven = rel - ctrl) %>% 
        select(morphType, primingeven);
    s1$subject <- i
    dataEngeven <- rbind(dataEngeven, s1);
    
  } else 
    print('skip')
  {
    next
  }
}

#second half, odd trials
dataEngodd <- NULL;
for (i in 1:84){
  if (i %in% unique(secondhalfdataEng$subject)){
    print(paste0("Subject number: ", i))
    print(paste0('orthographic: ',nrow(firsthalfdataEng[firsthalfdataEng$subject==i & firsthalfdataEng$relatedness=='rel' & firsthalfdataEng$morphType=='or',])))
    print(paste0('opaque: ',nrow(firsthalfdataEng[firsthalfdataEng$subject==i & firsthalfdataEng$relatedness=='rel' & firsthalfdataEng$morphType=='op',])))
    print(paste0('transparent: ',nrow(firsthalfdataEng[firsthalfdataEng$subject==i & firsthalfdataEng$relatedness=='rel' & firsthalfdataEng$morphType=='tr',])))
    s1<-dcast(secondhalfdataEng[secondhalfdataEng$subject==i,],  morphType ~ relatedness , value.var = "rt", mean) %>% 
      mutate(primingodd = rel - ctrl) %>% 
      select(morphType, primingodd);
    s1$subject <- i
    dataEngodd <- rbind(dataEngodd, s1);
    
  } else 
    print('skip')
  {
    next
    }
}
summary(dataEngeven);
summary(dataEngodd);
#good, we have one datapoint per condition per participant.

#3. correlate the two halves

dataEnghalf <- cbind(dataEngodd, dataEngeven)
dataEnghalf$subject <- NULL;
dataEnghalf$morphType <- NULL;
require(ggpubr);
p5<-ggscatter(dataEnghalf, x = "primingodd", y = "primingeven",
          add = "reg.line",                                 # Add regression line
          conf.int = T,                                  # Add confidence interval
          add.params = list(color = "#0892d0", fill = "lightgray", size = 2),
          size = 5,
          shape = 19)+  
  stat_cor(method = "pearson", label.x = -300, label.y = 300) +# Add correlation coefficient
  xlab('Masked morph priming Odd (zRT) ')+
  ylab('Masked morph priming Even (zRT)');
p5;

ggpubr::ggexport(p5, filename = 'splithalfRelITA.jpg', res = 300, width = 3000, height = 2000);


p6<-ggscatter(dataEnghalf, x = "primingodd", y = "primingeven",
          add = "reg.line",                                 # Add regression line
          conf.int = T,                                  # Add confidence interval
          facet.by = "morphType",
          size = 3,
          add.params = list(color = "#0892d0", fill = "lightgray"))+  
  stat_cor(method = "pearson", label.x = -300, label.y = 250) +# Add correlation coefficient
  xlab('Masked morph priming Odd (zRT) ')+
  ylab('Masked morph priming Even (zRT)');
p6;
ggpubr::ggexport(p6, filename = 'splithalfRelbyMorphTypeITA.jpg', res = 300, width = 3000, height = 2000);

psych::splitHalf(dataEnghalf[,c('primingodd', 'primingeven')], 
                 n.sample = 10000, use = 'pairwise');


#### GLMER models ####
#dataEng
glmer(rt ~ relatedness  * morphType + lengthTarget + freqTarget + (1|subject) + (1|target), data = dataEng, family=Gamma(link="identity"))-> mod0a
ss <- getME(mod0a,c("theta","fixef"))
mod0a<-update(mod0a,start=ss,control=glmerControl(optimizer="bobyqa",
                                                optCtrl=list(maxfun=2e5)))

car::Anova(mod0a);
summary(mod0a);

glmer(rt ~ 1+ (1|subject) + (1|target), data = dataEng, family=Gamma(link="identity"))-> mod0 
summary(mod0)
hist(resid(mod0))

dataEngClean <- dataEng[abs(scale(resid(mod0)))<2.5,];
(nrow(dataEng) - nrow(dataEngClean)) / nrow(dataEng);
lattice::dotplot(ranef(mod0, condVar = T));

glmer(rt ~ relatedness  * morphType + lengthTarget + freqTarget + (1|subject) + (1|target), 
      data = dataEngClean, family=Gamma(link="identity"))-> mod1 
car::Anova(mod1);
summary(mod1);

library(actuar);
fitdistrplus::fitdist(dataEng$rt, "gamma")-> gammafit
fitdistrplus::fitdist(dataEng$rt, "lnorm")-> lognormalfit
fitdistrplus::fitdist(dataEng$rt, "invgauss", start = list(mean = 5, shape = 1)) -> invgausfit

fitdistrplus::qqcomp(list(gammafit, lognormalfit, invgausfit),
       legendtext=c("gamma","lnorm", "inverse gaussian"))

#we have convergence issues
#following Ben Bolker's blog
#We try restarting from previous fit . restart didn't converge in 10000 evals, so bumped up max number of iterations.
ss <- getME(mod1,c("theta","fixef"))
mod1<-update(mod1,start=ss,control=glmerControl(optimizer="bobyqa",
                                           optCtrl=list(maxfun=2e5)))
anova(mod1)
summary(mod1)

car::Anova(mod1)

#dataEng with proficiency
glmer(rt ~ relatedness  * morphType * phonemicFluency + lengthTarget + freqTarget + (1|subject) + (1|target), data = dataEng, family=Gamma(link="identity"))-> mod1a 

ss <- getME(mod1a,c("theta","fixef"))
mod1a<-update(mod1a,start=ss,control=glmerControl(optimizer="bobyqa",
                                                optCtrl=list(maxfun=2e5)))
car::Anova(mod1a);
summary(mod1a);

#cleaned data
glmer(rt ~ relatedness  * morphType * phonemicFluency + lengthTarget + freqTarget + (1|subject) + (1|target), data = dataEngClean, family=Gamma(link="identity"))-> mod2 

ss <- getME(mod2,c("theta","fixef"))
mod2<-update(mod2,start=ss,control=glmerControl(optimizer="bobyqa",
                                                optCtrl=list(maxfun=2e5)))
car::Anova(mod2);
summary(mod2);

