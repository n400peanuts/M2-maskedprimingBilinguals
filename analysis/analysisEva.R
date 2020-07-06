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

d<-xtabs(rt  ~ morphType + relatedness  , 
      aggregate(rt~ morphType + relatedness , dataEng, sd));
print(xtable(d), type = 'latex');


#-----------------------#
#### modelling, ita #####
#-----------------------#
library(lmerTest);
dataIta$morphType <- relevel(dataIta$morphType, "or");
contrasts(dataIta$relatedness);
contrasts(dataIta$morphType);

italmer0 <- lmer(-1000/rt ~ 1 + (1|subject) + (1|target), data= dataIta, REML = F);
italmer0a <- lmer(rt ~ 1 + (1|subject) + (1|target), data= dataIta, REML = F);
itaglmer0<- glmer(rt ~ 1 + (1|subject) + (1|target), data= dataIta, family=Gamma(link="identity"));
summary(italmer0);
summary(itaglmer0);
summary(italmer0a);

itaglmer1<- glmer(rt ~ trialCount + rotation + (1|subject) + (1|target), data= dataIta, family=Gamma(link="identity"));
anova(itaglmer0, itaglmer1); #no effect  here

itaglmer1<- glmer(rt ~ freqTarget + lengthTarget + nTarget + (1|subject) + (1|target), data= dataIta, family=Gamma(link="identity"));
anova(itaglmer0, itaglmer1);#strong improvement in GoF
car::Anova(itaglmer1);  #to which only freq seems to contribute

#we introduce the varibles of interest now
itaglmer2<- glmer(rt ~ relatedness * morphType + freqTarget + (1|subject) + (1|target), data= dataIta, family=Gamma(link="identity"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)));

italmer2 <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + (1|subject) + (1|target), data= dataIta, REML = T);
summary(italmer2); #residuals are rather symmetrical -- clean bill of health


anova(italmer2, itaglmer2);

car::Anova(itaglmer2); 
summary(itaglmer2);
anova(italmer2);

lm(-1000/rt ~ relatedness * morphType + freqTarget , data = dataIta)-> lita


#outliers trimming, a la Baayen (2008)
italmer2b <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + (1|subject) + (1|target), data=subset(dataIta, abs(scale(resid(italmer2)))<2.5), REML = T);
anova(italmer2b); #here we get the overall significance of the interaction btw prime type and relatedness
summary(italmer2b);#here we get the parameters for contrasts across individual conditions

itaglmer2b<- glmer(rt ~ relatedness * morphType + freqTarget + (1|subject) + (1|target), data= subset(dataIta, abs(scale(resid(itaglmer2)))<2.5), family=Gamma(link="identity"));
ss <- getME(itaglmer2b,c("theta","fixef"));
itaglmer2b<-update(itaglmer2b,start=ss,control=glmerControl(optimizer="bobyqa",
                                                          optCtrl=list(maxfun=2e5)));



#bonus track for Laurie Feldman - proficiency in ITA
italmer2a <- lmer(rt ~ relatedness * morphType * phonemicFluency + freqTarget + (1|subject) + (1|target), data= dataIta, REML = T);
summary(italmer2a); #residuals are rather symmetrical -- clean bill of health
italmer2b <- lmer(-1000/rt ~ relatedness * morphType * phonemicFluency + freqTarget + (1|subject) + (1|target), data=subset(dataIta, abs(scale(resid(italmer2)))<2.5), REML = T);
anova(italmer2b); #here we get the overall significance of the interaction btw prime type and relatedness

italmer2a <- lmer(rt ~ relatedness * morphType * phonemicComprehension + freqTarget + (1|subject) + (1|target), data= dataIta, REML = T);
summary(italmer2a); #residuals are rather symmetrical -- clean bill of health
italmer2b <- lmer(-1000/rt ~ relatedness * morphType * phonemicComprehension + freqTarget + (1|subject) + (1|target), data=subset(dataIta, abs(scale(resid(italmer2)))<2.5), REML = T);
anova(italmer2b); #here we get the overall significance of the interaction btw prime type and relatedness

italmer2a <- lmer(rt ~ relatedness * morphType * morphComprehension + freqTarget + (1|subject) + (1|target), data= dataIta, REML = T);
summary(italmer2a); #residuals are rather symmetrical -- clean bill of health
italmer2b <- lmer(-1000/rt ~ relatedness * morphType * morphComprehension + freqTarget + (1|subject) + (1|target), data=subset(dataIta, abs(scale(resid(italmer2)))<2.5), REML = T);
anova(italmer2b); #here we get the overall significance of the interaction btw prime type and relatedness

italmer2a <- lmer(rt ~ relatedness * morphType * spelling + freqTarget + (1|subject) + (1|target), data= dataIta, REML = T);
summary(italmer2a); #residuals are rather symmetrical -- clean bill of health
italmer2b <- lmer(-1000/rt ~ relatedness * morphType * spelling + freqTarget + (1|subject) + (1|target), data=subset(dataIta, abs(scale(resid(italmer2)))<2.5), REML = T);
anova(italmer2b); #here we get the overall significance of the interaction btw prime type and relatedness

italmer2a <- lmer(rt ~ relatedness * morphType * vocabulary + freqTarget + (1|subject) + (1|target), data= dataIta, REML = T);
summary(italmer2a); #residuals are rather symmetrical -- clean bill of health
italmer2b <- lmer(-1000/rt ~ relatedness * morphType * vocabulary + freqTarget + (1|subject) + (1|target), data=subset(dataIta, abs(scale(resid(italmer2)))<2.5), REML = T);
anova(italmer2b); #here we get the overall significance of the interaction btw prime type and relatedness

italmer2a <- lmer(rt ~ relatedness * morphType * readingComprehension + freqTarget + (1|subject) + (1|target), data= dataIta, REML = T);
summary(italmer2a); #residuals are rather symmetrical -- clean bill of health
italmer2b <- lmer(-1000/rt ~ relatedness * morphType * readingComprehension + freqTarget + (1|subject) + (1|target), data=subset(dataIta, abs(scale(resid(italmer2)))<2.5), REML = T);
anova(italmer2b); #here we get the overall significance of the interaction btw prime type and relatedness


italmer2a <- lmer(rt ~ relatedness * morphType * oralComprehension + freqTarget + (1|subject) + (1|target), data= dataIta, REML = T);
summary(italmer2a); #residuals are rather symmetrical -- clean bill of health
italmer2b <- lmer(-1000/rt ~ relatedness * morphType * oralComprehension + freqTarget + (1|subject) + (1|target), data=subset(dataIta, abs(scale(resid(italmer2)))<2.5), REML = T);
anova(italmer2b); #here we get the overall significance of the interaction btw prime type and relatedness

rm(itaglmer, itaglmer1, itaglmer2);

a<-plot(itaglmer2, ylab = 'ITA Residuals', xlab = 'Predicted RT', 
        main = 'ITA GLMM Gamma Distribution identity link');
b<-plot(italmer2,  ylab = 'ITA Residuals', xlab = 'Predicted RT', 
        main = 'ITA LMM -1000/RT');
c<-plot(italmer2a,  ylab = 'ITA Residuals', xlab = 'Predicted RT', 
        main = 'ITA LMM raw RT');
d<-plot(italmer2b,  ylab = 'ITA Residuals', xlab = 'Predicted RT', 
        main = 'ITA LMM -1000/RT with model trimming');

z<- ggarrange(c, b, a);
ggsave('residualPlot.png',z, dpi = 300, width = 9, height = 5);

#great, statistics are strong

#transparent versus opaque condition:
dataIta$morphType <- relevel(dataIta$morphType, "op");

italmer2c <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + (1|subject) + (1|target), data= dataIta, REML = T);
summary(italmer2c); #residuals are rather symmetrical -- clean bill of health

itaglmer2c<- glmer(rt ~ relatedness * morphType + freqTarget + (1|subject) + (1|target), data= dataIta, family=Gamma(link="identity"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)));

car::Anova(itaglmer2c);  
summary(itaglmer2c);
rm(itaglmer2c, itaglmer1, itaglmer0);

italmer2d <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + (1|subject) + (1|target), data= subset(dataIta, abs(scale(resid(italmer2c)))<2.5), REML = T);
summary(italmer2d); #residuals are rather symmetrical -- clean bill of health

itaglmer2d<- glmer(rt ~ relatedness * morphType + freqTarget + (1|subject) + (1|target), data= subset(dataIta, abs(scale(resid(itaglmer2c)))<2.5), family=Gamma(link="identity"));
ss <- getME(itaglmer2d,c("theta","fixef"))
itaglmer2d<-update(itaglmer2d,start=ss,control=glmerControl(optimizer="bobyqa",
                                                           optCtrl=list(maxfun=2e5)));



#----------------------#
#### modelling, eng LMMs and GLMMs ####
#----------------------#
dataEng$morphType <- relevel(dataEng$morphType, "or");
contrasts(dataEng$relatedness);
contrasts(dataEng$morphType);


lm(-1000/rt ~ relatedness * morphType + freqTarget + lengthTarget + as.factor(subject) + target, data = dataEng)->leng

englmer0a <- lmer(rt ~ 1 + (1|subject) + (1|target), data= dataEng, REML = F);

englmer0 <- lmer(-1000/rt ~ 1 + (1|subject) + (1|target), data= dataEng, REML = F);
summary(englmer0)
hist(resid(englmer0))

engglmer0 <- glmer(rt ~ 1+ (1|subject) + (1|target), data = dataEng, 
      family=Gamma(link="identity")); 
hist(resid(engglmer0))

engglmer1 <- glmer(rt ~ trialCount + rotation+ (1|subject) + (1|target), data = dataEng, family=Gamma(link="identity")); 
anova(engglmer0, engglmer1); #no effect here

engglmer1 <- glmer(rt ~ freqTarget + lengthTarget + nTarget + (1|subject) + (1|target), data = dataEng, family=Gamma(link="identity")); 
ss <- getME(engglmer1,c("theta","fixef"))
engglmer1<-update(engglmer1,start=ss,control=glmerControl(optimizer="bobyqa",
                                                          optCtrl=list(maxfun=2e5)))
anova(engglmer0, engglmer1); #strong improvement in GoF
car::Anova(engglmer1); #frequency and length contribute

engglmer2 <- glmer(rt ~ relatedness * morphType + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, family=Gamma(link="identity"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))); 

car::Anova(engglmer2);
summary(engglmer2); 

englmer2 <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + lengthTarget + (1|subject) + (1|target), data= dataEng, REML = T);
summary(englmer2); #residuals are quite symmetrical

#outliers trimming, a la Baayen (2008)
englmer2b <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + lengthTarget + (1|subject) + (1|target), data=subset(dataEng, abs(scale(resid(englmer2)))<2.5), REML = T);

engglmer2b <- glmer(rt ~ relatedness * morphType + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(engglmer2)))<2.5), family=Gamma(link="identity")); 
ss <- getME(engglmer2b,c("theta","fixef"))
engglmer2b<-update(engglmer2b,start=ss,control=glmerControl(optimizer="bobyqa",
                                                          optCtrl=list(maxfun=2e5)))

car::Anova(englmer2) #lmer dataEng
car::Anova(englmer2b); #lmer with subset(dataEng, abs(scale(resid(englmer2)))<2.5)
summary(englmer2); 
summary(englmer2b); 

car::Anova(engglmer2) #GLMM dataEng
car::Anova(engglmer2b); #GLMM with subset(dataEng, abs(scale(resid(engglmer2)))<2.5)
summary(engglmer2); 
summary(engglmer2b); 

#transparent versus opaque condition:
dataEng$morphType <- relevel(dataEng$morphType, "op");
englmer2c <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + lengthTarget + (1|subject) + (1|target), data=dataEng, REML = T);
englmer2d <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + lengthTarget + (1|subject) + (1|target), data=subset(dataEng, abs(scale(resid(englmer2c)))<2.5), REML = T);
summary(englmer2d); #here we get the contrast between transparent and opaque pairs

engglmer2c <- glmer(rt ~ relatedness * morphType + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, family=Gamma(link="identity")); 
ss <- getME(engglmer2c,c("theta","fixef"))
engglmer2c<-update(engglmer2c,start=ss,control=glmerControl(optimizer="bobyqa",
                                                           optCtrl=list(maxfun=2e5)))

engglmer2d <- glmer(rt ~ relatedness * morphType + freqTarget + lengthTarget + (1|subject) + (1|target), 
                    data = subset(dataEng, abs(scale(resid(engglmer2c)))<2.5), family=Gamma(link="identity")); 
ss <- getME(engglmer2d,c("theta","fixef"))
engglmer2d<-update(engglmer2d,start=ss,control=glmerControl(optimizer="bobyqa",
                                                           optCtrl=list(maxfun=2e5)))


#plot residuals
a<-plot(engglmer2, ylab = 'ENG Residuals', xlab = 'Predicted RT', 
        main = 'ENG GLMM Gamma Distribution identity link');
b<-plot(englmer2,  ylab = 'ENG Residuals', xlab = 'Predicted RT', 
        main = 'ENG LMM -1000/RT');
c<-plot(englmer2a,  ylab = 'ENG Residuals', xlab = 'Predicted RT', 
        main = 'ENG LMM raw RT');
d<-plot(englmer2b,  ylab = 'ENG Residuals', xlab = 'Predicted RT', 
        main = 'ENG LMM -1000/RT with model trimming');
e<-plot(engglmer2b,  ylab = 'ENG Residuals', xlab = 'Predicted RT', 
        main = 'ENG GLMM with model trimming');


z<- ggpubr::ggarrange(c, b, a, d, e, ncol = 3, nrow = 2);
z
ggsave('residualPlotENG.png',z, dpi = 300, width = 9, height = 5);
rm(a,b,c,d,e);


#------------------------------#
#### plots of estimated RTs ####
#------------------------------#
# figure 1 of the paper
df <- effect("relatedness:morphType",itaglmer2);
df <- as.data.frame(df);
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


df <- effect("relatedness:morphType",engglmer2); 
df <- as.data.frame(df);
revalue(df$relatedness, c("ctrl"="Unrelated"))-> df$relatedness;
revalue(df$relatedness, c("rel"="Related"))-> df$relatedness;

dodge <- position_dodge(width = 0.25);
gg  <-ggplot(data = df, aes(x = relatedness, y = fit,group = morphType)) + 
  geom_point(size = 2, position = dodge) + 
  geom_line(aes(linetype=morphType), position = dodge) + 
  scale_linetype_manual(values=c( "dotted", "dashed", "solid")) + 
  theme_classic();
gg  <- gg + geom_pointrange(aes(ymin = df$lower, ymax = df$upper), position = dodge);
gg  <- gg + scale_y_continuous("RT (ms)") ;
gg  <- gg + theme(axis.text.y = element_text(angle = 00, hjust = 1, size=10, colour = 'black'));
gg  <- gg + theme(axis.title.x = element_blank()) + theme(axis.text.x = element_text(size=13, colour = 'black'));
gg <- gg + labs(title='L2 - English');
gg <- gg + theme(plot.title= element_text(angle = 00, hjust=0.5, size=15, face = 'bold', colour = 'black'));
gg + theme(legend.position="none");

ggsave("engplot.jpg", width = 4, height = 3, dpi = 300);
rm(gg, bb);

rm(engglmer2, itaglmer2);


plotLMER.fnc(engglmer2, pred = "relatedness", intr = list("morphType", c("or", "op", "tr"), "end"))
plot(allEffects(engglmer2))
#----------------------------------#
#### cross language interaction ####
#----------------------------------#
rbind(dataEng, dataIta) -> crossExp;
summary(crossExp);

crosslmer <- lmer(-1000/rt ~ relatedness * morphType * language + freqTarget + lengthTarget + (1|subject) + (1|target), data = crossExp, REML = T);
crosslmerb <- lmer(-1000/rt ~ relatedness * morphType * language + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(crossExp, abs(scale(resid(crosslmer)))<2.5), REML = T);
anova(crosslmer);

crossglmer <- glmer(rt ~ relatedness * morphType * language + freqTarget + lengthTarget + (1|subject) + (1|target), data = crossExp, family=Gamma(link="identity"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))); 

car::Anova(crossglmer);
summary(crossglmer); #residuals are quite symmetrical

anova(crossglmer);
#--------------------------------------------------------#
#### proficiency scores, correlation and distribution ####
#--------------------------------------------------------#
#create a database with one line per ppt
pptFeatures <- unique(dataEng[,c('subject','age','gender','handedness','rotation','phonemicFluency', 'phonemicComprehension','morphComprehension','spelling','readingComprehension','vocabulary','oralComprehension','aoa1.Aoa', 'aoa2.usage', 'aoa3.context','aoa4.contextMultling','aoa5.selfRatedProf','aoa6.otherLang')]);
summary(pptFeatures);

car::vif(proficiencyglmer6)



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
#### proficiency modelling LMMs and GLMMs ####
#-----------------------------#
dataEng$morphType <- relevel(dataEng$morphType, "or");

#overall improvement in goodness of fit
proficiencylmer0 <- lmer(-1000/rt ~ relatedness * morphType + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F); #this establishes the baseline model, with no proficiency score
proficiencyglmer0 <- glmer(rt ~ relatedness * morphType  + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, family=Gamma(link="identity"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))

car::Anova(proficiencylmer0);
anova(proficiencylmer0)
car::Anova(proficiencyglmer0);
summary(proficiencyglmer0);

#here we test whether each individual proficiency score guarantees a better overall fit
proficiencylmer1 <- lmer(-1000/rt ~ relatedness * morphType * phonemicFluency  + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(proficiencylmer0, proficiencylmer1);

proficiencyglmer1 <- glmer(rt ~ relatedness  * morphType * phonemicFluency + lengthTarget + freqTarget + (1|subject) 
      + (1|target), data = dataEng, family=Gamma(link="identity"));
ss <- getME(proficiencyglmer1,c("theta","fixef"));
proficiencyglmer1<-update(proficiencyglmer1,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                            optCtrl=list(maxfun=2e5)));

anova(proficiencyglmer0, proficiencyglmer1); #Probably not working with glmer models

#trim a la bayeen
proficiencylmer1b <- lmer(-1000/rt ~ relatedness *  morphType * phonemicFluency + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer1)))<2.5), REML = F);


car::Anova(proficiencylmer1);
car::Anova(proficiencylmer1b);
summary(proficiencylmer1b); 
car::Anova(proficiencyglmer1);
car::Anova(proficiencyglmer1b);
summary(proficiencyglmer1b); 

proficiencylmer2 <- lmer(-1000/rt ~ relatedness *  morphType * phonemicComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(proficiencylmer0, proficiencylmer2);

proficiencyglmer2<- glmer(rt ~ relatedness  * morphType * phonemicComprehension + lengthTarget + freqTarget + (1|subject) 
         + (1|target), data = dataEng, family=Gamma(link="identity"));
ss <- getME(proficiencyglmer2,c("theta","fixef"));
proficiencyglmer2<-update(proficiencyglmer2,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                          optCtrl=list(maxfun=2e5)));

car::Anova(proficiencylmer2);
anova(proficiencylmer2);
summary(proficiencylmer2);
anova(proficiencyglmer2);
car::Anova(proficiencyglmer2);
summary(proficiencyglmer2); 

#trimming method a la bayeen
proficiencylmer2b <- lmer(-1000/rt ~ relatedness *  morphType * phonemicComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer2)))<2.5), REML = F);

proficiencyglmer2b<- glmer(rt ~ relatedness  * morphType * phonemicComprehension + lengthTarget + freqTarget + (1|subject) 
                          + (1|target), data = subset(dataEng, abs(scale(resid(proficiencyglmer2)))<2.5), family=Gamma(link="identity"));
ss <- getME(proficiencyglmer2b,c("theta","fixef"));
proficiencyglmer2b<-update(proficiencyglmer2b,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                          optCtrl=list(maxfun=2e5)));
car::Anova(proficiencylmer2);
car::Anova(proficiencylmer2b);
summary(proficiencylmer2b);
car::Anova(proficiencyglmer2);
car::Anova(proficiencyglmer2b); #interesting here is less significant
summary(proficiencyglmer2b); 


proficiencylmer3 <- lmer(-1000/rt ~ relatedness *  morphType * morphComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(proficiencylmer0,proficiencylmer3);

proficiencyglmer3<- glmer(rt ~ relatedness  * morphType * morphComprehension + lengthTarget + freqTarget + (1|subject) + (1|target), data = dataEng, family=Gamma(link="identity"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))); 
ss <- getME(proficiencyglmer3,c("theta","fixef"));
proficiencyglmer3<-update(proficiencyglmer3,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                            optCtrl=list(maxfun=2e5)));

car::Anova(proficiencyglmer3);
car::Anova(proficiencylmer3);

#trim a la bayeen
proficiencylmer3b <- lmer(-1000/rt ~ relatedness *  morphType * morphComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer3)))<2.5), REML = F);

proficiencyglmer3b<- glmer(rt ~ relatedness  * morphType * morphComprehension + lengthTarget + freqTarget + (1|subject) 
                          + (1|target), data = subset(dataEng, abs(scale(resid(proficiencyglmer3)))<2.5), family=Gamma(link="identity")); 
ss <- getME(proficiencyglmer3b,c("theta","fixef"));
proficiencyglmer3b<-update(proficiencyglmer3b,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                          optCtrl=list(maxfun=2e5)));



proficiencylmer4 <- lmer(-1000/rt ~ relatedness * morphType * spelling + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(proficiencylmer0, proficiencylmer4);

proficiencyglmer4<- glmer(rt ~ relatedness  * morphType * spelling + lengthTarget + freqTarget + (1|subject) + (1|target), data = dataEng, family=Gamma(link="identity"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)));

car::Anova(proficiencylmer4);
anova(proficiencylmer4);
summary(proficiencylmer4);
anova(proficiencyglmer4);
car::Anova(proficiencyglmer4);
summary(proficiencyglmer4);

#trim a la bayeen
proficiencylmer4b <- lmer(-1000/rt ~ relatedness * morphType * spelling + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer4)))<2.5), REML = F);

proficiencyglmer4b<- glmer(rt ~ relatedness  * morphType * spelling + lengthTarget + freqTarget + (1|subject) 
                          + (1|target), data = subset(dataEng, abs(scale(resid(proficiencyglmer4)))<2.5), family=Gamma(link="identity"));
ss <- getME(proficiencyglmer4b,c("theta","fixef"));
proficiencyglmer4b<-update(proficiencyglmer4b,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                          optCtrl=list(maxfun=2e5)));



proficiencylmer5 <- lmer(-1000/rt ~ relatedness * morphType * readingComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(proficiencylmer0, proficiencylmer5); 

proficiencyglmer5<- glmer(rt ~ relatedness  * morphType * readingComprehension + lengthTarget + freqTarget + (1|subject) + (1|target), data = dataEng, family=Gamma(link="identity"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)));
ss <- getME(proficiencyglmer5,c("theta","fixef"));
proficiencyglmer5<-update(proficiencyglmer5,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                          optCtrl=list(maxfun=2e5)));

#trim a la bayeen
proficiencylmer5b <- lmer(-1000/rt ~ relatedness * morphType * readingComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng, abs(scale(resid(proficiencylmer5)))<2.5), REML = F);
anova(proficiencylmer0, proficiencylmer5); 

proficiencyglmer5b<- glmer(rt ~ relatedness  * morphType * readingComprehension + lengthTarget + freqTarget + (1|subject) + (1|target), 
                           data = subset(dataEng, abs(scale(resid(proficiencyglmer5)))<2.5), nAGQ=0, family=Gamma(link="identity"));
ss <- getME(proficiencyglmer5b,c("theta","fixef"));
proficiencyglmer5b<-update(proficiencyglmer5b,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                          optCtrl=list(maxfun=2e5)), , REML =T);

proficiencylmer6 <- lmer(-1000/rt ~ relatedness * morphType * vocabulary + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(proficiencylmer0,proficiencylmer6);

proficiencyglmer6<- glmer(rt ~ relatedness  * morphType * vocabulary + lengthTarget + freqTarget + (1|subject) + (1|target),  data = dataEng, family=Gamma(link="identity"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)));
ss <- getME(proficiencyglmer6,c("theta","fixef"));
proficiencyglmer6<-update(proficiencyglmer6,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                          optCtrl=list(maxfun=2e5)));
#trim a la bayeen

proficiencylmer6b <- lmer(-1000/rt ~ relatedness * morphType * vocabulary + freqTarget + lengthTarget + (1|subject) + (1|target), 
                         data = subset(dataEng, abs(scale(resid(proficiencylmer6)))<2.5), REML = F);

proficiencyglmer6b<- glmer(rt ~ relatedness  * morphType * vocabulary + lengthTarget + freqTarget + (1|subject) 
                          + (1|target), data = subset(dataEng, abs(scale(resid(proficiencyglmer6)))<2.5), family=Gamma(link="identity"));
ss <- getME(proficiencyglmer6b,c("theta","fixef"));
proficiencyglmer6b<-update(proficiencyglmer6b,start=ss,control=glmerControl(optimizer="bobyqa",
                                                                          optCtrl=list(maxfun=2e5)));



proficiencylmer7 <- lmer(-1000/rt ~ relatedness * morphType * oralComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(proficiencylmer0,proficiencylmer7);

proficiencyglmer7<- glmer(rt ~ relatedness  * morphType * oralComprehension + lengthTarget + freqTarget + (1|subject) + (1|target), data = dataEng, family=Gamma(link="identity"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)));

proficiencyglmer8<- glmer(rt ~ relatedness  * morphType * vocabulary * spelling + lengthTarget + freqTarget + (1|subject) + (1|target), data = dataEng, family=Gamma(link="identity"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)));

car::vif(proficiencyglmer8)


anova(proficiencylmer7, proficiencyglmer7);
car::Anova(proficiencylmer7);
anova(proficiencylmer7);
summary(proficiencylmer7);
anova(proficiencyglmer7);
car::Anova(proficiencyglmer7);
summary(proficiencyglmer7);

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

#what sort of effect this is? [Figure 4 in the paper]
temp <- data.frame(effect('relatedness:morphType:morphComprehension', proficiencyglmer3, se=list(level=.95), xlevels=list(morphComprehension=quantile(dataEng$morphComprehension, probs=c(.05,.50,.95)))));
revalue(temp$relatedness, c("rel"="Related"))-> temp$relatedness;
revalue(temp$relatedness, c("ctrl"="Unrelated"))-> temp$relatedness;

morphComprehension_names <- c(
  "6" = "Low morphComprehension",
  "9" = "Medium morphComprehension",
  "10" = "High morphComprehension");

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
  facet_grid(~ morphComprehension, 
             labeller = labeller(morphComprehension = as_labeller(morphComprehension_names))) +
  theme(strip.text = element_text(size=12)) + 
  theme(legend.position="none");

ggsave("morphComprehensionModel.jpg", width = 7.5, height = 3, dpi = 300)


#GLMM phon comprehension
#what sort of effect this is? [Figure 3 in the paper]
temp <- data.frame(effect('relatedness:morphType:phonemicFluency', proficiencyglmer1, se=list(level=.95), xlevels=list(phonemicFluency=quantile(dataEng$phonemicFluency, probs=c(.05,.50,.95)))));
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


#GLMM vocabulary [figure 5 paper]
temp <- data.frame(effect('relatedness:morphType:vocabulary', proficiencyglmer6, se=list(level=.95), xlevels=list(vocabulary=quantile(dataEng$vocabulary, probs=c(.05,.50,.95)))));
revalue(temp$relatedness, c("rel"="Related"))-> temp$relatedness;
revalue(temp$relatedness, c("ctrl"="Unrelated"))-> temp$relatedness;

vocabulary_names <- c(
  "11" = "Low vocabulary",
  "16" = "Medium vocabulary",
  "19" = "High vocabulary");

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
  facet_grid(~ vocabulary, 
             labeller = labeller(vocabulary = as_labeller(vocabulary_names))) +
  theme(strip.text = element_text(size=12)) + 
  theme(legend.position="none");

ggsave("vocabularyModel.jpg", width = 7.5, height = 3, dpi = 300)


#GLMM w/ phon fluency [figure 6]
temp <- data.frame(effect('relatedness:morphType:phonemicFluency', proficiencyglmer1, se=list(level=.95), xlevels=list(phonemicFluency=quantile(dataEng$phonemicFluency, probs=c(.05,.50,.95)))));
revalue(temp$relatedness, c("rel"="Related"))-> temp$relatedness;
revalue(temp$relatedness, c("ctrl"="Unrelated"))-> temp$relatedness;

phonemicFluency_names <- c(
  "10" = "Low phonFluency",
  "23" = "Medium phonFluency",
  "39" = "High phonFluency");

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

ggsave("phonFluencyModel.jpg", width = 7.5, height = 3, dpi = 300)

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

aoaglmer1<- glmer(rt ~ relatedness * morphType*aoa1.Aoa + freqTarget + (1|subject) + (1|target), data= dataEng, family=Gamma(link="identity"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)));
anova(proficiencyglmer0, aoaglmer1);


#other AoA questionnaire scores
aoalmer2 <- lmer(-1000/rt ~ relatedness*morphType*aoa2.usage + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(proficiencylmer0, aoalmer2);

aoaglmer2c<- glmer(rt ~ relatedness * morphType*aoa2.usage + freqTarget + (1|subject) + (1|target), data= dataEng, family=Gamma(link="identity"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)));
anova(proficiencyglmer0, aoaglmer2);

aoalmer3 <- lmer(-1000/rt ~ relatedness*morphType*aoa3.context + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(proficiencylmer0, aoalmer3);

aoaglmer3c<- glmer(rt ~ relatedness * morphType*aoa3.context + freqTarget + (1|subject) + (1|target), data= dataEng, family=Gamma(link="identity"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)));
anova(proficiencyglmer0, aoaglmer3);

aoalmer4 <- lmer(-1000/rt ~ relatedness*morphType*aoa4.contextMultling + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(proficiencylmer0, aoalmer4);

aoaglmer4c<- glmer(rt ~ relatedness * morphType*aoa4.contextMultling + freqTarget + (1|subject) + (1|target), data= dataEng, family=Gamma(link="identity"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)));

aoalmer5 <- lmer(-1000/rt ~ relatedness*morphType*aoa5.selfRatedProf + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(proficiencylmer0, aoalmer5);

aoaglmer5<- glmer(rt ~ relatedness * morphType*aoa5.selfRatedProf + freqTarget + lengthTarget + (1|subject) + (1|target), data= dataEng, family=Gamma(link="identity"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)));

aoalmer6 <- lmer(-1000/rt ~ relatedness*morphType*aoa6.otherLang + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, REML = F);
anova(proficiencylmer0, aoalmer6);

aoaglmer6c<- glmer(rt ~ relatedness * morphType*aoa6.otherLang + freqTarget + lengthTarget + (1|subject) + (1|target), data= dataEng, family=Gamma(link="identity"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)));

emmeans::emmeans(aoalmer5b, list(pairwise ~ relatedness*morphType*aoa5.selfRatedProf), adjust = "bonferroni");

# ctrl,or,3.52915053518561 - rel,or,3.52915053518561   0.044218 0.01033 Inf  4.282  0.0003 
# ctrl,tr,3.52915053518561 - rel,tr,3.52915053518561   0.105024 0.00950 Inf 11.053  <.0001 
# ctrl,op,3.52915053518561 - rel,op,3.52915053518561   0.075141 0.00997 Inf  7.537  <.0001 

#aoa1 figure 8 paper
temp <- data.frame(effect('relatedness:morphType:aoa1.Aoa', aoaglmer1, se=list(level=.95), xlevels=list(aoa1.Aoa=quantile(dataEng$aoa1.Aoa, probs=c(.05,.50,.95)))));
revalue(temp$relatedness, c("rel"="Related"))-> temp$relatedness;
revalue(temp$relatedness, c("ctrl"="Unrelated"))-> temp$relatedness;

aoa1.Aoa_names <- c(
  "2" = "2 years",
  "6" = "6 years",
  "11" = "11 years");

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
  facet_grid(~ aoa1.Aoa, 
             labeller = labeller(aoa1.Aoa = as_labeller(aoa1.Aoa_names))) +
  theme(strip.text = element_text(size=12)) + 
  theme(legend.position="none");

ggsave("aoa1.AoA.jpg", width = 7, height = 3, dpi = 300);


#aoa 2 figure 9 paper
temp <- data.frame(effect('relatedness:morphType:aoa2.usage', aoaglmer2c, se=list(level=.95), xlevels=list(aoa2.usage=quantile(dataEng$aoa2.usage, probs=c(.05,.50,.95)))));
revalue(temp$relatedness, c("rel"="Related"))-> temp$relatedness;
revalue(temp$relatedness, c("ctrl"="Unrelated"))-> temp$relatedness;

aoa2.usage_names <- c(
  "1" = "1 time per day",
  "3" = "3 times per day ",
  "5" = "5 times per day");

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
  facet_grid(~ aoa2.usage, 
             labeller = labeller(aoa2.usage = as_labeller(aoa2.usage_names))) +
  theme(strip.text = element_text(size=12)) + 
  theme(legend.position="none");

ggsave("aoa2.usage.jpg", width = 7, height = 3, dpi = 300);

#aoa 2 figure 10 paper
temp <- data.frame(effect('relatedness:morphType:aoa5.selfRatedProf', aoaglmer5c, se=list(level=.95), xlevels=list(aoa5.selfRatedProf=quantile(dataEng$aoa5.selfRatedProf, probs=c(.05,.50,.95)))));
revalue(temp$relatedness, c("rel"="Related"))-> temp$relatedness;
revalue(temp$relatedness, c("ctrl"="Unrelated"))-> temp$relatedness;

aoa5.selfRatedProf_names <- c( 
  "2" = "Low self-rated proficiency",
  "3" = "Medium self-rated proficiency",
  "5" = "High self-rated proficiency");

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
  facet_grid(~ aoa5.selfRatedProf, 
             labeller = labeller(aoa5.selfRatedProf = as_labeller(aoa5.selfRatedProf_names))) +
  theme(strip.text = element_text(size=12)) + 
  theme(legend.position="none");

ggsave("aoa5.selfratedProf.jpg", width = 7.5, height = 3, dpi = 300);

#-----------#
#### osc ####
#-----------#
#the best model for English is now proficiencyglmer6

#first, let's try to pit OSC against priming condition -- these two are typically confounded:
temp <- unique(masterFile[masterFile$lexicality=='word' & masterFile$language=='eng',c('target','prime','morphType','relatedness','freqTarget','freqPrime','lengthTarget','lengthPrime','nTarget','nPrime','oscTarget')]);
summary(temp);
aggregate(oscTarget ~ morphType, FUN=fivenum, data=temp); #indeed they are

#this represents this graphically [figure 11 in the paper]
revalue(temp$morphType, c("or"="Orthographic", 'op'='Opaque', 'tr'='Transparent'))-> temp$morphType;
library(ggpubr);
ggboxplot(subset(temp, oscTarget>0), "morphType", "oscTarget",
          color = "black", fill = grey(.80),
          width = 0.5, ylab = 'OSC', xlab = ''); 

ggsave("oscMorph.jpg", width = 4, height = 3, dpi = 300);

#and this tests it via NHST
summary(aov(oscTarget~morphType, data=subset(temp, relatedness=='rel')));

#modelling
osc1 <- glmer(rt ~ relatedness *  oscTarget * phonemicFluency + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, family=Gamma(link="identity"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)));
car::Anova(osc1);

osc2 <- glmer(rt ~ relatedness *  oscTarget * vocabulary + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, family=Gamma(link="identity"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)));
car::Anova(osc2);

osc3 <- glmer(rt ~ relatedness *  oscTarget * phonemicComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, family=Gamma(link="identity"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)));
car::Anova(osc3);

osc4 <- glmer(rt ~ relatedness *  oscTarget * morphComprehension + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng, family=Gamma(link="identity"), control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)));



#[figure 12 in the paper]
temp <- data.frame(effect('relatedness:oscTarget:phonemicFluency', osc1, se=list(level=.95), xlevels=list(oscTarget=c(.20,.80), phonemicFluency=quantile(dataEng$phonemicFluency, probs=c(.05,.50,.95)))));
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

#[figure 13 in the paper]
temp <- data.frame(effect('relatedness:oscTarget:vocabulary', osc2, se=list(level=.95), xlevels=list(oscTarget=c(.20,.80), vocabulary=quantile(dataEng$vocabulary, probs=c(.05,.50,.95)))));
revalue(temp$relatedness, c("rel"="Related"))-> temp$relatedness;
revalue(temp$relatedness, c("ctrl"="Unrelated"))-> temp$relatedness;

vocabulary_names <- c(
  "11" = "Low Vocabulary",
  "16" = "Medium Vocabulary",
  "19" = "High Vocabulary"
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
  facet_grid(~ vocabulary, 
             labeller = labeller(vocabulary = as_labeller(vocabulary_names))) +
  theme(strip.text = element_text(size=12))+
  theme(legend.position="none");

ggsave("vocabulary.jpg", width = 7.5, height = 3, dpi = 300);

#[figure 14 in the paper]
temp <- data.frame(effect('relatedness:oscTarget:morphComprehension', osc4, se=list(level=.95), xlevels=list(oscTarget=c(.20,.80), morphComprehension=quantile(dataEng$morphComprehension, probs=c(.05,.50,.95)))));
revalue(temp$relatedness, c("rel"="Related"))-> temp$relatedness;
revalue(temp$relatedness, c("ctrl"="Unrelated"))-> temp$relatedness;

morphComprehension_names <- c(
  "6" = "Low morphComprehension",
  "9" = "Medium morphComprehension",
  "10" = "High morphComprehension"
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
  facet_grid(~ morphComprehension, 
             labeller = labeller(morphComprehension = as_labeller(morphComprehension_names))) +
  theme(strip.text = element_text(size=12))+
  theme(legend.position="none");

ggsave("figure14.jpg", width = 7.7, height = 3, dpi = 300);

#bonus track: we check whether OSC explains data better than morphological condition
extractAIC(osc1);
extractAIC(engglmer2);
extractAIC(proficiencyglmer1); #now it doesn't anymore


#Laura comment on bare Lexical decision times
osc2 <- lmer(-1000/rt ~   oscTarget * phonemicFluency + freqTarget + lengthTarget + (1|subject) + (1|target), data = dataEng[dataEng$relatedness=='ctrl',], REML = F);
anova(osc2);
osc2b <- lmer(-1000/rt ~  oscTarget * phonemicFluency + freqTarget + lengthTarget + (1|subject) + (1|target), data = subset(dataEng[dataEng$relatedness=='ctrl',], abs(scale(resid(osc2)))<2), REML = T);
anova(osc2b);
summary(osc2b); #very solid 3-way interaction



#### Analysis of Errors ####
summary(dataEngAcc);
dataEngAcc2 <- subset(dataEngAcc, rt>300 & rt<2000 & subject!=15 & subject!=22 & subject!=43);

#The overall accuracy is 76.7%
aggregate(accuracy ~ subject + phonemicComprehension, data = dataEngAcc2, mean)-> distrppt;
head(distrppt, 10)-> firstppt
tail(distrppt, 10)-> lastppt
(mean(lastppt$accuracy) - mean(firstppt$accuracy))*100
(nrow(dataEng[dataEng$subject %in% lastppt$subject,])/10) - (nrow(dataEng[dataEng$subject %in% firstppt$subject,])/10);

head(distrppt, 5)-> firstppt
tail(distrppt, 5)-> lastppt
(mean(lastppt$accuracy) - mean(firstppt$accuracy))*100
(nrow(dataEng[dataEng$subject %in% lastppt$subject,])/5) - (nrow(dataEng[dataEng$subject %in% firstppt$subject,])/5);

head(distrppt, 15)-> firstppt
tail(distrppt, 15)-> lastppt
(mean(lastppt$accuracy) - mean(firstppt$accuracy))*100
(nrow(dataEng[dataEng$subject %in% lastppt$subject,])/15) - (nrow(dataEng[dataEng$subject %in% firstppt$subject,])/15);

#Q1: how these errors are distributed across the three prime conditions?#
mean(dataEngAcc2[dataEngAcc2$morphType=='tr',]$accuracy); 
mean(dataEngAcc2[dataEngAcc2$morphType=='op',]$accuracy); 
mean(dataEngAcc2[dataEngAcc2$morphType=='or',]$accuracy); 
#more correct datapoints in the transparent condition

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
#visualization of the overall Accuracy for the reviewers
library(dplyr);
ss_prop <- dataEngAcc2 %>% 
  group_by(subject, relatedness, morphType) %>% 
  dplyr::summarise(mean_correct = mean(accuracy)) 


ss_prop <- dataEngAcc2 %>% 
  group_by(subject, relatedness, morphType) %>%
  dplyr::summarise(mean_RT = mean(rt)) 

df2 <- ss_prop %>%
  group_by(morphType, relatedness) %>%
  dplyr::summarise( 
    n=n(),
    mean=mean(mean_RT),
    sd=sd(mean_RT)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))



#plot
df3 <- ss_prop %>%
  group_by(morphType, relatedness) %>%
  summarise( 
    n=n(),
    mean=mean(mean_correct),
    sd=sd(mean_correct)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

revalue(df3$relatedness, c("ctrl"="unrelated"))-> df3$relatedness;
revalue(df3$relatedness, c("rel"="related"))-> df3$relatedness;
revalue(df3$morphType, c("op"="opaque"))-> df3$morphType;
revalue(df3$morphType, c("or"="orthographic"))-> df3$morphType;
revalue(df3$morphType, c("tr"="transparent"))-> df3$morphType;


p1<-ggplot(aes(x = morphType, y = mean, fill = relatedness), data = df3) +
  geom_bar(stat = "identity", color='white', position=position_dodge(), size=1.2) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.15, size=1,position=position_dodge(.9)) +
  ylab("Accuracy ") +
  xlab("Morphological type") +
  coord_cartesian(ylim = c(0.5, 1))+
  ggpubr::theme_pubclean() + 
  #scale_fill_grey() +
  theme(legend.position="top", legend.title = element_blank()) +
  theme(text = element_text(size=20)) ;

ggpubr::ggexport(p1, filename = 'Accuracy.jpg', res = 300, width = 2000, height = 1200);


aggregate(accuracy ~ subject, data = dataEngAcc2, mean)->

#plot of each proficiency metric against error rate
#### All proficiency accuracy graph####

dataEngAcc2$phonemicComprehension <- scale(dataEngAcc2$phonemicComprehension, scale = T)
dataEngAcc2$phonemicFluency <- scale(dataEngAcc2$phonemicFluency, scale = T)
dataEngAcc2$morphComprehension <- scale(dataEngAcc2$morphComprehension, scale = T)
dataEngAcc2$spelling <- scale(dataEngAcc2$spelling, scale = T)
dataEngAcc2$readingComprehension <- scale(dataEngAcc2$readingComprehension, scale = T)
dataEngAcc2$vocabulary <- scale(dataEngAcc2$vocabulary, scale = T)
dataEngAcc2$oralComprehension <- scale(dataEngAcc2$oralComprehension, scale = T)

ss_prop <- dataEngAcc2 %>% 
  group_by(subject, target, phonemicFluency,
           phonemicComprehension, morphComprehension, spelling, readingComprehension,
           vocabulary, oralComprehension) %>% 
  dplyr::summarise(mean_correct = mean(accuracy))

ss_prop <- dataEngAcc2 %>% 
  group_by(subject, target, phonemicFluency,
           phonemicComprehension, morphComprehension, spelling, readingComprehension,
           vocabulary, oralComprehension) %>%
  dplyr::summarise(mean_rt = mean(rt)) %>% 
  left_join(ss_prop)


melt(ss_prop, id=c("subject", "target",
                   "mean_rt", "mean_correct")) -> mdata;


ms <- mdata %>%
  group_by(subject, variable, value) %>% 
  langcog::multi_boot_standard(col = "mean_correct") 

accplot<-ggpubr::ggscatter(ms, x = "value", y = "mean",
                           legend = "bottom",
                           show.legend.text = NA,
                           alpha = 0.8) +
  geom_point(shape = 21, fill = "white", size = 3) +
  ggpubr::theme_pubclean()+
  labs(x=" ", y = "mean accuracy") +
  theme(legend.title=element_blank()) +
  coord_cartesian(ylim = c(0.25, 1)) + 
  facet_wrap(.~variable, ncol = 3) +
  ggpubr::stat_cor(method = "pearson", label.x = 1.5, label.y = 0.4);


accplot;

ggpubr::ggexport(accplot, filename = 'AccuracyallProficiency.jpg', res = 300, width = 2800, height = 1800);

#### Speed-accuracy trade-off####
#Q3: careful considerate how the error data may or may not affect the interpretation of the RT results
#Calculating median reaction times for each condition in the related condition only

ss_prop <- dataEngAcc2 %>% 
  group_by(subject, target, morphType) %>% 
  dplyr::summarise(mean_correct = mean(accuracy))

ss_prop <- dataEngAcc2 %>% 
  group_by(subject, target, morphType) %>%
  dplyr::summarise(mean_rt = mean(rt)) %>% 
  left_join(ss_prop)

ms_rt <- ss_prop %>%
  #filter(relatedness == "rel") %>%
  group_by(morphType, mean_correct) %>% 
  langcog::multi_boot_standard(col = "mean_rt", na.rm = T, empirical_function = "mean")

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
n_bins <- 10
break_seq <- seq(0, rt_range, rt_range/n_bins)

timeslice_range <- dataEngAcc2 %>%
  #filter(relatedness == "rel", is.na(rt) == F) %>%
  dplyr::mutate(RT_bin = cut(rt, breaks = break_seq)) %>%
  dplyr::group_by(RT_bin, morphType) %>%
  dplyr::mutate(RT_bin_avg = mean(rt, na.rm = T))

timeslice_range <- timeslice_range %>%
  dplyr::group_by(RT_bin_avg, morphType, target) %>% 
  dplyr::summarise(ss_acc = mean(accuracy, na.rm=T)) %>% 
  dplyr::group_by(RT_bin_avg, morphType) %>%
  dplyr::summarise(mean = mean(ss_acc),
            n = n())

p3<-ggplot(aes(x=RT_bin_avg, y=mean, weight = n), 
           data = timeslice_range) + 
  geom_point(aes(size = n), shape = 21, fill = "white", stroke = 1.5) +
  geom_smooth(method = "lm", formula = y ~ poly(x,2), se = TRUE, color = "#0892d0", fill = "lightgray") +
  geom_hline(yintercept = 0.5, lty = "dashed", color = 'red') +
  coord_cartesian(ylim = c(0, 1))+
  ggthemes::theme_hc()+
  xlab("Average RT") +
  ylab("Proportion Correct")

p3;

ggpubr::ggexport(p3, filename = 'AccuracyRTradeoff.jpg', res = 300, width = 2500, height = 1300);

aggregate(accuracy ~ subject, dataEngAcc2, mean)-> speedacc
aggregate(rt ~ subject, dataEng, mean)-> speedacc2
merge(speedacc, speedacc2, by = "subject")-> speedacc

p3<-ggplot(aes(x=rt, y=accuracy), 
           data = speedacc) + 
  geom_point( shape = 21, fill = "white", size = 3, stroke = 1.5) +
  #geom_smooth(method = "lm", formula = y ~ poly(x,2), se = TRUE, color = "#0892d0", fill = "lightgray") +
  geom_hline(yintercept = 0.5, lty = "dashed", color = 'red') +
  coord_cartesian(ylim = c(0.4, 1))+
  ggthemes::theme_hc()+
  xlab("Average RT") +
  ylab("Proportion Correct")

p3;
ggpubr::ggexport(p3, filename = 'AccuracyRTradeoffbysubj.jpg', res = 300, width = 2500, height = 1300);


#### Reliability analysis ####
#internal consistency of the proficiency measures
pptFeatures <- unique(dataEng[,c('subject','age','gender','handedness','rotation','phonemicFluency', 'phonemicComprehension','morphComprehension','spelling','readingComprehension','vocabulary','oralComprehension','aoa1.Aoa', 'aoa2.usage', 'aoa3.context','aoa4.contextMultling','aoa5.selfRatedProf','aoa6.otherLang')]);

psych::alpha(pptFeatures[,6:12], check.keys = TRUE) #great value

psych::splitHalf(pptFeatures[,6:12], raw = T, n.sample = 5000) #split-half reliability
multicon::splithalf.r(pptFeatures[,6:12], sims = 5000);
multicon::alpha.cov(cov(pptFeatures[,6:12]));




####split half reliability of the priming####
#Method 1
library(data.table);
#in order to run the split half test, we need:
#1. to split in two halves the ENG datasets

dataEng[c(TRUE, FALSE), ]->coreven;
dataEng[c(FALSE, TRUE), ]->corodd;


table(coreven$subject);
table(corodd$subject);
table(dataEng$subject);

#3. compute for each subject the priming effect in the two halves separately
library(tidyverse);
dataEngeven <- NULL;

for (i in 1:length(unique(coreven$subject))){
  if (i %in% unique(coreven$subject)){
    s1<-dcast(coreven[coreven$subject==i,],  morphType ~ relatedness , value.var = "rt", mean) %>% 
        mutate(primingeven = rel - ctrl) %>% 
        select(morphType, primingeven);
    s1$subject <- i
    dataEngeven <- rbind(dataEngeven, s1)
  } else 
    print('skip')
  {
    next
    }
};

dataEngodd <- NULL;

for (i in 1:length(unique(corodd$subject))){
  if (i %in% unique(corodd$subject)){
    s1<-dcast(corodd[corodd$subject==i,],  morphType ~ relatedness , value.var = "rt", mean) %>% 
        mutate(primingodd = rel - ctrl) %>% 
        select(morphType, primingodd);
    s1$subject <- i
    dataEngodd <- rbind(dataEngodd, s1)
  } else 
    print('skip')
  {
    next
    }
};

#let's put the two halves in the same dataset
dataEnghalf <- cbind(dataEngodd, dataEngeven);
dataEnghalf$subject <- NULL; #double column
dataEnghalf$morphType <- NULL; #double column
#rm(dataEngodd, dataEngeven);
summary(dataEnghalf);

#3 correlate the two halves

psych::splitHalf(dataEnghalf[,c('primingodd', 'primingeven')], 
                 n.sample = 5000, use = 'pairwise');
multicon::splithalf.r(dataEnghalf[,c('primingodd', 'primingeven')], sims = 5000);
multicon::alpha.cov(cov(dataEnghalf[,c('primingodd', 'primingeven')], use="p"));


#ICC analysis
library(irr)
d<- NULL
dataEngodd[dataEngodd$morphType=='or',]$primingodd-> d$orodd
dataEngeven[dataEngeven$morphType=='or',]$primingeven-> d$oreven

dataEngodd[dataEngodd$morphType=='op',]$primingodd-> d$opodd
dataEngeven[dataEngeven$morphType=='op',]$primingeven-> d$opeven

dataEngodd[dataEngodd$morphType=='tr',]$primingodd-> d$trodd
dataEngeven[dataEngeven$morphType=='tr',]$primingeven-> d$treven

d <- as.data.frame(d)
icc(d,model = "oneway",
    type = "consistency",
    unit = "average",
    r0 = 0,
    conf.level = 0.95)


#raw data
r<-cor(coreven$rt, corodd$rt) 
(2 * r) / (1 + r) #spearman brown formula

#priming
r<-cor(dataEnghalf$primingodd, dataEnghalf$primingeven) #manual method to compute the spearman-brown formula
(2 * r) / (1 + r) #spearman brown formula

r<-cor(dataEnghalf[dataEnghalf$morphType=='or',]$primingodd, dataEnghalf[dataEnghalf$morphType=='or',]$primingeven) 
(2 * r) / (1 + r) 

r<-cor(dataEnghalf[dataEnghalf$morphType=='op',]$primingodd, dataEnghalf[dataEnghalf$morphType=='op',]$primingeven) #manual method to compute the spearman-brown formula
(2 * r) / (1 + r) #spearman brown formula

r<-cor(dataEnghalf[dataEnghalf$morphType=='tr',]$primingodd, dataEnghalf[dataEnghalf$morphType=='tr',]$primingeven) #manual method to compute the spearman-brown formula
(2 * r) / (1 + r) #spearman brown formula

dataEnghalf[dataEnghalf$morphType=='or',]->Reliability;
multicon::splithalf.r(Reliability[,c('primingodd', 'primingeven')], sims = 5000);
multicon::alpha.cov(cov(Reliability[,c('primingodd', 'primingeven')], use="p"));

dataEnghalf[dataEnghalf$morphType=='op',]->Reliability
multicon::splithalf.r(Reliability[,c('primingodd', 'primingeven')], sims = 5000);
multicon::alpha.cov(cov(Reliability[,c('primingodd', 'primingeven')], use="p"));

dataEnghalf[dataEnghalf$morphType=='tr',]->Reliability
multicon::splithalf.r(Reliability[,c('primingodd', 'primingeven')], sims = 5000);
multicon::alpha.cov(cov(Reliability[,c('primingodd', 'primingeven')], use="p"));

psych::splitHalf(Reliability[,c('primingodd', 'primingeven')], 
                 n.sample = 5000, use = 'pairwise');


 #### critical GLMER model comparisons ####
#load all the lmer and GLMM models
df <- list.files(paste(localGitDir, "/LMMs and GLMMs/", sep = "")); 
length(df); #65 models

for (i in 1:length(df)){
  gsub(".rds$", "", df[i]) -> id
  assign(id, data.frame())
  readRDS(paste(localGitDir, "/LMMs and GLMMs/", df[i],sep = "")) -> temp 
  assign(paste0(id), temp)
};

rm(i, id, temp);



#dataITA without trimming
car::Anova(itaglmer2); #dataIta main model - or baseline comparison

car::Anova(italmer2);

car::Anova(itaglmer2c); #dataIta main model - op baseline comparison
car::Anova(italmer2c);

#dataITA with trimming
car::Anova(itaglmer2b); #dataIta main model - or baseline comparison
car::Anova(italmer2b);

car::Anova(itaglmer2d); #dataIta main model - op baseline comparison
car::Anova(italmer2d);

#cross interaction between languages
car::Anova(crossglmer);

#dataENG without trimming
car::Anova(engglmer2); #dataEng main model - or baseline comparison
car::Anova(englmer2);
summary(engglmer2);

l4.emm <- emmeans(engglmer2c, ~ relatedness * morphType )
contrast(l4.emm, "consec",  simple = "each", combine = F, adjust = "bonferroni")


car::Anova(engglmer2c); #dataEng main model - op baseline comparison
car::Anova(englmer2c);

car::Anova(proficiencyglmer1); #fluency - or baseline
car::Anova(proficiencyglmer1d); #fluency - op baseline

l4.emm <- emmeans(proficiencyglmer1d, ~ relatedness * morphType * phonemicFluency)
contrast(l4.emm, "consec",  simple = "each", combine = T, adjust = "bonferroni")
fixef(proficiencyglmer1d)[13:14]
as.data.frame(phonFlu)->phonFlu
mean(phonFlu$V1); mean(phonFlu$V2)
sd(phonFlu$V1); sd(phonFlu$V2)


car::Anova(proficiencyglmer2); #phonemicComprehension - or baseline
car::Anova(proficiencyglmer2d); #phonemicComprehension - op baseline
fixef(proficiencyglmer2d)[13:14]
as.data.frame(phonAware)->phonAware
mean(phonAware$V1); mean(phonAware$V2)
sd(phonAware$V1); sd(phonAware$V2)

car::Anova(proficiencylmer2);

car::Anova(proficiencyglmer3); #morph comprehension - or baseline
car::Anova(proficiencyglmer3d); #morph comprehension - op baseline
fixef(proficiencyglmer3d)[13:14]
as.data.frame(morphAware)->morphAware
mean(morphAware$V1); mean(morphAware$V2)
sd(morphAware$V1); sd(morphAware$V2)


car::Anova(proficiencyglmer4); #spelling - or baseline
car::Anova(proficiencyglmer4d); #spelling - op baseline

car::Anova(proficiencylmer4);

car::Anova(proficiencyglmer5); #readingComprehension - or baseline
car::Anova(proficiencyglmer5d); #readingComprehension - op baseline
fixef(proficiencyglmer5d)[13:14]
as.data.frame(readComp)->readComp
mean(readComp$V1); mean(readComp$V2)
sd(readComp$V1); sd(readComp$V2)

car::Anova(proficiencylmer5);

car::Anova(proficiencyglmer6); #vocabulary - or baseline
car::Anova(proficiencyglmer6d); #vocabulary - op baseline
fixef(proficiencyglmer6d)[13:14]
as.data.frame(voc)->voc
mean(voc$V1); mean(voc$V2)
sd(voc$V1); sd(voc$V2)

car::Anova(proficiencyglmer7); #oralComprehension - or baseline
car::Anova(proficiencyglmer7d); #oralComprehension - op baseline
car::Anova(proficiencylmer7);


car::Anova(crossglmer);

#dataEng with trimming
car::Anova(engglmer2b); #dataEng main model - or baseline comparison
car::Anova(englmer2b);

car::Anova(engglmer2d); #dataEng main model - op baseline comparison
car::Anova(englmer2d);

car::Anova(proficiencyglmer1b); #fluency
car::Anova(proficiencylmer1b);
fixef(proficiencylmer1b)[13:14]


car::Anova(proficiencyglmer2b); #phonemicComprehension
car::Anova(proficiencylmer2b);
summary(proficiencylmer2b)

car::Anova(proficiencyglmer3b); #morph comprehension
car::Anova(proficiencylmer3b);

fixef(proficiencylmer3b)[13:14]
car::Anova(proficiencyglmer4b); #spelling
car::Anova(proficiencylmer4b);

car::Anova(proficiencyglmer5b); #readingComprehension
car::Anova(proficiencylmer5b);

car::Anova(proficiencyglmer6b); #vocabulary
car::Anova(proficiencylmer6b);

car::Anova(proficiencyglmer7b); #oralComprehension
car::Anova(proficiencylmer7b);

#dataEng with age of acquisition

car::Anova(aoaglmer1); # or baseline #proper aoa
car::Anova(aoaglmer1c); # op baseline
anova(proficiencyglmer0, aoaglmer1)

car::Anova(aoaglmer2); # or baseline #daily usage
car::Anova(aoaglmer2c); # op baseline
anova(proficiencyglmer0, aoaglmer2)

car::Anova(aoaglmer3); # or baseline #where did you learn English? Home versus school
car::Anova(aoaglmer3c); # op baseline

car::Anova(aoaglmer5); # or baseline #self-rated proficiency
car::Anova(aoaglmer5c); # op baseline

anova(proficiencyglmer0, aoaglmer5)

saveRDS(osc4, paste(localGitDir, "/LMMs and GLMMs/osc4.rds",sep = ""))

car::Anova(osc1);
car::Anova(osc2);
car::Anova(osc3);
car::Anova(osc4);

library(emmeans)

em <- emmeans(proficiencyglmer2, pairwise ~  relatedness | morphType * phonemicComprehension)
em
