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
rbind(datartENG[,1:43], datartITA) -> crossExp
crossExp$Morphtype<- relevel(crossExp$Morphtype,"OR")
languagelmer1 <- lmer(-1000/rt ~ as.factor(Relatedness) * Morphtype * Language + Logfreq.Zipf.t +rcs(TrialCount) + Lent + (1|Subject) + (1|Target), data = crossExp, REML = F)
languagelmer2 <- lmer(-1000/rt ~ as.factor(Relatedness) * Morphtype * Language + Logfreq.Zipf.t +rcs(TrialCount) + Lent + (1|Subject) + (1|Target), data = subset(crossExp, abs(scale(resid(languagelmer1)))<2.5), REML = F)
summary(languagelmer2)
summary(languagelmer1)
anova(languagelmer2)
#add separate graph for ita and eng
par(mfrow=c(1,2))
plotLMER.fnc(languagelmer2, fun = inv, pred = "as.factor(Relatedness)", control = list("Languageita", 1),intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, main = "ITA", ylab='RT (ms)', ylim=c(515,640))
plotLMER.fnc(languagelmer2, fun = inv, pred = "as.factor(Relatedness)", control = list("Languageita", 0),intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, main= 'ENG', ylab='RT (ms)', ylim=c(515,640))
par(mfrow=c(1,1))

#---------------------------------------------------------------------------------------------------#
#                                     Language proficiency analysis                                 #
#---------------------------------------------------------------------------------------------------#

#first take a look at variables distributions
proficiencyData <- datartENG[,c('Subject','Age','Gender','Handedness','Rotation','phoneticFluency', 'phoneticComprehension','morphComprehension','spelling','readingComprehension','vocabulary','oralComprehension','AoA1', 'AoA2', 'AoA3','AoA4','AoA5','AoA6','AoA7','AoA8','AoA9')];
proficiencyData <- unique(proficiencyData);
summary(proficiencyData)

hist(proficiencyData$phoneticFluency, breaks = seq(0,50,5)) 
hist(proficiencyData$phoneticComprehension, breaks = seq(0,15,1)) #capacit? di discriminazione fonologica
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

#Let's try to sum all the proficiency score in one variable: overallProf
datartENG$overallProf <- apply(datartENG[28:34],1,FUN = sum);
summary(datartENG) #okay, the sum by rows worked!
hist(datartENG$overallProf) #distribution looks normal

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
plotLMER.fnc(languagelmer2, fun = inv, pred = "as.factor(Relatedness)", control = list("Languageita", 1),intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, main = "ITA", ylab='RT (ms)', ylim=c(515,640))



par(mfrow=c(2,2));
plotLMER.fnc(proficiencylmer8b, fun = inv, pred = "Relatedness",control = list("overallProf", quantile(datartENG$overallProf, .15)), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', main='very low prof');
plotLMER.fnc(proficiencylmer8b, fun = inv, pred = "Relatedness",control = list("overallProf", quantile(datartENG$overallProf, .35)), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', main='low prof');
plotLMER.fnc(proficiencylmer8b, fun = inv, pred = "Relatedness",control = list("overallProf", quantile(datartENG$overallProf, .65)), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', main='high prof');
plotLMER.fnc(proficiencylmer8b, fun = inv, pred = "Relatedness",control = list("overallProf", quantile(datartENG$overallProf, .85)), intr = list("Morphtype", c("OR", "OP", "TR"), "end"), addlines = T, ylab='RT(ms)', main='very high prof');
par(mfrow=c(1,1));

#ah ah, bingo here!!!
#allora ci ho pensato un po', dimmi se sono conclusioni un po' affrettate. 
#Se filtriamo i dati secondo "subset(datartENG, abs(scale(resid(proficiencylmer8)))<2)" la 3way interaction salta fuori. 
#E' ammissibile un passaggio del genere? 
#nel caso in cui lo fosse, il pattern che emerge ? che pi? la proficiency cala, pi? si ha priming. Pi? la proficiency aumenta, meno si ha priming.
#Tuttavia, se si guarda il pattern in morphtype, si vede che pi? la proficiency cala, pi? il priming ? presente in tutte e tre le condizioni: nei TR, ma cos? anche negli OR e nei OP, anzi, in questi ultimi due i RTs sono pi? veloci dei TR.
#Invece per chi ha una proficiency alta (valore 112) si trova il classico pattern come in L1: TR e OP hanno RTs veloci, ma l'OR ? lentissimo. Questo pattern ? evidenziato dalla forma a /\ triangolare
#Ma queste speculazioni sono ammissibili solo se accettiamo il subset che rende significativa la relazione tra relatedness*overallProf
#E' cos???




#AoA1 "A che et? hai iniziato ad essere esposto alla lingua inglese?"
proficiencylmer9 <- lmer(-1000/rt ~ Relatedness * AoA1 * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
proficiencylmer10 <- lmer(-1000/rt ~ Relatedness * AoA1  * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer9)))<2))
anova(proficiencylmer0, proficiencylmer9) #relatedness*AoA1 significativo
anova(proficiencylmer9) #anche qui senza filtrare c'? solo l'interazione tra relatedness*AoA1
anova(proficiencylmer10) #qui invece anche la 3way interaction
plotLMER.fnc(proficiencylmer9, fun = inv, pred = "Relatedness",intr = list("AoA1", quantile(datartENG$AoA1), "end"), addlines = T)
#qui il pattern sembra essere interessante, pi? aumenta l'et? in cui si ? esposti all'inglese e meno si ha priming.
plotLMER.fnc(proficiencylmer10, fun = inv, pred = "Morphtype",intr = list("AoA1", quantile(datartENG$AoA1), "end"), addlines = T)
#Anche qui, se ammettiamo il subset, il soggetto 0, che sarebbe un perfetto bilingue, ha il pattern di priming come in L1, mentre gli altri no

#AoA2 "quanto usi l'inglese nella tua vita quotidiana da 1 a 5?" 
proficiencylmer11 <- lmer(-1000/rt ~ Relatedness * AoA2 * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
proficiencylmer11b <- lmer(-1000/rt ~ Relatedness * AoA2 * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = subset(datartENG, abs(scale(resid(proficiencylmer11)))<2))
anova(proficiencylmer0, proficiencylmer11)
anova(proficiencylmer11) #qui significativo morphtype*AoA2 e relatedness*AoA2 separatamente. No 3way interaction.
anova(proficiencylmer11b) #qui scompare morphtype*AoA2, ma compare una 3way tra relatedness:Aoa2:morphtype, perch???
plotLMER.fnc(proficiencylmer11, fun = inv, pred = "Relatedness",intr = list("AoA2", quantile(datartENG$AoA2), "end"), addlines = T)
plotLMER.fnc(proficiencylmer11, fun = inv, pred = "Morphtype",intr = list("AoA2", quantile(datartENG$AoA2), "end"), addlines = T)


#AoA3 "In quale contesto hai iniziato ad essere esposto alla lingua inglese? Casa o scuola?"
proficiencylmer12 <- lmer(-1000/rt ~ Relatedness * AoA3 * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
anova(proficiencylmer0, proficiencylmer12) 
anova(proficiencylmer12)
#Qua non so come procedere perch? la risposta ? una variabile dicotomica, o casa o scuola, in classe 'factor' 


#AoA5 "Sei cresciuta/o in un ambiente dove si parlano pi? lingue? 1: s? 2: no"
proficiencylmer13 <- lmer(-1000/rt ~ Relatedness * AoA5 * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
anova(proficiencylmer0, proficiencylmer13) 
anova(proficiencylmer13) 
#nothing significant here

#AoA6 "Se parli pi? lingue, qual ? la lingua che conosci meglio dopo la tua madrelingua? 1: eng 2: altro"
proficiencylmer14 <- lmer(-1000/rt ~ Relatedness * AoA6 * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
anova(proficiencylmer0, proficiencylmer14) 
anova(proficiencylmer14) 
#anche qui non penso sia da usare come predictor questo AoA perch? dice solo se l'inglese ? la seconda lingua oppure no

#AoA7 "Come valuteresti il livello di conoscenza della tua seconda lingua su una scala da 1 (base) a 5 (avanzato)?"
proficiencylmer15 <- lmer(-1000/rt ~ Relatedness * AoA7 * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
anova(proficiencylmer0, proficiencylmer15) 
anova(proficiencylmer15) #AoA7:Morphtype significativo, non penso sia proprio utile

#AoA8 "Qual ? la terza lingua che conosci meglio dopo la tua madrelingua? 1: eng 2: altro 3: nessun'altra"
proficiencylmer16 <- lmer(-1000/rt ~ Relatedness * AoA8 * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
anova(proficiencylmer0, proficiencylmer16) 
anova(proficiencylmer16) #same as AoA7, nonsense this analysis

#AoA9 "Come valuteresti il livello di conoscenza della tua terza lingua su una scala da 1 (base) a 5 (avanzato)?"
proficiencylmer17 <- lmer(-1000/rt ~ as.factor(Relatedness) * AoA9 * Morphtype + rcs(TrialCount) + Logfreq.Zipf.t + Lent + (1|Subject) + (1|Target), data = datartENG)
anova(proficiencylmer0, proficiencylmer17) 
anova(proficiencylmer17) #nothing significant here

#***********SPIN-OFF
#"how much eng words are similar to ita words? and how the similarity between them drives RTs in eng?"
library(vwr)
#ita subtlex
subtlex<-read.table("C:/Users/Eva Viviani/OneDrive/Documenti/R/subtlex-it.txt", header=T) #see "http://crr.ugent.be/subtlex-it"; Crepaldi, Keuleers, Mandera & Brysbaert, 2013
#eng subtlex
subtlex.uk<-read.table("C:/Users/Eva Viviani/OneDrive/Documenti/SISSA - Experiments/Masked Priming/bilingualism/Stimoli/subtlex-UK/ukrimaneggiato.txt", header=T)
subtlex.uk$length <- nchar(as.character(subtlex.uk$Spelling))  #counting lenght of Spelling words
#bigram freq basata sull'ita per gli stimoli eng, ma solo sui prime.