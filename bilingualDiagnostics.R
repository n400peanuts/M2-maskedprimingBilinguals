# ---- Diagnostics.Rfunction of Davide Crepaldi (see: http://www.davidecrepaldi.net/wordpress/software-utilities-2/)
# ---- Run this script after the preprocessing. Or in alternative, upload directly the file 'masterFile.txt'
# ---- 13/01/2017

#---------------------------------------------------------------------------------------------------#
#-------------------------------------ITA f.diagnostics---------------------------------------------#
#---------------------------------------------------------------------------------------------------#

#set your own working directory
#upload masterFile from masterFile.txt in the folder 'data'

subset(masterFile, Language=="ita")-> masterfileIta

sbj.id<- masterfileIta$Subject
acc<- masterfileIta$Accuracy
lexicality<- masterfileIta$Lexicality
target<-masterfileIta$Target
rt<-masterfileIta$rt

source("Diagnostics.R") #I've uploaded this function into the git folder, so that everyone can import it into R, and use it. Doesn't seem to work here, however. Perhaps you could try putting your own diagnostics.r file into the git, Eva, it may be that there's a problem in my file
diagnostics.f(rt, acc, sbj.id, target, lexicality, ita)

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
#-------------------------------------ENG f.diagnostics---------------------------------------------#
#---------------------------------------------------------------------------------------------------#
subset(masterFile, Language=="eng")-> masterfileEng


sbj.id<- masterfileEng$Subject
acc<- masterfileEng$Accuracy
lexicality<- masterfileEng$Lexicality
target<-masterfileEng$Target
rt<-masterfileEng$rt

diagnostics.f(rt, acc, sbj.id, target, lexicality, eng)

#Subjs 16 saw the prime. Sbjs 22 and 26 will be taken out because their performance is very far from the others and the accuracy less than 40%, see the output graph from the diagnostics function.
# We filter also Rts from 250 to 1900ms
subset(masterfileEng, masterfileEng$rt>250 & masterfileEng$rt<1900 & masterfileEng$Subject!=16 & masterfileEng$Subject!=22 & masterfileEng$Subject!=26 & masterfileEng$Lexicality=="WORD") -> dataAccENG
#Then, we select only right answers
subset(dataAccENG, dataAccENG$Accuracy==1)-> datartENG
#First look at the means
round(xtabs(datartENG$rt ~ datartENG$Morphtype + datartENG$Primetype) / xtabs(~datartENG$Morphtype + datartENG$Primetype), digits = 0)

#clean up the workspace
rm(rt, acc, sbj.id, target, lexicality, diagnostics.f, masterFile);

