WordsNgramFreq <- function(Stimuli, Words, Freqs, NgramSize, TypeToken="token", WriteOut=F, OutputType="mean")
{
# Checks the input given by the user
if (length(Words)!=length(Freqs)) stop("The word and frequency vectors must be the same length");
if (NgramSize!=2 & NgramSize!=3) stop("The script is currently working only on bigrams and trigrams");
if (TypeToken!="type" & TypeToken!="token") stop("TypeToken can only take the value 'type' or the value 'token'");
if (OutputType!="mean" & OutputType!="IndividualNgrams") stop("OutputType can only take the value 'mean' or the value 'IndividualNgrams'");
  
# Define the object that will collect the frequency values
  if (OutputType=="mean")
  out <- vector(length=length(Stimuli), mode="numeric")
  else
  out <- list();  

# Convert the input vector in a character vector
Stimuli <- as.character(Stimuli);

for (i in 1:length(Stimuli)) {
			      # Check stimulus length
			      if (nchar(Stimuli[i])<2) out[i]<-NA
			      
			      else
			      {

			      # Define the set of relevant Ngrams
			      LettersInWord <- strsplit(Stimuli[i], "")[[1]];

			      if (NgramSize==2) {
						 ngrams <- vector(length = nchar(Stimuli[i])-1, mode="character");
						 j <- 1;
						 while (j < nchar(Stimuli[i])) {ngrams[j] <- paste(LettersInWord[j], LettersInWord[j+1], sep=""); j <- j+1};
						}

						else

						{
						 ngrams <- vector(length = nchar(Stimuli[i])-2, mode="character");
						 j <- 1;
						 while (j < nchar(Stimuli[i])-1) {ngrams[j] <- paste(LettersInWord[j], LettersInWord[j+1], LettersInWord[j+2], sep=""); j <- j+1};
						};

			      # Calculate log-frequencies for each Ngram
			      freqs <- vector(length = length(ngrams), mode="integer");

                              for (j in 1:length(ngrams)) {
			     				   match <- grep(ngrams[j], Words);
							   if (TypeToken == "type") freqs[j] <- log(length(match)+1, base=10) else freqs[j] <- sum(log(Freqs[match]+1, base=10));
							  };

			      # Take the mean of the log-frequencies and write the output
            if (OutputType=="mean")
			      out[i] <- round(mean(freqs), digits=2)
            else
            out[[i]] <- freqs;

			      barplot(i, xlab="", ylab="Number of words processed", ylim=c(0, length(Stimuli)));
			   
			      };
			      };

# Write results outside R, if the user desires so
if (WriteOut==T) {
		  temp <- data.frame(Words=Stimuli, MeanNgramFreq=out);
		  write.table(temp, file=paste("Word_Mean", as.character(NgramSize),"gramFreq_", as.character(TypeToken), ".txt", sep=""), append=F);
		 }

		 else

		 return(out);
};
 
			  
