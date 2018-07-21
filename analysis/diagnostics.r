diagnostics.f <- function (rt, acc, sbj.id, target, lexicality, filename)
  
{
  aggregate(rt[acc==1 & lexicality=="word"], list(sbj.id[acc==1 & lexicality=="word"]),mean) -> temp1;     
  names(temp1) <- c("sbj.id", "rt.words");
  aggregate(rt[acc==1 & lexicality=="nonword"], list(sbj.id[acc==1 & lexicality=="nonword"]), mean) -> temp2;     
  names(temp2) <- c("sbj.id", "rt.nonwords");
  aggregate(acc[lexicality=="word"], list(sbj.id[lexicality=="word"]), mean) -> temp3;      
  names(temp3) <- c("sbj.id", "acc.words");
  aggregate(acc[lexicality=="nonword"], list(sbj.id[lexicality=="nonword"]), mean) -> temp4;
  names(temp4) <- c("sbj.id", "acc.nonwords");
  sbj.diagnostics <- merge(temp1,temp2);
  sbj.diagnostics <- merge(sbj.diagnostics,temp3);
  sbj.diagnostics <- merge(sbj.diagnostics,temp4);
  sbj.diagnostics <<- sbj.diagnostics;
  
  aggregate(rt[acc==1 & lexicality=="word"], list(target[acc==1 & lexicality=="word"]), mean) -> temp1;
  names(temp1) <- c("target","rt");
  aggregate(acc[lexicality=="word"], list(target[lexicality=="word"]), mean) -> temp2;
  names(temp2) <- c("target","acc");
  target.diagnostics <<- merge(temp1, temp2);
  
  jpeg(paste(as.character(filename),".jpg",sep=""), res=200, height=1654, width=2339);
  layout(matrix(c(1,2,3,3), nrow=2, byrow=T), heights=c(2,1));	 
  
  ymin <- min(min(sbj.diagnostics$acc.words),min(sbj.diagnostics$acc.nonwords));
  ymax <- max(max(sbj.diagnostics$acc.words),max(sbj.diagnostics$acc.nonwords));
  xmin <- min(min(sbj.diagnostics$rt.words),min(sbj.diagnostics$rt.nonwords));
  xmax <- max(max(sbj.diagnostics$rt.words),max(sbj.diagnostics$rt.nonwords));
  
  sdwords <- vector(mode="numeric", length=nrow(sbj.diagnostics));
  for (i in 1:nrow(sbj.diagnostics)) sdwords[i] <- sd(rt[sbj.id==sbj.diagnostics$sbj.id[i] & acc==1 & lexicality=="word"])/sqrt(length(rt[sbj.id==sbj.diagnostics$sbj.id[i] & acc==1 & lexicality=="word"]));
  
  sdnonwords <- vector(mode="numeric", length=nrow(sbj.diagnostics));
  for (i in 1:nrow(sbj.diagnostics)) sdnonwords[i] <- sd(rt[sbj.id==sbj.diagnostics$sbj.id[i] & acc==1 & lexicality=="nonword"])/sqrt(length(rt[sbj.id==sbj.diagnostics$sbj.id[i] & acc==1 & lexicality=="nonword"]));
  
  sbj.diagnostics$sd.words <<- sdwords;
  sbj.diagnostics$sd.nonwords <<- sdnonwords;
  
  plot(sbj.diagnostics$rt.words, sbj.diagnostics$acc.words, xlab="RT (ms)", ylab="% correct", main="Subjects", type="n", ylim=c(ymin,ymax), xlim=c(xmin,xmax));
  
  symbols(sbj.diagnostics$rt.words, sbj.diagnostics$acc.words, fg="red", add=T, inches=F, circles=sdwords);
  symbols(sbj.diagnostics$rt.nonwords, sbj.diagnostics$acc.nonwords, fg="blue", add=T, inches=F, circles=sdnonwords);
  text(sbj.diagnostics$rt.words, sbj.diagnostics$acc.words, as.character(sbj.diagnostics$sbj.id), col="red");
  text(sbj.diagnostics$rt.nonwords, sbj.diagnostics$acc.nonwords, as.character(sbj.diagnostics$sbj.id), col="blue");
  for (i in 1:nrow(sbj.diagnostics)) lines(c(sbj.diagnostics$rt.words[i], sbj.diagnostics$rt.nonwords[i]), c(sbj.diagnostics$acc.words[i], sbj.diagnostics$acc.nonwords[i]), col=grey(.70));
  
  abline(v=mean(sbj.diagnostics$rt.words)+(2*sd(sbj.diagnostics$rt.words)), col="red", lty=2, lwd=2);
  abline(v=mean(sbj.diagnostics$rt.nonwords)+(2*sd(sbj.diagnostics$rt.nonwords)), col="blue", lty=2, lwd=2);
  abline(h=mean(sbj.diagnostics$acc.words)-(2*sd(sbj.diagnostics$acc.words)), col="red", lty=2, lwd=2);
  abline(h=mean(sbj.diagnostics$acc.nonwords)-(2*sd(sbj.diagnostics$acc.nonwords)), col="blue", lty=2, lwd=2);
  
  plot(target.diagnostics$rt, target.diagnostics$acc, pch=19, xlab="RT (ms)", ylab="% correct", main="Targets", type="n");
  text(target.diagnostics$rt, target.diagnostics$acc, as.character(target.diagnostics$target));
  
  hist(rt[acc==1 & lexicality=="word"], xlab="RT (ms)", ylab="Density", main="Individual datapoints", breaks=(max(rt[acc==1 & lexicality=="word"])-min(rt[acc==1 & lexicality=="word"]))/50);
  
  dev.off();
  
  par(mfrow=c(1,1));
  
};
