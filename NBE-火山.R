
rm(list=ls())
dev.off()

data<-read.csv('C1.csv',stringsAsFactors = F)
gene.list<-data$Symbol
fc.list<-data$es
p.value.list<-data$fdr
plot(fc.list,-log10(p.value.list),pch=8,xlab = 'effect size',ylab = '-log10 (p.value)',type = 'n')


for(i in c(1:length(gene.list))){
  if(p.value.list[i]<0.05){
    if(fc.list[i]>0.2){
      point.col<-'#FF6347'
    }else if(fc.list[i]<(-0.2)){
      point.col<-'#1E90FF'
    }else{
      point.col<-'grey'
    }
  }else{
    point.col<-'grey'
  }
  points(fc.list[i],-log10(p.value.list[i]),pch=20,col=point.col,cex=0.5)
}
abline(h=-log10(0.05),lty=2,col='black')
abline(v=c(0.2,-0.2),lty=2,col='black')

