#--------------------------------------------
# File name: out.stats.R
#--------------------------------------------
out.stats<-function(stat){

  len=length(stat[[1]])
        
if (len>4){
  R1.m1=stat[[1]][1,]
  R2.m1=stat[[1]][2,]
  R1.m2=stat[[2]][1,]
  R2.m2=stat[[2]][2,]
  DV.m1=stat[[3]]
  DV.m2=stat[[4]]
  
  sum1=matrix(c(
      exp(R1.m1[1]),exp(R1.m1[1]-1.96*R1.m1[2]),exp(R1.m1[1]+1.96*R1.m1[2]),pchisq(R1.m1[3]^2,1,lower.tail=FALSE),
      exp(R2.m1[1]),exp(R2.m1[1]-1.96*R2.m1[2]),exp(R2.m1[1]+1.96*R2.m1[2]),pchisq(R2.m1[3]^2,1,lower.tail=FALSE),
      pchisq(DV.m1,2,lower.tail=FALSE),
      exp(R1.m2[1]),exp(R1.m2[1]-1.96*R1.m2[2]),exp(R1.m2[1]+1.96*R1.m2[2]),pchisq(R1.m2[3]^2,1,lower.tail=FALSE),
      exp(R2.m2[1]),exp(R2.m2[1]-1.96*R2.m2[2]),exp(R2.m2[1]+1.96*R2.m2[2]),pchisq(R2.m2[3]^2,1,lower.tail=FALSE),
      pchisq(DV.m2,2,lower.tail=FALSE)),
      ncol=9, byrow=TRUE)
    
    sum1=cbind(c(1,2),data.frame(sum1))
    sum1=data.frame(sum1)
    names(sum1)=c("Model",
                  "RR1","LCI","UCI","p-value",
                  "RR2","LCI","UCI","p-value",
                  "LRT p-value")
    
    sum2=matrix(c(
      sum1[1,1], 
      paste(signif(sum1[1,2],4),"(",signif(sum1[1,3],4),",",signif(sum1[1,4],4),")",sep=""),
      signif(sum1[1,5],4),
      paste(signif(sum1[1,6],4),"(",signif(sum1[1,7],4),",",signif(sum1[1,8],4),")",sep=""),
      signif(sum1[1,9],4),
      signif(sum1[1,10],4),
      sum1[2,1], 
      paste(signif(sum1[2,2],4),"(",signif(sum1[2,3],4),",",signif(sum1[2,4],4),")",sep=""),
      signif(sum1[2,5],4),
      paste(signif(sum1[2,6],4),"(",signif(sum1[2,7],4),",",signif(sum1[2,8],4),")",sep=""),
      signif(sum1[2,9],4),
      signif(sum1[2,10],4)),
      byrow=T,ncol=6)
    
    sum2=data.frame(sum2)
    names(sum2)=c("Model",
                  "RR1(95%CI)","p-value",
                  "RR2(95%CI)","p-value",
                  "LRT p-value")
    
  }else{
    R1.m1=stat[[1]]
    R1.m2=stat[[2]]
    DV.m1=stat[[3]]
    DV.m2=stat[[4]]
    
    sum1=matrix(c(
      exp(R1.m1[1]),exp(R1.m1[1]-1.96*R1.m1[2]),exp(R1.m1[1]+1.96*R1.m1[2]),pchisq(R1.m1[3]^2,1,lower.tail=FALSE),
      pchisq(DV.m1,1,lower.tail=FALSE),
      exp(R1.m2[1]),exp(R1.m2[1]-1.96*R1.m2[2]),exp(R1.m2[1]+1.96*R1.m2[2]),pchisq(R1.m2[3]^2,1,lower.tail=FALSE),
      pchisq(DV.m2,1,lower.tail=FALSE)),
      ncol=5, byrow=TRUE)
    
    sum1=cbind(c(1,2),data.frame(sum1))
    sum1=data.frame(sum1)
    names(sum1)=c("Model",
                  "RR1","LCI","UCI","p-value",
                  "LRT p-value")
    
    sum2=matrix(c(
      sum1[1,1], 
      paste(signif(sum1[1,2],4),"(",signif(sum1[1,3],4),",",signif(sum1[1,4],4),")",sep=""),
      signif(sum1[1,5],4),
      signif(sum1[1,6],4),
      sum1[2,1], 
      paste(signif(sum1[2,2],4),"(",signif(sum1[2,3],4),",",signif(sum1[2,4],4),")",sep=""),
      signif(sum1[2,5],4),
      signif(sum1[2,6],4)),
      byrow=T,ncol=4)
    
    sum2=data.frame(sum2)
    names(sum2)=c("Model",
                  "RR(95%CI)","p-value",
                  "LRT p-value")
  }
  
  stat2=list(sum1,sum2)
  names(stat2)=c('sumRaw','sumFormat')
  stat2

}