#-----------------------------------------
# File name: findr.R
#-----------------------------------------
findr<-function(sample){
  idx.case=grep(TRUE,(sample[,1]==1))
  idx.ctrl=grep(TRUE,(sample[,1]==0))
  
  case=sample[idx.case,]
  ctrl=sample[idx.ctrl,]
  
  b1=tdt.cnt(case)[1]
  c1=tdt.cnt(case)[2]
  r1=b1/(b1+c1)
  
  b2=tdt.cnt(ctrl)[1]
  c2=tdt.cnt(ctrl)[2]
  r2=b2/(b2+c2)
  
  r=c(r1,r2)
  names(r)=c('r1','r2')
  r
}
