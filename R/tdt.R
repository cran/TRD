#-----------------------------------------
# File name: tdt.R
#-----------------------------------------
tdt<-function(sample){
  idx.case=grep(TRUE,(sample[,1]==1))
  idx.ctrl=grep(TRUE,(sample[,1]==0))

  case=sample[idx.case,]
  ctrl=sample[idx.ctrl,]
  
  #----------------------------------------
  # Compute diagonal elements of TDT table
  # for case-trios and control-trios, find
  # TDT p-values, and adjusted TDT p-value
  #----------------------------------------  
  b1=tdt.cnt(case)[1]
  c1=tdt.cnt(case)[2]
  t.tdt.case=(b1-c1)^2/(b1+c1)
  p.tdt.case=1-pchisq(t.tdt.case,1)
  
  b2=tdt.cnt(ctrl)[1]
  c2=tdt.cnt(ctrl)[2]
  r2=b2/(b2+c2)
  t.tdt.ctrl=(b2-c2)^2/(b2+c2)
  p.tdt.ctrl=1-pchisq(t.tdt.ctrl,1)
    
  t.tdt.adj=((1-r2)*b1-r2*c1)^2/(r2*(1-r2)*(b1+c1))
  p.tdt.adj=1-pchisq(t.tdt.adj,1)
  
  out=data.frame(rbind(c(t.tdt.case,t.tdt.ctrl,t.tdt.adj),
                       c(p.tdt.case,p.tdt.ctrl,p.tdt.adj)))
  rownames(out)=c("Statistics","p-value")
  names(out)=c("Case","Control","Ajusted-Case")
  out
}

