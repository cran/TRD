#--------------------------------------------
# File name: prev.R
#--------------------------------------------
prev<-function(f0,f1,f2,rmother,rfather,maf){
  r=rmother
  mu1=(maf^4)
  mu2=2*(maf^3*(1-maf))
  mu3=(maf^2*(1-maf)^2)
  mu4=4*(maf^2*(1-maf)^2)
  mu5=2*(maf*(1-maf)^3)
  mu6=((1-maf)^4)
  tau=c(1,r,1-r,r,1-r,1,1,r^2,2*r*(1-r),(1-r)^2,r,1-r,r,1-r,1)
  f=c(f2,f2,f1,f2,f1,f1,f1,f2,f1,f0,f1,f0,f1,f0,f0)
  mu<-c(mu1,rep(mu2,4),rep(mu3,2),rep(mu4,3),rep(mu5,4),mu6)
  d=sum(f*mu*tau)
  d
}
