#--------------------------------------------
# File name: gcount.R
#--------------------------------------------
gcount<-function(sample){
  idx.case=grep(TRUE,(sample[,1]==1))
  idx.ctrl=grep(TRUE,(sample[,1]==0))
  
  case=sample[idx.case,]
  ctrl=sample[idx.ctrl,]
  
  case.g2=length(grep(TRUE, case[,4]==2))
  case.g1=length(grep(TRUE, case[,4]==1))
  case.g0=length(grep(TRUE, case[,4]==0))
  ctrl.g2=length(grep(TRUE, ctrl[,4]==2))
  ctrl.g1=length(grep(TRUE, ctrl[,4]==1))
  ctrl.g0=length(grep(TRUE, ctrl[,4]==0))
  
  stat= list( 
    c(case.g2,case.g1,case.g0),
    c(ctrl.g2,ctrl.g1,ctrl.g0)
  )
  names(stat)=c('case','control')
  names(stat$case)=c('g2','g1','g0')
  names(stat$control)=c('g2','g1','g0')
  stat
  
}

