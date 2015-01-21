#------------------------------------------------
# Filename: find.maf.R
#------------------------------------------------
find.maf<-function(sample){
  
idx.case=grep(TRUE,(sample[,1]==1))
idx.ctrl=grep(TRUE,(sample[,1]==0))
  
case=sample[idx.case,]
ctrl=sample[idx.ctrl,]

# find MAF in whole sample
# including case-trios and control-trios
idx.m.0=length(grep(TRUE,sample[,2]==0))
idx.m.1=length(grep(TRUE,sample[,2]==1))
idx.m.2=length(grep(TRUE,sample[,2]==2))
idx.f.0=length(grep(TRUE,sample[,3]==0))
idx.f.1=length(grep(TRUE,sample[,3]==1))
idx.f.2=length(grep(TRUE,sample[,3]==2))
idx.b.0=length(grep(TRUE,sample[,4]==0))
idx.b.1=length(grep(TRUE,sample[,4]==1))
idx.b.2=length(grep(TRUE,sample[,4]==2))

# population size
psize=sum(idx.m.0,idx.m.1,idx.m.2,
          idx.f.0,idx.f.1,idx.f.2,
          idx.b.0,idx.b.1,idx.b.2)

# MAF (2 alleles per person)
p=((idx.m.1+idx.f.1+idx.b.1)+2*(idx.m.2+idx.f.2+idx.b.2))/(2*psize)

# find MAF in cases
idx.m.0=length(grep(TRUE,case[,2]==0))
idx.m.1=length(grep(TRUE,case[,2]==1))
idx.m.2=length(grep(TRUE,case[,2]==2))
idx.f.0=length(grep(TRUE,case[,3]==0))
idx.f.1=length(grep(TRUE,case[,3]==1))
idx.f.2=length(grep(TRUE,case[,3]==2))
idx.b.0=length(grep(TRUE,case[,4]==0))
idx.b.1=length(grep(TRUE,case[,4]==1))
idx.b.2=length(grep(TRUE,case[,4]==2))

# population size
psize.case=sum(idx.m.0,idx.m.1,idx.m.2,
          idx.f.0,idx.f.1,idx.f.2,
          idx.b.0,idx.b.1,idx.b.2)

# MAF (2 alleles per person)
p.case=((idx.m.1+idx.f.1+idx.b.1)+2*(idx.m.2+idx.f.2+idx.b.2))/(2*psize.case)

# Find MAF in child cases
psize.case.b=sum(idx.b.0,idx.b.1,idx.b.2)
p.case.b=(idx.b.1+2*idx.b.2)/2/psize.case.b

# Find MAF in parent cases
psize.case.p=sum(idx.m.0,idx.m.1,idx.m.2,idx.f.0,idx.f.1,idx.f.2)
p.case.p=(idx.m.1+2*idx.m.2+idx.f.1+2*idx.f.2)/2/psize.case.p
  
# find MAF in controls
idx.m.0=length(grep(TRUE,ctrl[,2]==0))
idx.m.1=length(grep(TRUE,ctrl[,2]==1))
idx.m.2=length(grep(TRUE,ctrl[,2]==2))
idx.f.0=length(grep(TRUE,ctrl[,3]==0))
idx.f.1=length(grep(TRUE,ctrl[,3]==1))
idx.f.2=length(grep(TRUE,ctrl[,3]==2))
idx.b.0=length(grep(TRUE,ctrl[,4]==0))
idx.b.1=length(grep(TRUE,ctrl[,4]==1))
idx.b.2=length(grep(TRUE,ctrl[,4]==2))

# population size
psize.ctrl=sum(idx.m.0,idx.m.1,idx.m.2,
               idx.f.0,idx.f.1,idx.f.2,
               idx.b.0,idx.b.1,idx.b.2)

# MAF (2 alleles per person)
p.ctrl=((idx.m.1+idx.f.1+idx.b.1)+2*(idx.m.2+idx.f.2+idx.b.2))/(2*psize.ctrl)

# Find MAF in child controls
psize.ctrl.b=sum(idx.b.0,idx.b.1,idx.b.2)
p.ctrl.b=(idx.b.1+2*idx.b.2)/2/psize.ctrl.b

# Find MAF in parent controls
psize.ctrl.p=sum(idx.m.0,idx.m.1,idx.m.2,idx.f.0,idx.f.1,idx.f.2)
p.ctrl.p=(idx.m.1+2*idx.m.2+idx.f.1+2*idx.f.2)/2/psize.ctrl.p

# Output
stat=c(p.case.b,p.ctrl.b,p.case.p,p.ctrl.p,p.case,p.ctrl,p)
names(stat)=c('case-child','ctrl-child','case-parent','ctrl-parent',
              'case','ctrl','sample')
stat
}

