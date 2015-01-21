#-----------------------------------------
# File name: tdt.cnt.R
#-----------------------------------------
tdt.cnt<-function(sample){
  #Mother with genotype 1
  idx.100=length(grep(TRUE, (sample[,2]==1&sample[,3]==0&sample[,4]==0)))
  idx.101=length(grep(TRUE, (sample[,2]==1&sample[,3]==0&sample[,4]==1)))
  idx.102=length(grep(TRUE, (sample[,2]==1&sample[,3]==0&sample[,4]==2)))
  
  idx.110=length(grep(TRUE, (sample[,2]==1&sample[,3]==1&sample[,4]==0)))
  idx.111=length(grep(TRUE, (sample[,2]==1&sample[,3]==1&sample[,4]==1)))
  idx.112=length(grep(TRUE, (sample[,2]==1&sample[,3]==1&sample[,4]==2)))
  
  idx.120=length(grep(TRUE, (sample[,2]==1&sample[,3]==2&sample[,4]==0)))
  idx.121=length(grep(TRUE, (sample[,2]==1&sample[,3]==2&sample[,4]==1)))
  idx.122=length(grep(TRUE, (sample[,2]==1&sample[,3]==2&sample[,4]==2)))
    
  #Father with genotype 1
  idx.010=length(grep(TRUE, (sample[,3]==1&sample[,2]==0&sample[,4]==0)))
  idx.011=length(grep(TRUE, (sample[,3]==1&sample[,2]==0&sample[,4]==1)))
  idx.012=length(grep(TRUE, (sample[,3]==1&sample[,2]==0&sample[,4]==2)))
    
  idx.210=length(grep(TRUE, (sample[,3]==1&sample[,2]==2&sample[,4]==0)))
  idx.211=length(grep(TRUE, (sample[,3]==1&sample[,2]==2&sample[,4]==1)))
  idx.212=length(grep(TRUE, (sample[,3]==1&sample[,2]==2&sample[,4]==2)))
  
  b=idx.101+idx.111+2*idx.112+idx.122+idx.011+idx.212
  c=idx.100+2*idx.110+idx.111+idx.121+idx.010+idx.211
  
  cnt=c(b,c)
  names(cnt)=c('b','c')
  cnt
}

