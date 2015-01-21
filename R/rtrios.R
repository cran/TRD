#--------------------------------------------
# File name: rtrios.R
#--------------------------------------------

rtrios<-function(n,ssize,f0,f1,f2,rmother,rfather,maf){
#--------------------------------------------
# Specification of parameters
#--------------------------------------------
rf=rfather			# TRD ratio father
rm=rmother			# TRD ratio mother

#-----------------------------------------
# Simulation of parents genotype
#-----------------------------------------
gen=rbern(n*4,maf)
gf1=gen[1:n]				    #father allele 1
gf2=gen[(n+1):(2*n)]		#father allele 2
gm1=gen[(2*n+1):(3*n)]	#mother allele 1
gm2=gen[(3*n+1):(4*n)]	#mother allele 2
gf=gf1+gf2
gm=gm1+gm2
gp=rbind(gf1,gf2,gf,gm1,gm2,gm)

#--********-----
# clean disk
#--********-----
rm(gen,gf1,gf2,gf,gm1,gm2,gm)

#-----------------------------------------
# Simulation of transmission index
#-----------------------------------------
tf=rbern(n,rf)	#father
tm=rbern(n,rm)	#mother

#-----------------------------------------
# Simulation of child genotype received
#-----------------------------------------
gc2=tf
gc2[gp[1,]==gp[2,]]<-gp[1,][gp[1,]==gp[2,]]
gc1=tm
gc1[gp[4,]==gp[5,]]<-gp[4,][gp[4,]==gp[5,]]

gc=gc1+gc2
g=rbind(gp,gc1,gc2,gc)

#--********-----
# clean disk
#--********-----
rm(gc1,gc2,gc,gp,tf,tm)

#-----------------------------------------
# Simulation of child phenotype based on penetrance factors
#-----------------------------------------
pheno=rep(0,n)

p0=rbern(n,f0)
p1=rbern(n,f1)
p2=rbern(n,f2)

pheno[g[9,]==0&p0==1]=1
pheno[g[9,]==1&p1==1]=1
pheno[g[9,]==2&p2==1]=1

id=c(1:n)
data=t(rbind(g,pheno))
data=cbind(id,data[,c(10,6,3,9,7,8)])

#--********-----
# clean disk
#--********-----
rm(id,g,p0,p1,p2,pheno)

#-----------------------------------------
# collect case-trios and control-trios
#-----------------------------------------
case=data[data[,2]==1,][1:ssize,2:5]
ctrl=data[data[,2]==0,][1:ssize,2:5]

data=list(case,ctrl)
names(data)=c('case','control')
data

}