#-----------------------------------------
# File name: strata.cnt.R
#-----------------------------------------
strata.cnt<-function(data){

cnt.222=length(grep(TRUE,(data[,2]==2&data[,3]==2&data[,4]==2)))
cnt.212=length(grep(TRUE,(data[,2]==2&data[,3]==1&data[,4]==2)))
cnt.211=length(grep(TRUE,(data[,2]==2&data[,3]==1&data[,4]==1)))
cnt.122=length(grep(TRUE,(data[,2]==1&data[,3]==2&data[,4]==2)))
cnt.121=length(grep(TRUE,(data[,2]==1&data[,3]==2&data[,4]==1)))
cnt.201=length(grep(TRUE,(data[,2]==2&data[,3]==0&data[,4]==1)))
cnt.021=length(grep(TRUE,(data[,2]==0&data[,3]==2&data[,4]==1)))
cnt.112=length(grep(TRUE,(data[,2]==1&data[,3]==1&data[,4]==2)))
cnt.111=length(grep(TRUE,(data[,2]==1&data[,3]==1&data[,4]==1)))
cnt.110=length(grep(TRUE,(data[,2]==1&data[,3]==1&data[,4]==0)))
cnt.101=length(grep(TRUE,(data[,2]==1&data[,3]==0&data[,4]==1)))
cnt.100=length(grep(TRUE,(data[,2]==1&data[,3]==0&data[,4]==0)))
cnt.011=length(grep(TRUE,(data[,2]==0&data[,3]==1&data[,4]==1)))
cnt.010=length(grep(TRUE,(data[,2]==0&data[,3]==1&data[,4]==0)))
cnt.000=length(grep(TRUE,(data[,2]==0&data[,3]==0&data[,4]==0)))

cnt=c(	cnt.222,
		cnt.212,cnt.211,cnt.122,cnt.121,
		cnt.201,cnt.021,
		cnt.112,cnt.111,cnt.110,
		cnt.101,cnt.100,cnt.011,cnt.010,
		cnt.000)
names(cnt)=c('cnt.222',
            'cnt.212','cnt.211','cnt.122','cnt.121',
            'cnt.201','cnt.021',
            'cnt.112','cnt.111','cnt.110',
            'cnt.101','cnt.100','cnt.011','cnt.010',
            'cnt.000')
cnt
}
