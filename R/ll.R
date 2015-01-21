#-----------------------------------------
# File name: ll.R
#-----------------------------------------
ll<-function(data,data.type,r,model.type){
  if(data.type=='sample'){
    cnt=strata.cnt(data)
  }else if (data.type=='counts'){
    cnt=data
  }
  
  if(model.type=='grr'){
    stat=ll.grr(cnt,r)
  }else if (model.type=='dom'){
    stat=ll.dom(cnt,r)
  }else if (model.type=='gd'){
    stat=ll.gd(cnt,r)
  }
  
  out=out.stats(stat)
}
  