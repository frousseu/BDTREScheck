

checkShow<-function(html=FALSE){
  r<-deparse(checkBD)
  g<-setdiff(grep("msg <-",r),grep("\"msg <-",r))
  cc<-data.frame(ID=seq_along(g),Checks=gsub("msg <-|  ","",r[g]),stringsAsFactors=FALSE)
  if(html){
    htmlTable(cc,rnames=FALSE)  
  }else{
    cc
  }
}