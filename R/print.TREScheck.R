
### a print method for the check object

print.TREScheck<-function(x){
  invisible(sapply(seq_along(x),function(i){
    cat("\n","_____________________________________________________________________________","\n")
    cat("\n",paste("Check",formatC(i,width=3,flag=0),names(x)[i],sep=" - "))
    cat("\n","_____________________________________________________________________________","\n","\n")
    res<-x[[i]]
    n<-nrow(res)
    if(is.data.frame(res)){
      cat("Showing first",min(6,n),"lines of",n,"\n","\n","\n")
      print(head(res))
    }else{
      print(head(res))
    }
    
  }))
}