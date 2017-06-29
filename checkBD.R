if(!require(readxl))   {install.packages("readxl")}
if(!require(plyr))     {install.packages("plyr")}
if(!require(RCurl))    {install.packages("RCurl")}
if(!require(htmlTable)){install.packages("htmlTable")}

library(readxl)
library(plyr)
library(RCurl)
library(htmlTable)


path <- "./Databases" # Will work if a "Databases" folder exist within the working directory 
#cc <- read.csv("TRESchecks.csv",header=TRUE,stringsAsFactors=FALSE,sep=";") # Already present in the working directory?

#path<-"C:/Users/rouf1703/Documents/UdeS/Consultation/ELefol/Doc/BD 2004-2015"
cc<-read.csv("C:/Users/rouf1703/Documents/UdeS/Consultation/ELefol/GitHub/BDTREScheck/TRESchecks.csv",header=TRUE,stringsAsFactors=FALSE,sep=";")
htmlTable(cc[,c("temp_number","check")],rnames=FALSE)
dsn<-path

checkBD<-function(dsn=".",
           adulte="Adulte2016.xlsx",
           couvee="Couvee2016.xlsx",
           oisillon="oisillons2016.xlsx",
           adulte_old="Adulte_2004_2015.xlsx",
           couvee_old="Couvee_2004-2015.xlsx",
           oisillon_old="Oisillons_2004-2015.xlsx",
           stop=FALSE)
{
  
  
  
}

# adulte = base de données des adultes
# oisillon = base de données des oisillons
# couvee = base de données des couvées
# stop = FALSE arrêter la fonction dès qu'une erreur de base données est trouvée
# skip = un vecteur donnant une list des checks à ignorer

###########################################################################################
###########################################################################################
### This script is intended to cross validate the different data bases for the TRSW project
###########################################################################################
###########################################################################################


#############################################
### internal functions
#############################################

### function for checking if visits are all 2 days appart at the 
vis2days<-function(dat){
  x<-unique(dat[,c("ferme","jjulien")])
  x<-x[order(x$ferme,x$jjulien),]
  vis<-ddply(x,.(ferme),function(i){
    i$vis<-length(unique(i$jjulien%%2))>1
    i
  })$vis
  if(any(vis)){
    res<-sapply(unique(x$ferme[vis]),function(i){
      sort(unique(dat$jjulien[dat$ferme==i]))
    },simplify=FALSE)
  }else{
    res<-NULL
  }  
  res
}

### function for getting out all duplicated lines
dup<-function(x){
  duplicated(x) | duplicated(x,fromLast=TRUE)
}

### function for deleting false rows in excel files
checkNArows<-function(x){
  res<-x[!is.na(x$ferme),]
  res
}

### function for checking if column names are valid in each db
check_names<-function(x){
  bd<-deparse(substitute(x))
  m<-switch(bd,
            adulte=which(is.na(match(adulte_names,names(x)))),
            oisillon=which(is.na(match(oisillon_names,names(x)))),
            couvee=which(is.na(match(couvee_names,names(x))))
  )
  if(any(m)){
    stop(paste0("No matches for following names in ",bd,": ",paste(names(x)[m],collapse=", ")))
  }
}

### function for finding duplicates based on certain columns
check_dup<-function(x,col=NULL){
  if(is.null(col)){
    col<-names(x)
  }
  res<-x[dup(x[,col]),]
  if(nrow(res)>0){
    res<-res[order(apply(res[,col],1,paste0,collapse="")),]
  }else{
    res<-NULL
  }
  res
}

### function for checking for duplicate ids according to chosen columns
check_id_dup<-function(x,col){ # the first element of col is an id and the second and or third the farm and or nuest box
  id<-col[1]
  res<-unique(x[,col])
  ids<-res[,id][dup(res[,id])]
  if(length(ids)){
    res<-x[x[,id]%in%ids,] 
    res<-res[do.call(order,res[col]),]
  }else{
    res<-NULL
  }
  res
}

### function for checking the number of characters in different columns
check_nchar<-function(x){
  obj<-deparse(substitute(x))
  n<-list(ferme=2,nichoir=2,id=4,annee=4,nnich=1,idcouvee=9,prefixe=4,suffixe=5)
  w<-sort(unlist(lapply(seq_along(n),function(i){
    if(is.na(match(names(n)[i],names(x)))){
      warning(paste0("No match for ","\"",names(n)[i],"\""," in ",obj,": column not checked"))
    }else{
      which(nchar(x[,names(n)[i]])!=n[[i]])
    }
  })))
}




###############################################
### Build column types
###############################################

couv_col<-c(rep("text",4),rep("numeric",19),rep("text",6),"text")
adul_col<-c(rep("text",3),"numeric","numeric","text","date","numeric",rep("text",3),"numeric",rep("text",5),rep("numeric",10),rep("text",3))

couvee<-read_excel(file.path(dsn,"Couvee2016.xlsx"),sheet=1,na="NA",col_types=couv_col)
adulte<-read_excel(file.path(dsn,"Adulte2016.xlsx"),sheet="Adultes2016",na="NA",col_types=adul_col)
oisillon<-read_excel(file.path(dsn,"oisillons2016.xlsx"),sheet=1,na="NA") 

couvee_p<-read_excel(file.path(dsn,"Couvee_2004-2015.xlsx"),sheet=1,na="NA")
adulte_p<-read_excel(file.path(dsn,"Adultes_2004-2015.xlsx"),sheet=1,na="NA",col_types=adul_col)
oisillon_p<-read_excel(file.path(dsn,"Oisillons_2004-2015.xls"),sheet=1,na="NA") 


#TRESchecks<-read_excel(file.path(dsn,"oisillons2016.xlsx"),sheet=1,na="NA") 

adulte$heure<-substr(adulte$heure,12,16)


##########################################################################################
### delete empty lines included in the excel file by assuming all entries have a ferme id
##########################################################################################

couvee<-checkNArows(couvee)
adulte<-checkNArows(adulte)
oisillon<-checkNArows(oisillon)
warning("All entries are assumed to have a non-NA \"ferme\" id")


### list of elements that are checked
cond<-getURL("https://raw.githubusercontent.com/frousseu/BDTREScheck/master/TRESchecks.csv") # Ce fichier est sur mon github
cond<-read.csv(text=cond,header=TRUE,stringsAsFactors=FALSE,sep=";")

### build list of results to checks
checks<-vector(mode="list",length=nrow(cond))
names(checks)<-paste0("C",formatC(cond$temp_number,width=2,flag=0))



########################################################################
### C01 # First check for good column names in recent database
########################################################################

#cat(paste(paste0("\"",names(couvee),"\""),collapse=","))

adulte_names<-c("ferme","nichoir","id","annee","nnich","idcouvee","heure","jjulien","prefixe","suffixe","idadult","condition","sexe_morpho","age_morpho","sexe_gen","locus_sexe_gen","couleur","age_exact","laile1","laile2","masse","tarse1","tarse2","trougauche","troudroite","pararectrice","plaqueincu","Cause_recapt","commentaire","observateur")

oisillon_names<-c("ferme","nichoir","id","annee","nnich","idcouvee","heure","jjulien","prefixe","suffixe","idois2","sexe_gen","locus_sexe_gen","condition","numero_oisillon","jour_suivi","envol","masse","9primaires1","9primaires2","tarse1","tarse2","commentaires","manipulateur")

couvee_names<-c("idcouvee","id","ferme","nichoir","annee","codesp","nnich","noeufs","noisnes","noisenvol","noismort","dispa_ois","dispa_oeufs","abandon","pred_pot","dponte","dincub","declomin","declomax","denvomin","denvomax","dabanmin","dabanmax","idF1","idM1","idF2","idF3","idM2","idM3","Commentaires")


check_names(adulte)
check_names(oisillon)
check_names(couvee)



#####################################################
### C0X check if names are consistant across years
#####################################################

res<-list()
res[[1]]<-c(setdiff(names(couvee),names(couvee_p)),setdiff(names(couvee_p),names(couvee)))
res[[2]]<-c(setdiff(names(adulte),names(adulte_p)),setdiff(names(adulte_p),names(adulte)))
res[[3]]<-c(setdiff(names(oisillon),names(oisillon_p)),setdiff(names(oisillon_p),names(oisillon)))
res<-lapply(res,function(i){if(length(i)==0){NULL}else{i}})
names(res)<-c("couvee","adulte","oisillon")

if(any(!sapply(res,is.null))){
  print(res)
  stop("Preceding column names not consistant across old and new databases")
}else{
  res<-NULL
}
checks["C0X"]<-list(res)









######################################
### C02 # Check the number of characters which shoudl always be fixed in the different ids
######################################


w<-check_nchar(adulte)
w<-check_nchar(oisillon)
w<-check_nchar(couvee)

if(any(w)){
  res<-x[w,]
}else{
  res<-NULL
}

checks["C02"]<-list(res)



########################################################################
### C03 # Second check for unique values in columns with few non-numeric values
########################################################################


col_val<-c("ferme","nichoir","id","annee","nnich","jjulien","condition","sexe_morpho","age_morpho","couleur","age_exact","trougauche","troudroite","pararectrice","plaqueincu","Cause_recapt")

# function for checking unique values
check_val<-function(x){
  n<-col_val[!is.na(match(col_val,names(x)))]
  res<-sapply(n,function(i){
    sort(unique(x[,i]),na.last=TRUE)
  })
  res
}

checks["C03"]<-list(adulte=check_val(adulte),couvee=check_val(couvee),oisillon=check_val(oisillon))



################################################
### C05 # Check for females and brood assignment
################################################

x<-merge(couvee[,c("idcouvee","idF1")],adulte[adulte$sexe_morpho=="F",c("idcouvee", "idadult")],by="idcouvee",all.x=TRUE)

###C01
w<-which(is.na(x$idF1) & !is.na(x$idadult))
res<-x[w, ]
msg<-c("Females without matches in the couvee file")
if(nrow(res)){
  checks["c01"]<-list(res)
}

################################################
### C06 # 
################################################

w<-which(x$idF1!=x$idadult)
res<-x[w, ]
msg<-c("Adult females wwrongly assigned to couvee")
if(nrow(res)){
  checks["c02"]<-list(res)
}



################################################
### C07 
################################################

x<-merge(couvee[,c("idcouvee","idM1")],adulte[adulte$sexe_morpho=="F",c("idcouvee", "idadult")],by="idcouvee",all.x=TRUE)

msg<-c("Males without matches in the couvee file")
w<-which(is.na(x$idM1) & !is.na(x$idadult))
res<-x[w, ]
if(nrow(res)){
  checks["c03"]<-list(res)
}

###################################################
### C08
###################################################

msg<-c("Adult males wwrongly assigned to couvee")
w<-which(x$idM1!=x$idadult)
res<-x[w, ]
if(nrow(res)){
  checks["c04"]<-list(res)
}



##########################################################
### Check if the nnich correspond
##########################################################

x<-merge(adulte,couvee[couvee$codesp==1,c("idcouvee","codesp", "abandon", "dponte", "declomin", "declomax", "denvomin", "denvomax", "dabanmin","dabanmax")],by="idcouvee",all.x = TRUE)

###########################################################
### C09
###########################################################

msg<-"Capture date is lower than the laying date and the individual has not been found dead"
w<-which(x$jjulien < x$dponte & x$condition == 1)
res<-x[w, ]
checks[length(checks)+1]<-if(nrow(res)){res}else{list(NULL)}
if(nrow(res)){
  checks["c05"]<-list(res)
}

###################################################################
### C10
###################################################################

#3.2) 
#Check if some lines from the adult DB are not associated with a line that do not 
#correspond to a TRSW in the couvee file
#Not sure what that means!!!
msg<-"Adults in the adult DB that are not in the couvee DB"
w<-which(x$codesp != 1 & x$condition == 1)
res <- x[w,]
if(nrow(res)){
  checks["c06"]<-list(res)
}


###################################################################
###C11
###################################################################

#3.3)
msg<-"Capture date is later than the min or max departure date from the nest"
w<-which((x$jjulien > x$denvomin) | (x$jjulien > x$denvomax))
res<-x[w,]
if(nrow(res)){
  checks["c07"]<-list(res)
}

######################################################################
###C12
######################################################################

#3.3.2) Theses cases should be checked thoroughly as date of abandonment is tracked back to the first day when the eggs were cold.
# e.g. Incubation is declared on day 145. 147 and 149 eggs are cold but female is caught anyway on day 149. On field, we consider that 
# the nest was abandonned on day 151 (if incubation was declared previously and eggs are cold for 3 consecutive visits) and stop 
# following this nest (i.e. adult manipulations) from this date. However, in the data base, dabanmin = 146 and dabanmax = 147. 
# That is, the first day when the eggs were cold during that 3 visits sequence of cold eggs... probably not clear...

msg<-"Capture date is later than the min or max date of nest abandonment"
w<-which(((x$jjulien > x$dabanmin) | (x$jjulien > x$dabanmax)) & x$condition == 1)
res<-x[w, ]
if(nrow(res)){
  checks["c08"]<-list(res)
}


############################################## 
### Is nnich assigned correctly
##############################################

x<-merge(oisillon,couvee[couvee$codesp==1,c("idcouvee","dponte","dincub","declomin","declomax","dabanmin","dabanmax")],by="idcouvee",all.x=TRUE)

###############################################################
### C13
###############################################################

msg<-"Capture date of young is later than the minimal abandonment date if nest was abandoned"
w<-which(x$jjulien > (x$dabanmin + 1))
res <- x[w, ]
if(nrow(res)){
  checks["c09"]<-list(res)
}

###############################################################
### C14
###############################################################

msg<-"Capture date of young is before the laying date"
w<-which(x$jjulien < x$dponte)
res<-x[w,]
if(nrow(res)){
  checks["c10"]<-list(res)
}


###############################################################
### C15
###############################################################

x<-adulte
w<-which((x$sexe_morpho%in%c("F") & !x$age_morpho%in%c("SY","ASY",NA)) | (x$sexe_morpho%in%c("M") & !x$age_morpho%in%c("AHY",NA)) | (x$sexe_morpho%in%c(NA) & !x$age_morpho%in%c(NA)))
if(any(w)){
  res<-x[w,]
}else{
  res<-NULL
}
checks["c11"]<-list(res)


###############################################################
### C16
###############################################################

x<-adulte
w<-which(x$heure<"07:00" | x$heure>"20:00")
if(any(w)){
  res<-x[w,]
}else{
  res<-NULL
}
checks["c12"]<-list(res)


###############################################################
### C17
###############################################################

x<-adulte
w<-which(!x$couleur%in%c("B","V","BV","BR",NA))
if(any(w)){
  res<-x[w,]
}else{
  res<-NULL
}
checks["c13"]<-list(res)

###############################################################
### C18
###############################################################

x<-adulte
val<-c(104,127)
w<-which(x$laile<val[1] | x$laile2<val[1] | x$laile>val[2] | x$laile2>val[2])
if(any(w)){
  res<-x[w,]
}else{
  res<-NULL
}
checks["c14"]<-list(res)


###############################################################
### C19
###############################################################

x<-adulte
val<-c(10,14)
w<-which(x$masse<val[1] | x$masse>val[2])
if(any(w)){
  res<-x[w,]
}else{
  res<-NULL
}
checks["c15"]<-list(res)


###############################################################
### C20
###############################################################

x<-adulte
val<-c(10,14)
w<-which(x$tarse1<val[1] | x$tarse2<val[1] | x$tarse1>val[2] | x$tarse2>val[2])
if(any(w)){
  res<-x[w,]
}else{
  res<-NULL
}
checks["c16"]<-list(res)


###############################################################
### C21
###############################################################

x<-adulte
val<-c(10,14)
w<-which(x$sexe_morpho%in%c("M") & !x$plaqueincu%in%c(0,NA))
if(any(w)){
  res<-x[w,]
}else{
  res<-NULL
}
checks["c17"]<-list(res)


##########################################################
### C22
##########################################################


###############################################################
### C23
###############################################################

checks["C23"]<-list(vis2days(adulte))


###############################################################
### C24
###############################################################

checks["C24"]<-list(vis2days(oisillon))





###############################################################
### C25
###############################################################

adulte[grep(" ",adulte$ferme),]



###############################################################
### C26 # doublons globaux
###############################################################


checks["C26"]<-list(
  adulte=check_dup(adulte),
  couvee=check_dup(couvee),
  oisillon=check_dup(oisillon)
)


###############################################################
### C27
###############################################################


checks["C27"]<-list(
  adulte=check_dup(adulte,col=c("idadult","jjulien")),
  oisillon=check_dup(oisillon,col=c("idois2","jjulien"))
)



###############################################################
### C28
###############################################################

checks["C28"]<-list(
  adulte=check_id_dup(adulte,col=c("idadult","ferme")),
  oisillon=check_id_dup(adulte,col=c("idois2","ferme"))
)


###############################################################
### C29
###############################################################

checks["C29"]<-list(
  adulte=check_id_dup(adulte,col=c("idadult","ferme","nichoir")),
  oisillon=check_id_dup(adulte,col=c("idois2","ferme","nichoir"))
)



###############################################################
### C30
###############################################################

adm_cond<-c("vivant","disparu","mort")
w<-which(!oisillon$condition%in%adm_cond)
if(any(w)){
  res<-oisillon[w,]
}else{
  res<-NULL
}
checks["C30"]<-list(res)


###############################################################
### C31
###############################################################

w<-which(oisillon$condition%in%c("disparu","mort") & oisillon$envol==1)
if(any(w)){
  res<-oisillon[w,]
}else{
  res<-NULL
}
checks["C31"]<-list(res)


###############################################################
### C32
###############################################################

w<-which(oisillon$condition%in%c("vivant") & oisillon$envol==0)
ids<-unique(oisillon$idois2[w])
if(any(w)){
  x<-oisillon[oisillon$idois2%in%ids,]
  x<-x[order(x$idois2,x$jjulien),]
  x<-ddply(x,.(idois2),function(i){!tail(i$condition,1)%in%c("mort","disparu")})
  ids2<-x$idois2[x$V1]
  res<-oisillon[oisillon$idois2%in%ids2,]
  res<-res[order(res$idois2,res$jjulien),]
}else{
  res<-NULL
}
checks["cxxx"]<-list(res)


###############################################################
### C33
###############################################################

x<-oisillon[order(oisillon$idois2,oisillon$jjulien,oisillon$heure),]
res<-ddply(x,.(idois2),function(i){
  w1<-which(i$condition%in%c("mort","disparu"))
  if(any(w1)){
    w2<-which(i$condition%in%c("vivant"))
    if(any(w2)){
      res<-any(w2>min(w1))
    }else{
      res<-FALSE
    }
  }else{
    res<-FALSE
  }
})
ids<-res$idois2[res$V1]
if(length(ids)){
  res<-oisillon[oisillon$idois2%in%ids,]
  res<-res[order(res$idois2,res$jjulien,res$heure),]
}else{
  res<-NULL
}
checks["cxxx"]<-list(res)


###############################################################
### C34
###############################################################

### find chicks for which id is not the band number despite having been followed after their 12e days
x<-oisillon
x$idois<-paste0(x$ferme,x$nichoir,x$annee,x$nnich,x$numero_oisillon)
x<-ddply(x,.(idois),function(i){
     sup<-any(which(i$jour_suivi>=12))
     if(sup){
       res<-all(i$idois2==paste0(i$prefixe,i$suffixe))
     }else{
       res<-all(i$idois2==i$i$idois)
     }
     res 
})
ids<-x$idois[!x$V1]
if(length(ids)){
  res<-oisillon[oisillon$idois2%in%ids,] 
  res<-res[order(res$idois2,res$jjulien,res$heure),]
}else{
  res<-NULL
}
checks["C34"]<-list(res)


###############################################################
### C35
###############################################################

x<-oisillon
x$idois<-paste0(x$ferme,x$nichoir,x$annee,x$nnich,x$numero_oisillon)
w<-which(!is.na(x$prefixe) & !is.na(x$suffixe) & x$idois2!=paste0(x$prefixe,x$suffixe))
ids<-x$idois2[w]
if(length(ids)){
  res<-oisillon[oisillon$idois2%in%ids,] 
  res<-res[order(res$idois2,res$jjulien,res$heure),]
}else{
  res<-NULL
}
checks["C35"]<-list(res)


###############################################################
### C36
###############################################################

temp<-setdiff(oisillon$idcouvee,couvee$idcouvee)
if(length(temp)>0){
  res<-temp
}else{
  res<-NULL
}
checks["C36"]<-list(res)


###############################################################
### C37
###############################################################

temp<-setdiff(couvee$idcouvee,oisillon$idcouvee)
if(length(temp)>0){
  res<-temp
}else{
  res<-NULL
}
checks["C37"]<-list(res)




##########################################################
### Summarize brood information
##########################################################

y<-ddply(oisillon,.(idcouvee),function(i){
  #browser()
  Nois<-length(unique(i$numero_oisillon))
  Nenvol<-length(unique(i$numero_oisillon[i$envol==1]))
  Ndead<-length(unique(i$numero_oisillon[i$condition%in%"mort"]))
  Ndispa<-length(unique(i$numero_oisillon[i$condition%in%"disparu"]))
  ans<-data.frame(idcouvee=i$idcouvee[1],Nois,Nenvol,Ndead,Ndispa)
  ans
    
})



### hatching detected in brood but no nestlings in chicks
# check possible erreur dans le script original avec le min et la max de declo
x<-couvee
w<-which(is.na(x$declomin) | is.na(x$declomax) & !is.na(y$Nois)) #if this does not output intger(0), Needs to be cheked
res<-x[w, ]




invisible(sapply(names(checks),function(i){
  cat("\n","\n",paste(i,toupper(cond[[i]]),sep=" - "),"\n","\n","\n")
  print(checks[[i]])
}))





################################
################################



## Summarise nestling informations ==> idcouvee, number of nestlings, number of fledglings, number of dead nestlings, number of nestling dispareared

SumOis <- data.frame(idcouvee = character(0), Nois = numeric(0), Nenvol = numeric(0),  NDead = numeric(0) , NDispa = numeric(0))

z <- unique(oisillon$idcouvee)

for(i in z){
  
  # i = z[1]
  
  id <- subset(oisillon, idcouvee == i)
  
  Nois <- length(unique(id$numero_oisillon))
  
  Ndead <- 0
  Nenvol <- 0
  Ndispa <- 0
  z2 <- unique(id$numero_oisillon)
  
  for(j in z2){
    # j = z2[2]
    id2 <- subset(id, numero_oisillon == j)
    ifelse(sum(id2$envol) == 0, Nenvol <- Nenvol, Nenvol <- Nenvol + 1)
    
    ifelse("mort" %in% id2$condition, Ndead <- Ndead + 1, Ndead <- Ndead)
    
    ifelse("disparu" %in% id2$condition,  Ndispa <- Ndispa + 1, NDispa <- Ndispa)
  }
  
  NewLine <- data.frame(i, Nois, Nenvol, Ndead, Ndispa)
  colnames(NewLine) <- names(SumOis)
  
  SumOis <- rbind(SumOis, NewLine)  
  
}
rm(id, id2, NewLine, i, j, Nenvol, Nois, z, z2, Ndead, Ndispa)

couveeOis <- merge(x = couvee, y = SumOis, by = "idcouvee", all.x = TRUE)
couveeOis <-  couveeOis[!is.na(couveeOis$idcouvee), ]

couveeOis$OisEqual <- as.numeric(couveeOis$noisnes == couveeOis$Nois) 
couveeOis$EnvolEqual <- as.numeric(couveeOis$noisenvol == couveeOis$Nenvol)

Errors4 <- couveeOis[0,] ### JE NE COMPREND PAS CETTE PARTIE ET L'UTILISATION DES ERRORS4 et 5
Errors4$ErrorType <- character(0)

# #####################
Errors5 <- couveeOis[0,]
Errors5$ErrorType <- character(0)


#Check 5.1: hatching was detected in couvee but no nestlings in oisillons
which(is.na(couveeOis$declomin) & !is.na(couveeOis$Nois)) #if this does not output intger(0), Needs to be cheked
check5.1 <- couveeOis[which(is.na(couveeOis$declomin) & !is.na(couveeOis$Nois)), ]
check5.1$ErrorType <- "5.1"
Errors5 <- rbind(Errors5, check5.1)
rm(check5.1)

# check 5.1.2: hatching was detected in couvee but no nestlings in oisillons
which(is.na(couveeOis$declomax) & !is.na(couveeOis$Nois)) #if this does not output intger(0), Needs to be cheked
check5.1.2 <- couveeOis[which(is.na(couveeOis$declomin) & !is.na(couveeOis$Nois)), ]
check5.1.2$ErrorType <- "5.1.2"
Errors5 <- rbind(Errors5, check5.1.2)
rm(check5.1.2)


#check 5.2: number of noines in couvee != number of nestlings in oisillons
check5.2 <- couveeOis[couveeOis$OisEqual == 0 & !is.na(couveeOis$OisEqual), ]
check5.2$ErrorType <- "5.2"
Errors5 <- rbind(Errors5, check5.2)
rm(check5.2)

#check 5.3: number of noienvol in couvee != number of fledlings in oisillons
check5.3 <- couveeOis[couveeOis$EnvolEqual == 0 & !is.na(couveeOis$EnvolEqual), ]
check5.3$ErrorType <- "5.3"
Errors5 <- rbind(Errors5, check5.3)
rm(check5.3)

write.csv(Errors5, "Errors5.csv", row.names = FALSE, quote = TRUE)


plot(eff.pres)
trellis.focus("panel", 1, 1)
panel.points(50, 50)
panel.xyplot(50, 50)
trellis.unfocus()

xyplot(1:10,1:10)












