#if(!require(readxl))   {install.packages("readxl")}
#if(!require(plyr))     {install.packages("plyr")}
#if(!require(RCurl))    {install.packages("RCurl")}
#if(!require(htmlTable)){install.packages("htmlTable")}

library(readxl)
library(plyr)
library(RCurl)
library(htmlTable)

### produces html table to put in readme
#r<-readLines(file.path(getwd(),"checkBD.R"))
#g<-setdiff(grep("msg<-",r),grep("\"msg<-",r))
#cc<-data.frame(ID=seq_along(g),Checks=gsub("msg<-","",r[g]),stringsAsFactors=FALSE)
#htmlTable(cc,rnames=FALSE)

path<-"C:/Users/rouf1703/Documents/UdeS/Consultation/ELefol/Doc/BD 2004-2015"

dsn<-path

checkBD<-function(dsn=".",
           adults="Adulte2016.xlsx",
           broods="Couvee2016.xlsx",
           chicks="oisillons2016.xlsx",
           adults_old="Adulte_2004_2015.xlsx",
           broods_old="Couvee_2004-2015.xlsx",
           chicks_old="Oisillons_2004-2015.xlsx",
           sheet=1,
           stop=FALSE)
{
  

# adults = base de données des adultes
# chicks = base de données des oisillons
# broods = base de données des couvées
# adults_old = base de données des adultes (vieille)
# chicks_old = base de données des oisillons (vieille)
# broods_old = base de données des couvées (vieille)
# stop = FALSE arrêter la fonction dès qu'une erreur de base données est trouvée
# skip = un vecteur donnant une list des checks à ignorer
# sheet = sheet is assumed to be first one  

###########################################################################################
###########################################################################################
### This script is intended to cross validate the different data bases for the TRSW project
###########################################################################################
###########################################################################################


#############################################
### internal functions
#############################################

### function to append a list with a check and a message
# x = initial list
# a = thing to append
# msg = the name of the check that will be used as a name
lappend<-function(x,a,msg){
  if(!is.list(a) || is.data.frame(a)){
    a<-list(a)  
  }
  ans<-c(x,a)
  names(ans)[length(ans)]<-msg
  ans
}


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

### function for deleting false empty rows in excel files
checkNArows<-function(x){
  k<-!is.na(x$ferme)
  if(!all(k)){
    warning(paste("Removing",sum(!k),"rows with NA \"ferme\" id"))
    x<-x[k,]
  }
  x
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
  w
}




###############################################
### Build column types
###############################################

couv_col<-c(rep("text",4),rep("numeric",19),rep("text",6),"text")
adul_col<-c(rep("text",3),"numeric","numeric","text","date","numeric",rep("text",3),"numeric",rep("text",5),rep("numeric",10),rep("text",3))

couvee<-as.data.frame(read_excel(file.path(dsn,broods),sheet=1,na="NA",col_types=couv_col))
adulte<-as.data.frame(read_excel(file.path(dsn,adults),sheet="Adultes2016",na="NA",col_types=adul_col))
oisillon<-as.data.frame(read_excel(file.path(dsn,chicks),sheet=1,na="NA")) 

couvee_p<-as.data.frame(read_excel(file.path(dsn,broods_old),sheet=1,na="NA"))
adulte_p<-as.data.frame(read_excel(file.path(dsn,adults_old),sheet=1,na="NA",col_types=adul_col))
oisillon_p<-as.data.frame(read_excel(file.path(dsn,chicks_old),sheet=1,na="NA")) 


### make certain changes to columns and column names
adulte$heure<-substr(adulte$heure,12,16)

### temporarily change the names for the code to run !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
names(oisillon)[which(names(oisillon)=="idois2")]<-"idois"
names(adulte_p)[which(names(adulte_p)=="laile")]<-"laile1"
names(adulte_p)[which(names(adulte_p)=="sufixe")]<-"suffixe"


### build list of results to checks
checks<-list()


##########################################################################################
### delete empty lines included in the excel file by assuming all entries have a ferme id
##########################################################################################

msg<-"Remove rows with NA id's in couvee"
ini<-nrow(couvee)
couvee<-checkNArows(couvee)
res<-paste("Removed",ini-nrow(couvee),"rows with NA ferme id's")
checks<-lappend(checks,res,msg)

msg<-"Remove rows with NA id's in adulte"
ini<-nrow(adulte)
adulte<-checkNArows(adulte)
res<-paste("Removed",ini-nrow(adulte),"rows with NA ferme id's")
checks<-lappend(checks,res,msg)

msg<-"Remove rows with NA id's in oisillon"
ini<-nrow(oisillon)
oisillon<-checkNArows(oisillon)
res<-paste("Removed",ini-nrow(oisillon),"rows with NA ferme id's")
checks<-lappend(checks,res,msg)

#warning("All entries are assumed to have a non-NA \"ferme\" id")


########################################################################
### First check for good column names in recent database
########################################################################

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

### column names
adulte_names<-c("ferme","nichoir","id","annee","nnich","idcouvee","heure","jjulien","prefixe","suffixe","idadult","condition","sexe_morpho","age_morpho","sexe_gen","locus_sexe_gen","couleur","age_exact","laile1","laile2","masse","tarse1","tarse2","trougauche","troudroite","pararectrice","plaqueincu","Cause_recapt","commentaire","observateur")

oisillon_names<-c("ferme","nichoir","id","annee","nnich","idcouvee","heure","jjulien","prefixe","suffixe","idois","sexe_gen","locus_sexe_gen","condition","numero_oisillon","jour_suivi","envol","masse","9primaires1","9primaires2","tarse1","tarse2","commentaires","manipulateur")

couvee_names<-c("idcouvee","id","ferme","nichoir","annee","codesp","nnich","noeufs","noisnes","noisenvol","noismort","dispa_ois","dispa_oeufs","abandon","pred_pot","dponte","dincub","declomin","declomax","denvomin","denvomax","dabanmin","dabanmax","idF1","idM1","idF2","idF3","idM2","idM3","Commentaires")


msg<-"Are column names in adult consistent?"
check_names(adulte)
checks<-lappend(checks,NULL,msg)

msg<-"Are column names in oisillon consistent?"
check_names(oisillon)
checks<-lappend(checks,NULL,msg)

msg<-"Are column names in couvee consistent?"
check_names(couvee)
checks<-lappend(checks,NULL,msg)





#####################################################
### C0X check if names are consistant across years
#####################################################

msg<-"Are column names consistant across old and new databases"

res<-list()
res[[1]]<-c(setdiff(names(couvee),names(couvee_p)),setdiff(names(couvee_p),names(couvee)))
res[[2]]<-c(setdiff(names(adulte),names(adulte_p)),setdiff(names(adulte_p),names(adulte)))
res[[3]]<-c(setdiff(names(oisillon),names(oisillon_p)),setdiff(names(oisillon_p),names(oisillon)))
res<-lapply(res,function(i){if(length(i)==0){NULL}else{i}})
names(res)<-c("couvee","adulte","oisillon")

if(any(!sapply(res,is.null))){
  print(res)
  stop("Preceding column names not consistent across old and new databases")
}

checks<-lappend(checks,NULL,msg)




##########################################################################################
### Check the number of characters which shoudl always be fixed in the different ids
##########################################################################################

msg<-"Are number of characters consistent for id-type columns in adulte db?"
w<-check_nchar(adulte)
res<-x[w,]
checks<-lappend(checks,res,msg)

msg<-"Are number of characters consistent for id-type columns in oisillon db?"
w<-check_nchar(oisillon)
res<-x[w,]
checks<-lappend(checks,res,msg)

msg<-"Are number of characters consistent for id-type columns in couvee db?"
w<-check_nchar(couvee)
res<-x[w,]
checks<-lappend(checks,res,msg)


########################################################################
### C03 # Second check for unique values in columns with few non-numeric values
########################################################################

msg<-"Show all unique values in columns for which the number of possible values is restricted"

col_val<-c("ferme","nichoir","id","annee","nnich","jjulien","condition","sexe_morpho","age_morpho","couleur","age_exact","trougauche","troudroite","pararectrice","plaqueincu","Cause_recapt")

# function for checking unique values
check_val<-function(x){
  n<-col_val[!is.na(match(col_val,names(x)))]
  res<-sapply(n,function(i){
    sort(unique(x[,i]),na.last=TRUE)
  })
  res
}

checks<-lappend(checks,list(adulte=check_val(adulte),couvee=check_val(couvee),oisillon=check_val(oisillon)),msg)



################################################
### C05 # Check for females and brood assignment
################################################

msg<-"Females without matches in the couvee file"

x<-merge(couvee[,c("idcouvee","idF1")],adulte[adulte$sexe_morpho=="F",c("idcouvee", "idadult")],by="idcouvee",all.x=TRUE)

###C01
w<-which(is.na(x$idF1) & !is.na(x$idadult))
res<-x[w, ]
checks<-lappend(checks,res,msg)


################################################
### C06 # 
################################################

msg<-"Adult females wrongly assigned to couvee"

w<-which(x$idF1!=x$idadult)
res<-x[w, ]
checks<-lappend(checks,res,msg)




################################################
### C07 
################################################

msg<-"Males without matches in the couvee file"

x<-merge(couvee[,c("idcouvee","idM1")],adulte[adulte$sexe_morpho=="F",c("idcouvee", "idadult")],by="idcouvee",all.x=TRUE)
w<-which(is.na(x$idM1) & !is.na(x$idadult))
res<-x[w, ]
checks<-lappend(checks,res,msg)


###################################################
### C08
###################################################

msg<-c("Adult males wrongly assigned to couvee")

w<-which(x$idM1!=x$idadult)
res<-x[w, ]
checks<-lappend(checks,res,msg)




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
checks<-lappend(checks,res,msg)


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
checks<-lappend(checks,res,msg)



###################################################################
###C11
###################################################################

#3.3)

msg<-"Capture date is later than the min or max departure date from the nest"

w<-which((x$jjulien > x$denvomin) | (x$jjulien > x$denvomax))
res<-x[w,]
checks<-lappend(checks,res,msg)


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
checks<-lappend(checks,res,msg)



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
checks<-lappend(checks,res,msg)


###############################################################
### C14
###############################################################

msg<-"Capture date of young is before the laying date"

w<-which(x$jjulien < x$dponte)
res<-x[w,]
checks<-lappend(checks,res,msg)



###############################################################
### C15
###############################################################

msg<-"Sex/age incoherencies"

x<-adulte
w<-which((x$sexe_morpho%in%c("F") & !x$age_morpho%in%c("SY","ASY",NA)) | (x$sexe_morpho%in%c("M") & !x$age_morpho%in%c("AHY",NA)) | (x$sexe_morpho%in%c(NA) & !x$age_morpho%in%c(NA)))
res<-x[w,]
checks<-lappend(checks,res,msg)


###############################################################
### C16
###############################################################

mmh<-c("07:00","20:00")
msg<-paste("Capture time outside",mmh[1],"and",mmh[2])

x<-adulte
w<-which(x$heure<mmh[1] | x$heure>mmh[2])
res<-x[w,]
checks<-lappend(checks,res,msg)



###############################################################
### C17
###############################################################

msg<-"Some colors not in the list of possible values"

x<-adulte
w<-which(!x$couleur%in%c("B","V","BV","BR",NA))
res<-x[w,]
checks<-lappend(checks,res,msg)

###############################################################
### C18
###############################################################

msg<-"Wing measurement outside the range of likely values"

x<-adulte
val<-c(104,127)
w<-which(x$laile<val[1] | x$laile2<val[1] | x$laile>val[2] | x$laile2>val[2])
res<-x[w,]
checks<-lappend(checks,res,msg)


###############################################################
### C19
###############################################################

msg<-"Weight measurement outside the range of likely values"

x<-adulte
val<-c(10,14)
w<-which(x$masse<val[1] | x$masse>val[2])
res<-x[w,]
checks<-lappend(checks,res,msg)


###############################################################
### C20
###############################################################

msg<-"Tarsus measurement outside the range of likely values"

x<-adulte
val<-c(10,14)
w<-which(x$tarse1<val[1] | x$tarse2<val[1] | x$tarse1>val[2] | x$tarse2>val[2])
res<-x[w,]
checks<-lappend(checks,res,msg)


###############################################################
### C21
###############################################################

msg<-"Male with brood patch"

x<-adulte
val<-c(10,14)
w<-which(x$sexe_morpho%in%c("M") & !x$plaqueincu%in%c(0,NA))
res<-x[w,]
checks<-lappend(checks,res,msg)


##########################################################
### C22
##########################################################

msg<-"Newly installed band found in the previous years"

###############################################################
### C23
###############################################################

msg<-"Visits are not all 2 days apart for the following farms in the adult DB
"

checks<-lappend(checks,list(vis2days(adulte)),msg)



###############################################################
### C24
###############################################################

msg<-"Visits are not all 2 days apart for the following farms in the oisillon DB"

checks<-lappend(checks,list(vis2days(oisillon)),msg)





###############################################################
### C25
###############################################################

msg<-"Check for spaces in ferme ids"

x<-adulte
res<-x[grep(" ",x$ferme),]
checks<-lappend(checks,res,msg)



###############################################################
### C26 # doublons globaux
###############################################################

msg<-"Check for duplicates using all columns in each database"

checks<-lappend(checks,list(
  adulte=check_dup(adulte),
  couvee=check_dup(couvee),
  oisillon=check_dup(oisillon)
),msg)


###############################################################
### C27
###############################################################

msg<-"Check for adults or chicks with more than one entry for a single date"

checks<-lappend(checks,list(
  adulte=check_dup(adulte,col=c("idadult","jjulien")),
  oisillon=check_dup(oisillon,col=c("idois","jjulien"))
),msg)



###############################################################
### C28
###############################################################

msg<-"Check for adults or chicks found at more than one farm"

checks<-lappend(checks,list(
  adulte=check_id_dup(adulte,col=c("idadult","ferme")),
  oisillon=check_id_dup(oisillon,col=c("idois","ferme"))
),msg)


###############################################################
### C29
###############################################################

msg<-"Check for adults or chicks found at more than one nestbox"

checks<-lappend(checks,list(
  adulte=check_id_dup(adulte,col=c("idadult","ferme","nichoir")),
  oisillon=check_id_dup(oisillon,col=c("idois","ferme","nichoir"))
),msg)



###############################################################
### C30
###############################################################

msg<-"Make sure that chick conditions are from 3 possible values"

adm_cond<-c("vivant","disparu","mort")
w<-which(!oisillon$condition%in%adm_cond)
res<-oisillon[w,]
checks<-lappend(checks,res,msg)


###############################################################
### C31
###############################################################

msg<-"Make sure that dead or disappeared chicks have 0 for flight code"

w<-which(oisillon$condition%in%c("disparu","mort") & oisillon$envol==1)
res<-oisillon[w,]
checks<-lappend(checks,res,msg)


###############################################################
### C32
###############################################################

msg<-"Make sure that living chicks with a 0 flight code are eventually dead or disappeared"

w<-which(oisillon$condition%in%c("vivant") & oisillon$envol==0)
ids<-unique(oisillon$idois[w])
if(any(w)){
  x<-oisillon[oisillon$idois%in%ids,]
  x<-x[order(x$idois,x$jjulien),]
  x<-ddply(x,.(idois),function(i){!tail(i$condition,1)%in%c("mort","disparu")})
  ids2<-x$idois[x$V1]
  res<-oisillon[oisillon$idois%in%ids2,]
  res<-res[order(res$idois,res$jjulien),]
}else{
  res<-NULL
}
checks<-lappend(checks,res,msg)


###############################################################
### C33
###############################################################

msg<-"Make sure that no chick comes back to life"

x<-oisillon[order(oisillon$idois,oisillon$jjulien,oisillon$heure),]
res<-ddply(x,.(idois),function(i){
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
ids<-res$idois[res$V1]
if(length(ids)){
  res<-oisillon[oisillon$idois%in%ids,]
  res<-res[order(res$idois,res$jjulien,res$heure),]
}else{
  res<-NULL
}
checks<-lappend(checks,res,msg)


###############################################################
### C34
###############################################################

msg<-"Chicks which were followed for 12 days or more should have a band number as id and otherwise they should have a farm/brood id"

### find chicks for which id is not the band number despite having been followed after their 12e days
x<-oisillon
x$idois<-paste0(x$ferme,x$nichoir,x$annee,x$nnich,x$numero_oisillon)
x<-ddply(x,.(idois),function(i){
     sup<-any(which(i$jour_suivi>=12))
     if(sup){
       res<-all(i$idois==paste0(i$prefixe,i$suffixe))
     }else{
       res<-all(i$idois==i$i$idois)
     }
     res 
})
ids<-x$idois[!x$V1]
if(length(ids)){
  res<-oisillon[oisillon$idois%in%ids,] 
  res<-res[order(res$idois,res$jjulien,res$heure),]
}else{
  res<-NULL
}
checks<-lappend(checks,res,msg)


###############################################################
### C35
###############################################################

msg<-"Chicks for which there is a band number but it does not correspond to the id of the chick"

x<-oisillon
x$idois<-paste0(x$ferme,x$nichoir,x$annee,x$nnich,x$numero_oisillon)
w<-which(!is.na(x$prefixe) & !is.na(x$suffixe) & x$idois!=paste0(x$prefixe,x$suffixe))
ids<-x$idois[w]
if(length(ids)){
  res<-oisillon[oisillon$idois%in%ids,] 
  res<-res[order(res$idois,res$jjulien,res$heure),]
}else{
  res<-NULL
}
checks<-lappend(checks,res,msg)


###############################################################
### C36
###############################################################

msg<-"Broods that are in chicks db but not in broods db"

temp<-setdiff(oisillon$idcouvee,couvee$idcouvee)
if(length(temp)>0){
  res<-temp
}else{
  res<-NULL
}
checks<-lappend(checks,res,msg)


###############################################################
### C37
###############################################################

msg<-"Broods that are in broods db but not in chicks db"

temp<-setdiff(couvee$idcouvee,oisillon$idcouvee)
if(length(temp)>0){
  res<-temp
}else{
  res<-NULL
}
checks<-lappend(checks,res,msg)




##########################################################
### Summarize brood information
##########################################################

y<-ddply(oisillon,.(idcouvee),function(i){
  Nois<-length(unique(i$numero_oisillon))
  Nenvol<-length(unique(i$numero_oisillon[i$envol==1]))
  Ndead<-length(unique(i$numero_oisillon[i$condition%in%"mort"]))
  Ndispa<-length(unique(i$numero_oisillon[i$condition%in%"disparu"]))
  ans<-data.frame(idcouvee=i$idcouvee[1],Nois,Nenvol,Ndead,Ndispa)
  ans
    
})



### hatching detected in brood but no nestlings in chicks
# check possible erreur dans le script original avec le min et la max de declo

### checks on hatch dates and brood stuff

x<-couvee
#w<-which(is.na(x$declomin) | is.na(x$declomax) & !is.na(y$Nois)) #if this does not output integer(0), Needs to be checked
#res<-x[w, ]
#checks<-lappend(checks,res,msg)


################################################################
### PRINT RESULTS and replace empty data.frames with NULLs
################################################################

### replace empty data.frames with NULLs
nchecks<-names(checks)
checks<-lapply(checks,function(i){
  if(is.data.frame(i) && nrow(i)==0L){
    NULL
  }else{
    i  
  }  
})
names(checks)<-nchecks

### print check results
invisible(sapply(seq_along(checks),function(i){
  cat("\n","\n",paste("_______",paste("Check",formatC(i,width=3,flag=0),names(checks)[i],sep=" - "),"_______"),"\n","\n","\n")
  print(checks[[i]])
}))

checks

}





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












