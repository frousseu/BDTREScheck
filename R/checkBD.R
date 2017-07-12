

checkBD<-function(dsn=".",
           year,
           adultsNew=NULL,
           broodsNew=NULL,
           chicksNew=NULL,
           adultsOld="Adultes_2004-2015.xlsx",
           broodsOld="Couvee_2004-2015.xlsx",
           chicksOld="Oisillons_2004-2015.xls",
           sheet=1,
           stop=FALSE)
{
 

#############################################
### INTERNAL FUNCTIONS
#############################################

### function to append a list with a check and a message
# x = initial list
# a = thing to append
# msg = the name of the check that will be used as a name
# le premier if devrait être enlevé le problème vient du fait de c'er des liste qui ne marche pas comme le reste  
lappend<-function(x,a,msg){
  if(!is.list(a) || is.data.frame(a)){
    a<-list(a)  
    x<-c(x,a)
    names(x)[length(x)]<-msg
  }else{
    x[[length(x)+1]]<-a
    names(x)[length(x)]<-msg  
  }
  x
}


### function for checking if visits are all 2 days apart
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
### READ DATA
###############################################

couv_col  <- c(rep("text",4),rep("numeric",19),rep("text",6),"text")
adul_col  <- c(rep("text",3),"numeric","numeric","text","date","numeric",rep("text",3),"numeric",rep("text",5),rep("numeric",10),rep("text",3))
chick_col <- c(rep("text",3),rep("numeric",2), "text", "date", "numeric", rep("text", 6), rep("numeric", 8), rep("text",2))  

### read_excel est sûrement utilisé temporairement et je supprime donc les warnings associés à la détection de caractères non-attendus

broodsOld<-suppressWarnings(as.data.frame(read_excel(file.path(dsn,broodsOld),sheet=sheet,na="NA",col_types=couv_col,guess_max=100000)))
adultsOld<-suppressWarnings(as.data.frame(read_excel(file.path(dsn,adultsOld),sheet=sheet,na="NA",col_types=adul_col,guess_max=100000)))
chicksOld<-suppressWarnings(as.data.frame(read_excel(file.path(dsn,chicksOld),sheet=sheet,na="NA",col_types=chick_col,guess_max=100000))) 

### If not specify New data not specified, Get New data from Old dataset (subsetting based year)
if(is.null(broodsNew)==T){
   broodsNew <-   broodsOld[which(broodsOld$annee==year),]
   }else{
      broodsNew<-suppressWarnings(as.data.frame(read_excel(file.path(dsn,broodsNew),sheet=1,na="NA",col_types=couv_col,guess_max=100000)))
      #adultsNew <- adultsNew[which(adultsNew$annee < year),]
   }

if(is.null(adultsNew)==T){
   adultsNew <-   adultsOld[which(adultsOld$annee==year),]
   }else{
   adultsNew<-suppressWarnings(as.data.frame(read_excel(file.path(dsn,adultsNew),sheet=1,na="NA",col_types=adul_col,guess_max=100000)))
   #chicksNew <- chicksNew[which(chicksNew$annee < year),]
   }

if(is.null(chicksNew)==T){
   chicksNew <-   chicksOld[which(chicksOld$annee==year),]
   }else{
      chicksNew<-suppressWarnings(as.data.frame(read_excel(file.path(dsn,chicksNew),sheet=1,na="NA",col_types=chick_col,guess_max=1000000))) 
      #chicksNew <- chicksNew[which(chicksNew$annee < year),]
   }

# Use the year argument to subset Old dataset

broodsOld <- broodsOld[which(broodsOld$annee < year),]
adultsOld <- adultsOld[which(adultsOld$annee < year),]
chicksOld <- chicksOld[which(chicksOld$annee < year),]

### make certain changes to columns and column names
adultsNew$heure<-substr(adultsNew$heure,12,16)
adultsOld$heure<-substr(adultsOld$heure,12,16)
chicksNew$heure<-substr(chicksNew$heure,12,16)
chicksOld$heure<-substr(chicksOld$heure,12,16)

### build list of results to checks
checks<-list()


##########################################################################################
### REMOVE ferme id's with NA and NA lines generated by read_excel or excel files
##########################################################################################

### Empty lines are sometime generated when using read_excel

msg<-"GENERAL: Remove rows with NA id's in broodsNew"
ini<-nrow(broodsNew)
broodsNew<-checkNArows(broodsNew)
res<-paste("Removed",ini-nrow(broodsNew),"rows with NA ferme id's")
checks<-lappend(checks,res,msg)

msg<-"GENERAL: Remove rows with NA id's in adultsNew"
ini<-nrow(adultsNew)
adultsNew<-checkNArows(adultsNew)
res<-paste("Removed",ini-nrow(adultsNew),"rows with NA ferme id's")
checks<-lappend(checks,res,msg)

msg<-"GENERAL: Remove rows with NA id's in chicksNew"
ini<-nrow(chicksNew)
chicksNew<-checkNArows(chicksNew)
res<-paste("Removed",ini-nrow(chicksNew),"rows with NA ferme id's")
checks<-lappend(checks,res,msg)

#warning("All entries are assumed to have a non-NA \"ferme\" id")


########################################################################
### First check for good column names in recent database
########################################################################

### This will stop the function to prevent further checks if something does not match, if everything is ok, a NULL will be appended to the check list

### function for checking if column names are valid in each db
check_names<-function(x){
  bd<-deparse(substitute(x))
  m<-switch(bd,
            adultsNew=which(is.na(match(adulte_names,names(x)))),
            chicksNew=which(is.na(match(oisillon_names,names(x)))),
            broodsNew=which(is.na(match(couvee_names,names(x))))
  )
  if(any(m)){
    stop(paste0("No matches for following column names in ",bd,": ",paste(names(x)[m],collapse=", "),", please revise before running the checkBD function."))
  }
}

### column names
adulte_names<-c("ferme","nichoir","id","annee","nnich","idcouvee","heure","jjulien","prefixe","suffixe","idadult","condition","sexe_morpho","age_morpho","sexe_gen","locus_sexe_gen","couleur","age_exact","laile1","laile2","masse","tarse1","tarse2","trougauche","troudroite","pararectrice","plaqueincu","Cause_recapt","commentaire","observateur")

oisillon_names<-c("ferme","nichoir","id","annee","nnich","idcouvee","heure","jjulien","prefixe","suffixe","idois","sexe_gen","locus_sexe_gen","condition","numero_oisillon","jour_suivi","envol","masse","9primaires1","9primaires2","tarse1","tarse2","commentaires","manipulateur")

couvee_names<-c("idcouvee","id","ferme","nichoir","annee","codesp","nnich","noeufs","noisnes","noisenvol","noismort","dispa_ois","dispa_oeufs","abandon","pred_pot","dponte","dincub","declomin","declomax","denvomin","denvomax","dabanmin","dabanmax","idF1","idM1","idF2","idF3","idM2","idM3","Commentaires")


msg<-"GENERAL: Are column names in adult consistent?"
check_names(adultsNew)
checks<-lappend(checks,NULL,msg)

msg<-"GENERAL: Are column names in chicksNew consistent?"
check_names(chicksNew)
checks<-lappend(checks,NULL,msg)

msg<-"GENERAL: Are column names in broodsNew consistent?"
check_names(broodsNew)
checks<-lappend(checks,NULL,msg)



#####################################################
### Check if names are consitent across years
#####################################################

### This will also stop the function to prevent further checks if some names do not match, if everything is ok, a NULL will be appended to the check list

msg<-"GENERAL: Are column names consistant across old and new databases"

res<-list()
res[[1]]<-c(setdiff(names(broodsNew),names(broodsOld)),setdiff(names(broodsOld),names(broodsNew)))
res[[2]]<-c(setdiff(names(adultsNew),names(adultsOld)),setdiff(names(adultsOld),names(adultsNew)))
res[[3]]<-c(setdiff(names(chicksNew),names(chicksOld)),setdiff(names(chicksOld),names(chicksNew)))
res<-lapply(res,function(i){if(length(i)==0){NULL}else{i}})
names(res)<-c("broodsNew","adultsNew","chicksNew")

if(any(!sapply(res,is.null))){
  print(res)
  stop("Preceding column names not consistent across old and new databases")
}

checks<-lappend(checks,NULL,msg)

##########################################################################################
### Check that ferme id include only possible values
##########################################################################################

ferme_names <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36",  "37", "38", "39", "40", "41") 

msg<-"ADULTS: Wrong ferme id observed"

x<-adultsNew
w<-which(!x$ferme%in%ferme_names)
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","idadult")],msg)

##########################################################################################
### 
##########################################################################################

msg<-"NESTLINGS: Wrong ferme id observed"

x<-chicksNew
w<-which(!x$ferme%in%ferme_names)
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","idois")],msg)

##########################################################################################
### 
##########################################################################################

msg<-"BROODS: Wrong ferme id observed"

x<-broodsNew
w<-which(!x$ferme%in%ferme_names)
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee")],msg)


##########################################################################################
### Check that nichoir id include only possible values
##########################################################################################

nichoir_names_all <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10")
nichoir_names_41  <- c("11", "12", "13", "14", "15", "16", "17", "18", "19") 

msg<-"ADULTS: Wrong nichoir id observed"

x<-adultsNew
w<-which((!x$ferme%in%c("23","41") & !x$nichoir%in%nichoir_names_all) | (x$ferme%in%c("23") & !x$nichoir%in%c(nichoir_names_all, "11")) | (x$ferme%in%c("41") & !x$nichoir%in%c(nichoir_names_all, nichoir_names_41)))
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","idadult")],msg)

##########################################################################################
### 
##########################################################################################

msg<-"NESTLINGS: Wrong nichoir id observed"

x<-chicksNew
w<-which((!x$ferme%in%c("23","41") & !x$nichoir%in%nichoir_names_all) | (x$ferme%in%c("23") & !x$nichoir%in%c(nichoir_names_all, "11")) | (x$ferme%in%c("41") & !x$nichoir%in%c(nichoir_names_all, nichoir_names_41)))
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","idois")],msg)

##########################################################################################
### 
##########################################################################################

msg<-"BROODS: Wrong nichoir id observed"

x<-broodsNew
w<-which((!x$ferme%in%c("23","41") & !x$nichoir%in%nichoir_names_all) | (x$ferme%in%c("23") & !x$nichoir%in%c(nichoir_names_all, "11")) | (x$ferme%in%c("41") & !x$nichoir%in%c(nichoir_names_all, nichoir_names_41)))
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee")],msg)

##########################################################################################
### Check that id == ferme + nichoir
##########################################################################################

msg<-"ADULTS: id column doesn't correspond to ferme + nichoir id"

x<-adultsNew
x$TEST <- paste(x$ferme, x$nichoir, sep="")
w<-which(x$id != x$TEST)
checks<-lappend(checks,x[w,c("ferme","nichoir", "id", "idcouvee","jjulien","idadult")],msg)

##########################################################################################
### 
##########################################################################################

msg<-"NESTLINGS: id column doesn't correspond to ferme + nichoir id"

x<-chicksNew
x$TEST <- paste(x$ferme, x$nichoir, sep="")
w<-which(x$id != x$TEST)
checks<-lappend(checks,x[w,c("ferme","nichoir", "id", "idcouvee","jjulien","idois")],msg)

##########################################################################################
### 
##########################################################################################

msg<-"BROODS: id column doesn't correspond to ferme + nichoir id"

x<-broodsNew
x$TEST <- paste(x$ferme, x$nichoir, sep="")
w<-which(x$id != x$TEST)
checks<-lappend(checks,x[w,c("ferme","nichoir","id","idcouvee")],msg)

##########################################################################################
### Check that idcouv == ferme + nichoir + annee + nnich
##########################################################################################

msg<-"ADULTS: idcouv doesn't correspond to ferme + nichoir + annee + nnich column"

x<-adultsNew
x$TEST <- paste(x$ferme, x$nichoir, x$annee, x$nnich, sep="")
w<-which(x$idcouvee != x$TEST)

checks<-lappend(checks,x[w,c("ferme","nichoir", "annee", "nnich", "idcouvee","jjulien","idadult")],msg)

##########################################################################################
### 
##########################################################################################

msg<-"NESTLINGS: idcouv doesn't correspond to ferme + nichoir + annee + nnich column"

x<-chicksNew
x$TEST <- paste(x$ferme, x$nichoir, x$annee, x$nnich, sep="")
w<-which(x$idcouvee != x$TEST)
checks<-lappend(checks,x[w,c("ferme","nichoir", "annee", "nnich", "idcouvee","jjulien","idois")],msg)

##########################################################################################
### 
##########################################################################################

msg<-"BROODS: idcouv doesn't correspond to ferme + nichoir + annee + nnich column"

x<-broodsNew
x$TEST <- paste(x$ferme, x$nichoir, x$annee, x$nnich, sep="")
w<-which(x$idcouvee != x$TEST)
checks<-lappend(checks,x[w,c("ferme","nichoir", "annee", "nnich", "idcouvee","codesp")],msg)


##########################################################################################
### Check that prefixe columns include only possible values
##########################################################################################

prefixe_names <- c("1881", "1921", "2221", "2311", "2351", "2490", "2511", "2521", "2591", "2621")

msg<-"ADULTS: Wrong prefixe name observed (is it a new prefixe?)"

x<-adultsNew
w<-which(!x$prefixe%in%prefixe_names & !is.na(x$prefixe))
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","prefixe", "suffixe","idadult")],msg)

##########################################################################################
### 
##########################################################################################

msg<-"NESTLINGS: Wrong prefixe name observed (is it a new prefixe?)"

x<-chicksNew
w<-which(!x$prefixe%in%prefixe_names & !is.na(x$prefixe))
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","prefixe", "suffixe","idois")],msg)


##########################################################################################
### Check the number of characters which shoudl always be fixed in the different ids
##########################################################################################

#msg<-"Are number of characters consistent for id-type columns in adultsNew db?"
#w<-check_nchar(adultsNew)
#checks<-lappend(checks,adultsNew[w,],msg)
#
#msg<-"Are number of characters consistent for id-type columns in chicksNew db?"
#w<-check_nchar(chicksNew)
#checks<-lappend(checks,chicksNew[w,],msg)
#
#msg<-"Are number of characters consistent for id-type columns in broodsNew db?"
#w<-check_nchar(broodsNew)
#checks<-lappend(checks,broodsNew[w,],msg)


################################################################################
### Second check for unique values in columns with few non-numeric values
################################################################################

col_val<-c("annee","nnich","jjulien","sexe_morpho","age_exact")

# function for checking unique values
check_val<-function(x){
  n<-col_val[!is.na(match(col_val,names(x)))]
  res<-sapply(n,function(i){
    sort(unique(x[,i]),na.last=TRUE)
  })
  res
}

msg<-"ADULTS: Show all unique values in columns for which the number of possible values is restricted"

checks<-lappend(checks,check_val(adultsNew),msg)

msg<-"BROODS: Show all unique values in columns for which the number of possible values is restricted"

checks<-lappend(checks,check_val(broodsNew),msg)

msg<-"NESTLINGS: Show all unique values in columns for which the number of possible values is restricted"

checks<-lappend(checks,check_val(chicksNew),msg)


################################################
### Check for females and brood assignment
################################################

msg<-"ADULTS/BROODS: Females assigned to an idcouv in adults db but no female is assigned to this idcouv in broods db (check capture dates)"

x<-merge(broodsNew[!is.na(broodsNew$nnich),c("idcouvee","idF1","idF2","idF3", "dponte", "denvomax", "dabanmax")],adultsNew[adultsNew$sexe_morpho=="F" | adultsNew$sexe_gen=="F" ,c("idcouvee", "idadult", "jjulien", "sexe_gen", "sexe_morpho")],by="idcouvee",all.x=TRUE)
w<-which(is.na(x$idF1) & is.na(x$idF2) & is.na(x$idF3) & !is.na(x$idadult))
checks<-lappend(checks,x[w, ],msg)


################################################
### 
################################################

msg<-"ADULTS/BROODS: Females assigned to an idcouv in adults db but not referenced in broods db (idF2 or idF3)"

w<-which((x$idF1!=x$idadult | is.na(x$idF1) == T) & (x$idF2!=x$idadult | is.na(x$idF2) == T) & (x$idF3!=x$idadult | is.na(x$idF3) == T) & !(is.na(x$idF1)==T & is.na(x$idF2)==T & is.na(x$idF3)==T) )
checks<-lappend(checks,x[w,],msg)

################################################
### Females not assign to an idcouv
################################################

msg<-"ADULTS/BROODS: Females captured at a nestbox but not assigned to an idcouv in adults db (check nnich)"

x<-merge(broodsNew[,c("id","idcouvee","idF1","idF2","idF3", "dponte", "denvomax", "dabanmax")],adultsNew[adultsNew$sexe_morpho=="F" | adultsNew$sexe_gen=="F" ,c("id","idcouvee", "nnich", "idadult", "jjulien", "sexe_gen", "sexe_morpho")],by="id",all.x=TRUE)
w<-which(is.na(x$nnich) & !is.na(x$idadul) & !is.na(x$dponte) & x$dponte <= x$jjulien & (x$denvomax >= x$jjulien|x$dabanmax >= x$jjulien))
checks<-lappend(checks,x[w,],msg)

################################################
### Females assign to the wrong idcouv
################################################

msg<-"ADULTS/BROODS: Females captured at a nestbox but assigned to a wrong idcouv in adults db (check nnich)"

w<-which(!is.na(x$nnich) & !is.na(x$idadul) & !is.na(x$dponte) & x$dponte <= x$jjulien & (x$denvomax >= x$jjulien|x$dabanmax >= x$jjulien) & x$idcouvee.x != x$idcouvee.y)
checks<-lappend(checks,x[w,],msg)

################################################
### 
################################################

msg<-"ADULTS/BROODS: Males assigned to an idcouv in adults db but no male is assigned to this idcouv in broods db (check capture dates)"

x<-merge(broodsNew[!is.na(broodsNew$nnich),c("idcouvee","idM1","idM2","idM3")],adultsNew[adultsNew$sexe_morpho=="M" | adultsNew$sexe_gen=="M" ,c("idcouvee", "idadult", "jjulien", "sexe_gen", "sexe_morpho")],by="idcouvee",all.x=TRUE)
w<-which(is.na(x$idM1) & is.na(x$idM2) & is.na(x$idM3) & !is.na(x$idadult))
checks<-lappend(checks,x[w,],msg)

################################################
### 
################################################

msg<-"ADULTS/BROODS: Males assigned to an idcouv in adults db but not referenced in broods db (idM2 or idM3)"

w<-which((x$idM1!=x$idadult | is.na(x$idM1) == T) & (x$idM2!=x$idadult | is.na(x$idM2) == T) & (x$idM3!=x$idadult | is.na(x$idM3) == T) & !(is.na(x$idM1)==T & is.na(x$idM2)==T & is.na(x$idM3)==T) )
checks<-lappend(checks,x[w,],msg)

################################################
### Males not assign to an idcouv
################################################

msg<-"ADULTS/BROODS: Males captured at a nestbox but not assigned to an idcouv in adults db (check nnich)"

x<-merge(broodsNew[,c("id","idcouvee","idM1","idM2","idM3", "declomin", "denvomax", "dabanmax")],adultsNew[adultsNew$sexe_morpho=="M" | adultsNew$sexe_gen=="M" ,c("id","idcouvee", "nnich", "idadult", "jjulien", "sexe_gen", "sexe_morpho")],by="id",all.x=TRUE)
w<-which(is.na(x$nnich) & !is.na(x$idadul) & !is.na(x$declomin) & x$declomin <= x$jjulien & (x$denvomax >= x$jjulien|x$dabanmax >= x$jjulien))
checks<-lappend(checks,x[w,],msg)

################################################
### Males assign to the wrong idcouv
################################################

msg<-"ADULTS/BROODS: Males captured at a nestbox but assigned to a wrong idcouv in adults db (check nnich)"

w<-which(!is.na(x$nnich) & !is.na(x$idadul) & !is.na(x$declomin) & x$declomin <= x$jjulien & (x$denvomax >= x$jjulien|x$dabanmax >= x$jjulien) & x$idcouvee.x != x$idcouvee.y)
checks<-lappend(checks,x[w,],msg)

################################################
### No idF1
################################################

msg<-"BROODS: Female reference as second female when only one female captured (change to idF1 - some exceptions possible)"

w<-which(is.na(broodsNew$idF1) & (!is.na(broodsNew$idF2) | !is.na(broodsNew$idF3)))

checks<-lappend(checks,broodsNew[w,c("idcouvee","idF1","idF2","idF3", "Commentaires")],msg)

################################################
### No idM1
################################################

msg<-"BROODS: Male reference as second males when only one male captured (change to idM1)"

w<-which(is.na(broodsNew$idM1) & (!is.na(broodsNew$idM2) | !is.na(broodsNew$idM3)))

checks<-lappend(checks,broodsNew[w,c("idcouvee","idM1","idM2","idM3", "Commentaires")],msg)

################################################
### Two males in a nestbox
################################################

msg<-"BROODS: Two males captured in the same nestbox but not properly reported (idM2 and idM3, not idM1)"

w<-which(!is.na(broodsNew$idM1) & (!is.na(broodsNew$idM2) | !is.na(broodsNew$idM3)))
checks<-lappend(checks,broodsNew[w,c("idcouvee","idM1","idM2","idM3", "Commentaires")],msg)

###############################################################
### 
###############################################################

msg<-"ADULTS: Sex/age incoherencies within the current year"

x<-adultsNew
w<-which((x$sexe_morpho%in%c("F") & !x$age_morpho%in%c("SY","ASY",NA)) | (x$sexe_morpho%in%c("M") & !x$age_morpho%in%c("AHY",NA)) | (x$sexe_morpho%in%c(NA) & !x$age_morpho%in%c(NA)))
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","idadult","sexe_morpho","age_morpho","commentaire")],msg)

###############################################################
### 
###############################################################

msg<-"ADULTS: sex/age incoherencies between years"
checks<-lappend(checks,"TO DO !!!",msg)

###############################################################
### Capture time - Nestlings
###############################################################

msg<-"ADULTS: Capture time outside 06:00 and 20:40 (max)"

mmh<-c("06:00","20:40")

x<-adultsNew
w<-which(x$heure<mmh[1] | x$heure>mmh[2])
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","idadult","heure","commentaire")],msg)

###############################################################
### Capture time - Nestlings
###############################################################

msg<-"NESTLINGS: Capture time outside 06:00 and 20:40 (max)"

x<-chicksNew
w<-which(x$heure<mmh[1] | x$heure>mmh[2])
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","idois","heure","commentaires")],msg)

###############################################################
### Genetic sex combination with loci in adults
###############################################################

msg<-"ADULTS: Wrong sexe_gen/locus_sexe_gen association (both NA or with values)"

x<-adultsNew
w<-which(!(is.na(x$sexe_gen) & is.na(x$locus_sexe_gen) | (x$sexe_gen%in%c("M","F","I") & x$locus_sexe_gen%in%c("P2P8","L2550"))))
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","idadult","sexe_gen","locus_sexe_gen")],msg)

###############################################################
### Adults which change sex during a breeding season
###############################################################

msg<-"ADULTS: Check for adults with changing sexe_morph (within the current breeding season ONLY)"

checks<-lappend(checks,check_id_dup(adultsNew,col=c("idadult","sexe_morpho"))[,c("ferme","nichoir","idcouvee","jjulien","idadult","sexe_morpho","sexe_gen","locus_sexe_gen","commentaire")],msg)

###############################################################
### Adults which change sex during across breeding season
###############################################################

msg<-"ADULTS: Check for adults with changing sexe_morph (across seasons)"

checks<-lappend(checks,check_id_dup(rbind(adultsOld[adultsOld$idadult%in%unique(adultsNew$idadult),],adultsNew),col=c("idadult","sexe_morpho"))[,c("annee","ferme","nichoir","idcouvee","jjulien","idadult","sexe_morpho","sexe_gen","locus_sexe_gen","commentaire")],msg)

###############################################################
### Adults which change sex during a breeding season
###############################################################

msg<-"ADULTS: Check for adults with changing sexe_gen (within the current breeding season ONLY)"

checks<-lappend(checks,check_id_dup(adultsNew,col=c("idadult","sexe_gen"))[,c("ferme","nichoir","idcouvee","jjulien","idadult","sexe_morpho","sexe_gen","locus_sexe_gen","commentaire")],msg)

###############################################################
### Adults which change sex during across breeding season
###############################################################

msg<-"ADULTS: Check for adults with changing sexe_gen (across seasons)"

checks<-lappend(checks,check_id_dup(rbind(adultsOld[adultsOld$idadult%in%unique(adultsNew$idadult),],adultsNew),col=c("idadult","sexe_gen"))[,c("annee","ferme","nichoir","idcouvee","jjulien","idadult","sexe_morpho","sexe_gen","locus_sexe_gen","commentaire")],msg)

###############################################################
### Adults which change sex during a breeding season
###############################################################

msg<-"ADULTS: Check for adults with changing locus_sexe_gen (within the current breeding season ONLY)"

checks<-lappend(checks,check_id_dup(adultsNew,col=c("idadult","locus_sexe_gen"))[,c("ferme","nichoir","idcouvee","jjulien","idadult","sexe_morpho","sexe_gen","locus_sexe_gen","commentaire")],msg)

###############################################################
### Adults which change sex during across breeding season
###############################################################

msg<-"ADULTS: Check for adults with changing locus_sexe_gen (across seasons)"

checks<-lappend(checks,check_id_dup(rbind(adultsOld[adultsOld$idadult%in%unique(adultsNew$idadult),],adultsNew),col=c("idadult","locus_sexe_gen"))[,c("annee","ferme","nichoir","idcouvee","jjulien","idadult","sexe_morpho","sexe_gen","locus_sexe_gen","commentaire")],msg)

###############################################################
### 
###############################################################

msg<-"ADULTS: Some colors not in the list of possible values"

x<-adultsNew
w<-which(!x$couleur%in%c("B","V","BV","BR",NA))
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","idadult","couleur","commentaire")],msg)

###############################################################
###
###############################################################

msg<-"ADULTS: Wing measurement outside the range of likely values (105-125 mm)"

x<-adultsNew
val<-c(105,125)
w<-which(x$laile1<val[1] | x$laile2<val[1] | x$laile1>val[2] | x$laile2>val[2])
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","idadult","laile1","laile2","commentaire")],msg)

msg<-"ADULTS: Wing measurement 1 and 2 too far apart (>1 mm)"

x<-adultsNew
val<-c(1)
w<-which(abs(x$laile1 - x$laile2) > val)
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","idadult","laile1","laile2","commentaire")],msg)

###############################################################
### 
###############################################################

msg<-"ADULTS: Weight measurements outside the range of likely values (15-30g)"

x<-adultsNew
val<-c(15,30)
w<-which(x$masse<val[1] | x$masse>val[2])
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","idadult","masse","commentaire")],msg)

###############################################################
###
###############################################################

msg<-"ADULTS: Tarsus measurements outside the range of likely values (10-14 mm)"

x<-adultsNew
val<-c(10,14)
w<-which(x$tarse1<val[1] | x$tarse2<val[1] | x$tarse1>val[2] | x$tarse2>val[2])
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","idadult","tarse1","tarse2","commentaire")],msg)


msg<-"ADULTS: tarsus measurement 1 and 2 too far apart (>0.1 mm)"

x<-adultsNew
val<-c(0.1)
w<-which(abs(x$tarse1 - x$tarse2) > val)
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","idadult","tarse1","tarse2","commentaire")],msg)

###############################################################
###
###############################################################

msg<-"ADULTS: Wrong condition status"

x<-adultsNew
w<-which(!x$condition%in%c("0","1","2","3"))
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","idadult","condition","commentaire")],msg)

###############################################################
###
###############################################################

msg<-"ADULTS: Wrong plaqueincu status"

x<-adultsNew
w<-which(!x$plaqueincu%in%c("0","1") & !is.na(x$plaqueincu))
checks<-lappend(checks,x[w,c("ferme","nichoir","jjulien","idcouvee","idadult","plaqueincu","commentaire")],msg)

###############################################################
###
###############################################################

msg<-"ADULTS: Male with brood patch (plaqueincu)"

x<-adultsNew
w<-which(x$sexe_morpho%in%c("M") & !x$plaqueincu%in%c("0",NA))
checks<-lappend(checks,x[w,c("ferme","nichoir","jjulien","idcouvee","idadult","sexe_morpho","sexe_gen","plaqueincu","commentaire")],msg)

###############################################################
###
###############################################################

msg<-"ADULTS: Wrong Cause_capture status"

x<-adultsNew
w<-which(!x$Cause_recapt%in%c("0","RPCS","ACC","RPCM"))# & !x$plaqueincu%in%c(0,NA))
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","idadult","Cause_recapt","commentaire")],msg)

##########################################################
###
##########################################################

msg<-"Newly installed band found in the previous years"

checks<-lappend(checks,"NEED TO BUILD A CODE FOR THIS!",msg)

###############################################################
###
###############################################################

msg<-"ADULTS: Visits are not all 2 days apart for the following farms"

checks<-lappend(checks,list(vis2days(adultsNew)),msg)

###############################################################
###
###############################################################

msg<-"NESTLINGS: Visits are not all 2 days apart for the following farms"

checks<-lappend(checks,list(vis2days(chicksNew)),msg)

###############################################################
### DUPLICATES
###############################################################

msg<-"ADULTS: Check for duplicates using all columns"

checks<-lappend(checks,check_dup(adultsNew),msg)

msg<-"BROODS: Check for duplicates using all columns"

checks<-lappend(checks,check_dup(broodsNew),msg)

msg<-"CHICKS: Check for duplicates using all columns"

checks<-lappend(checks,check_dup(chicksNew),msg)

###############################################################
###
###############################################################

msg<-"ADULTS: Check for adults with more than one entry for a single date"

checks<-lappend(checks,check_dup(adultsNew,col=c("idadult","jjulien"))[,c("ferme","nichoir","idcouvee","jjulien","idadult","condition","commentaire")],msg)

msg<-"NESTLINGS: Check for chicks with more than one entry for a single date"

checks<-lappend(checks,check_dup(chicksNew,col=c("idois","jjulien"))[,c("ferme","nichoir","idcouvee","jjulien","idois","jour_suivi","condition","commentaires")],msg)

msg<-"NESTLINGS: Check for chicks with more than one entry for a single age"

checks<-lappend(checks,check_dup(chicksNew,col=c("idois","jour_suivi"))[,c("ferme","nichoir","idcouvee","jjulien","idois","jour_suivi","condition","commentaires")],msg)

###############################################################
###
###############################################################

msg<-"ADULTS: Check for adults found at more than one farm (maybe not an error)"

checks<-lappend(checks,check_id_dup(adultsNew,col=c("idadult","ferme"))[,c("ferme","nichoir","idcouvee","jjulien","idadult","commentaire")],msg)

###############################################################
###
###############################################################

msg<-"NESTLINGS: Check for nestlings found at more than one nestbox"

checks<-lappend(checks,check_id_dup(chicksNew,col=c("idois","ferme","nichoir"))[,c("ferme","nichoir","idcouvee","jjulien","jour_suivi","idois", "numero_oisillon", "commentaires")],msg)

###############################################################
### 
###############################################################

msg<-"NESTLINGS/BROODS: Capture date of young is later than the minimal abandonment date if nest was abandoned"
x<-merge(chicksNew,broodsNew[broodsNew$codesp==1,c("idcouvee","dponte","dincub","declomin","declomax","dabanmin","dabanmax")],by="idcouvee",all.x=TRUE)
w<-which(x$jjulien > (x$dabanmin + 1))
checks<-lappend(checks,x[w,],msg)

###############################################################
### 
###############################################################

msg<-"NESTLINGS/BROODS: Capture date of young is before the laying date"

w<-which(x$jjulien < x$dponte)
checks<-lappend(checks,x[w,],msg)

###############################################################
### 
###############################################################

msg<-"NESTLINGS/BROODS: jjulien of young that doesn't correspond to declomax + jour_suivi"

w<-which(x$jjulien != (x$declomax + x$jour_suivi))
checks<-lappend(checks,x[w,c("idcouvee","ferme","nichoir","idois","declomax","jjulien","jour_suivi")],msg)

###############################################################
### Genetic sex combination with loci in nestlings
###############################################################

msg<-"NESTLINGS: Wrong sexe_gen/locus_sexe_gen association (both NA or with values)"

x<-chicksNew
w<-which(!(is.na(x$sexe_gen) & is.na(x$locus_sexe_gen) | (x$sexe_gen%in%c("M","F","I") & x$locus_sexe_gen%in%c("P2P8","L2550"))))
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","idois","sexe_gen","locus_sexe_gen")],msg)

###############################################################
### Nestlings which change sex during a breeding season
###############################################################

msg<-"NESTLINGS: Check for individuals with changing sexe_gen"

checks<-lappend(checks,check_id_dup(chicksNew,col=c("idois","sexe_gen"))[,c("ferme","nichoir","idcouvee","jjulien","idois","sexe_morpho","sexe_gen","locus_sexe_gen","commentaires")],msg)

###############################################################
### Nestlings which change locus sex during a breeding season
###############################################################

msg<-"NESTLINGS: Check for individuals with changing locus_sexe_gen"

checks<-lappend(checks,check_id_dup(chicksNew,col=c("idois","locus_sexe_gen"))[,c("ferme","nichoir","idcouvee","jjulien","idois","sexe_morpho","sexe_gen","locus_sexe_gen","commentaires")],msg)

###############################################################
###
###############################################################

msg<-"NESTLINGS: Wrong chick conditions (4 possible values; vivant, disparu, mort or disparuj16)"

adm_cond<-c("vivant", "disparu", "mort", "disparuj16")
x<-chicksNew
w<-which(!x$condition%in%adm_cond)
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","idois","condition","envol")],msg)


###############################################################
###
###############################################################

msg<-"NESTLINGS: Dead or disappeared nestlings without a 0 for flight code (few exceptions possibles, see comments)"

x<-chicksNew
w<-which(x$condition%in%c("disparu","mort") & x$envol=="1")
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","idois","condition","envol","commentaires")],msg)


###############################################################
###
###############################################################

msg<-"NESTLINGS: Nestling with disparuj16 condition but without a 1 for flight code"

x<-chicksNew
w<-which(x$condition%in%c("disparuj16") & x$envol=="0")
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","idois","condition","envol","commentaires")],msg)

###############################################################
### 
###############################################################

msg<-"NESTLINGS: Make sure that living nestlings with a 0 flight code are eventually dead or disappeared"

w<-which(chicksNew$condition%in%c("vivant") & chicksNew$envol==0)
ids<-unique(chicksNew$idois[w])
if(any(w)){
  x<-chicksNew[chicksNew$idois%in%ids,]
  x<-x[order(x$idois,x$jjulien),]
  x<-ddply(x,.(idois),function(i){!tail(i$condition,1)%in%c("mort","disparu")})
  ids2<-x$idois[x$V1]
  res<-chicksNew[chicksNew$idois%in%ids2,]
  res<-res[order(res$idois,res$jjulien),]
}else{
  res<-NULL
}
checks<-lappend(checks,res[,c("ferme","nichoir","idcouvee","jour_suivi","idois","condition","envol")],msg)


###############################################################
### MOUAAHAHAHAHA!
###############################################################

msg<-"NESTLINGS: Make sure that no nestling comes back to life"

x<-chicksNew[order(chicksNew$idois,chicksNew$jjulien,chicksNew$heure),]
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
  res<-chicksNew[chicksNew$idois%in%ids,]
  res<-res[order(res$idois,res$jjulien,res$heure),]
}else{
  res<-NULL
}
checks<-lappend(checks,res[,c("ferme","nichoir","idcouvee","jour_suivi","idois","condition","envol")],msg)


###############################################################
###
###############################################################

msg<-"NESTLINGS: Nestlings which were followed for 12 days or more should have a band number as id and otherwise they should have a farm/brood id (maybe an exception, see comments)"

### find chicks for which id is not the band number despite having been followed after their 12e days
x<-chicksNew
x$idois<-paste0(x$ferme,x$nichoir,x$annee,x$nnich,x$numero_oisillon)
x<-ddply(x,.(idois),function(i){
     sup<-any(which(i$jour_suivi>=12 & !i$condition%in%c("mort","disparu")))
     if(sup){
       res<-all(i$idois==paste0(i$prefixe,i$suffixe))
     }else{
       res<-all(i$idois==i$i$idois)
     }
     res 
})
ids<-x$idois[!x$V1]
if(length(ids)){
  res<-chicksNew[chicksNew$idois%in%ids,] 
  res<-res[order(res$idois,res$jjulien,res$heure),]
}else{
  res<-NULL
}
checks<-lappend(checks,res[,c("ferme","nichoir","idcouvee","jour_suivi","idois","numero_oisillon","condition","envol","commentaires")],msg)


###############################################################
###
###############################################################

msg<-"NESTLINGS: Chicks for which there is a band number but it does not correspond to the id of the chick"

x<-chicksNew
x$idois<-paste0(x$ferme,x$nichoir,x$annee,x$nnich,x$numero_oisillon)
w<-which(!is.na(x$prefixe) & !is.na(x$suffixe) & x$idois!=paste0(x$prefixe,x$suffixe))
ids<-x$idois[w]
if(length(ids)){
  res<-chicksNew[chicksNew$idois%in%ids,] 
  res<-res[order(res$idois,res$jjulien,res$heure),]
}else{
  res<-NULL
}
checks<-lappend(checks,res[,c("ferme","nichoir","idcouvee","jour_suivi","idois","numero_oisillon","commentaires")],msg)

###############################################################
### 9primaires checks in nestlings
###############################################################

msg<-"NESTLINGS: 9primaires larger than expected (65 mm, no age consideration)"

x<-chicksNew
val<-c(65)
w<-which(x$"9primaires1">val | x$"9primaires2">val)
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","idois","9primaires1","9primaires2","commentaires")],msg)

msg<-"NESTLINGS: 9primaires outside the range of likely values at 6-day-old (0 - 10 mm)"

x<-chicksNew
val<-c(0,10)
w<-which(x$jour_suivi == 6 & (x$"9primaires1"<val[1] | x$"9primaires2"<val[1] | x$"9primaires1">val[2] | x$"9primaires2">val[2]))
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jour_suivi","idois","9primaires1","9primaires2","commentaires")],msg)

msg<-"NESTLINGS: 9primaires outside the range of likely values at 12-day-old (5 - 45 mm)"

x<-chicksNew
val<-c(5,45)
w<-which(x$jour_suivi == 12 & (x$"9primaires1"<val[1] | x$"9primaires2"<val[1] | x$"9primaires1">val[2] | x$"9primaires2">val[2]))
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jour_suivi","idois","9primaires1","9primaires2","commentaires")],msg)

msg<-"NESTLINGS: 9primaires outside the range of likely values at 16-day-old (15 - 65 mm)"

x<-chicksNew
val<-c(15,65)
w<-which(x$jour_suivi == 16 & (x$"9primaires1"<val[1] | x$"9primaires2"<val[1] | x$"9primaires1">val[2] | x$"9primaires2">val[2]))
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jour_suivi","idois","9primaires1","9primaires2","commentaires")],msg)

msg<-"NESTLINGS: 9primaires 1 and 2 too far apart (>0.1 mm)"

x<-chicksNew
val<-c(0.1)
w<-which(abs(x$"9primaires1" - x$"9primaires2") > val)
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","idois","9primaires1","9primaires2","commentaires")],msg)

###############################################################
### 
###############################################################

msg<-"NESTLINGS: Weight measurements larger than expected (27 g, no age consideration)"

x<-chicksNew
val<-c(27)
w<-which(x$masse>val[1])
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","idois","masse","commentaires")],msg)

msg<-"NESTLINGS: Weight measurements outside the range of likely value at 2-days-old (1-8 g)"

x<-chicksNew
val<-c(1,8)
w<-which(x$jour_suivi == 2 & (x$masse<val[1] | x$masse>val[2]))
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jour_suivi","idois","masse","commentaires")],msg)

msg<-"NESTLINGS: Weight measurements outside the range of likely value at 6-days-old (2-20 g)"

x<-chicksNew
val<-c(2,20)
w<-which(x$jour_suivi == 6 & (x$masse<val[1] | x$masse>val[2]))
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jour_suivi","idois","masse","commentaires")],msg)

msg<-"NESTLINGS: Weight measurements outside the range of likely value at 12-days-old (10-27 g)"

x<-chicksNew
val<-c(10,27)
w<-which(x$jour_suivi == 12 & (x$masse<val[1] | x$masse>val[2]))
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jour_suivi","idois","masse","commentaires")],msg)

msg<-"NESTLINGS: Weight measurements outside the range of likely value at 16-days-old (12-27 g)"

x<-chicksNew
val<-c(12,27)
w<-which(x$jour_suivi == 16 & (x$masse<val[1] | x$masse>val[2]))
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jour_suivi","idois","masse","commentaires")],msg)


###############################################################
###
###############################################################

msg<-"NESTLINGS: Tarsus measurements outside the range of likely values (10-14 mm)"

x<-chicksNew
val<-c(10,14)
w<-which(x$tarse1<val[1] | x$tarse2<val[1] | x$tarse1>val[2] | x$tarse2>val[2])
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","idois","tarse1","tarse2","commentaires")],msg)


msg<-"NESTLINGS: Tarsus measurement 1 and 2 too far apart (>0.1 mm)"

x<-chicksNew
val<-c(0.1)
w<-which(abs(x$tarse1 - x$tarse2) > val)
checks<-lappend(checks,x[w,c("ferme","nichoir","idcouvee","jjulien","idois","tarse1","tarse2","commentaires")],msg)

###############################################################
###
###############################################################

msg<-"NESTLINGS/BROODS: Broods that are in chicks db but not in broods db"

temp<-setdiff(chicksNew$idcouvee,broodsNew$idcouvee)
if(length(temp)>0){
  res<-temp
}else{
  res<-NULL
}
checks<-lappend(checks,res,msg)

###############################################################
###
###############################################################

msg<-"NESTLINGS/BROODS: TRES broods with at least one nestling that are in broods db but not in chicks db"

temp<-setdiff(broodsNew$idcouvee[which(broodsNew$codesp == 1 & broodsNew$noisnes >=1)],chicksNew$idcouvee)
if(length(temp)>0){
  x<-temp
}else{
  x<-NULL
}
checks<-lappend(checks,x,msg)

###############################################################
### Adults which were nestlings in our system
###############################################################

msg<-"ADULTS/NESTLINGS: Check for individuals with changing sexe_gen and locus_sexe_gen across db"

checks<-lappend(checks,"TO DO!!!",msg)


msg<-"ADULTS: Check for age_exact column for individuals hatched in our study system"

checks<-lappend(checks,"TO DO!!!",msg)

###############################################################
### Broods initial checks
###############################################################

msg<-"BROODS: Check for duplicates in idcouvee"

checks<-lappend(checks,check_dup(broodsNew, col=c("idcouvee")),msg)

###############################################################
### Broods initial checks
###############################################################

msg<-"BROODS: Check for duplicates in id/nnich (change nnich)"

checks<-lappend(checks,check_dup(broodsNew, col=c("id","nnich")),msg)

###############################################################
### Missing id
###############################################################

msg<-"BROODS: Check for missing id (add lines for them)"

id_names <- expand.grid(ferme_names[1:40], nichoir_names_all)
id_names <- as.data.frame(paste(id_names[,1],id_names[,2],sep=""))
names(id_names)<- "id"

x<-merge(id_names,broodsNew[,c("id","idcouvee")],by="id",all.x=TRUE)
w<-which(is.na(x$idcouvee))
checks<-lappend(checks,x[w,],msg)

###############################################################
### Wrong codesp
###############################################################

msg<-"BROODS: Wrong codesp"

x<-broodsNew
w<-which(!x$codesp%in%c(1:5,NA) | (x$codesp != 1 & (!is.na(x$idF1) | !is.na(x$idF2) | !is.na(x$idF3) | !is.na(x$idM1) | !is.na(x$idM2) | !is.na(x$idM3))))
checks<-lappend(checks,x[w,c("idcouvee","ferme","nichoir","codesp","Commentaires")],msg)

###############################################################
### Check nnich 1 vs 2 (2 = later)
###############################################################

msg<-"BROODS: Wrong nnich"

checks<-lappend(checks,"TO DO!!!",msg)

###############################################################
### Wrong abandon / pred_pot
###############################################################

msg<-"BROODS: Wrong abandon / pred_pot"

x<-broodsNew
w<-which(!x$abandon%in%c(0:2,NA) | !x$pred_pot%in%c(0:1,NA) | (x$abandon!=1 & x$pred_pot==1))
checks<-lappend(checks,x[w,c("idcouvee","ferme","nichoir","abandon","pred_pot","Commentaires")],msg)

###############################################################
### Check for chronolgy in events
###############################################################

msg<-"BROODS: Wrong chronology in events within a brood"

x<-broodsNew
w<-which(x$dponte > x$dincub | x$dincub > x$declomin | x$declomin > x$declomax | x$declomax > x$denvomin | x$denvomin > x$denvomax | x$dponte > x$declomin)
checks<-lappend(checks,x[w,c("idcouvee","ferme","nichoir","dponte","dincub","declomin","declomax","denvomin","denvomax","Commentaires")],msg)

###############################################################
### Checks for errors in clutch size vs nestling
###############################################################

msg<-"BROODS: Broods with more nestlings than eggs"

x<-broodsNew
w<-which(x$noisnes > x$noeufs)
checks<-lappend(checks,x[w,c("idcouvee","ferme","nichoir","codesp","noeufs","noisnes","Commentaires")],msg)

###############################################################
### Checks for errors in clutch size vs nestling
###############################################################

msg<-"BROODS: More/less nestlings than nestling status (noines != noisenvol + noismort + dispa_ois)"

x<-broodsNew
w<-which(!(x$noisnes == (x$noisenvol + x$noismort + x$dispa_ois)))
checks<-lappend(checks,x[w,c("idcouvee","ferme","nichoir","codesp","noeufs","noisnes","noisenvol","noismort","dispa_ois","Commentaires")],msg)

###############################################################
### Too big difference between LD and II
###############################################################

msg<-"BROODS: Very long time elapse between laying date and incubation initiation (> 2 weeks; 2 different broods?)"

x<-broodsNew
w<-which(x$dincub - x$dponte + x$noeufs > 14)
checks<-lappend(checks,x[w,c("idcouvee","ferme","nichoir","noeufs","dponte","dincub","Commentaires")],msg)

###############################################################
### Too big difference between LD and HD
###############################################################

msg<-"BROODS: Very long time elapse between laying date and hatching date (> 4 weeks; 2 different broods?)"

x<-broodsNew
w<-which(x$declomin - x$dponte + x$noeufs > 28)
checks<-lappend(checks,x[w,c("idcouvee","ferme","nichoir","noeufs","dponte","declomin","Commentaires")],msg)

###############################################################
### Too big difference between II and HD
###############################################################

msg<-"BROODS: Very long time elapse between incubation initiation and hatching date (> 2 weeks; 2 different broods?)"

x<-broodsNew
w<-which(x$declomin - x$dincub > 14)
checks<-lappend(checks,x[w,c("idcouvee","ferme","nichoir","noeufs","dponte","dincub","declomin","Commentaires")],msg)

###############################################################
### Too big difference between eclo min and max
###############################################################

msg<-"BROODS: Too long time elapse between minimum and maximum hatching date (> 1 day)"

x<-broodsNew
w<-which(x$declomax - x$declomin > 1)
checks<-lappend(checks,x[w,c("idcouvee","ferme","nichoir","declomin","declomax","Commentaires")],msg)


##########################################################
### Summarize brood information
##########################################################

y<-ddply(chicksNew,.(idcouvee),function(i){
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

#x<-broodsNew
#w<-which(is.na(x$declomin) | is.na(x$declomax) & !is.na(y$Nois)) #if this does not output integer(0), Needs to be checked
#res<-x[w, ]
#checks<-lappend(checks,res,msg)


################################################################
### Replace empty data.frames with NULLs
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

### assign class
class(checks)<-"TREScheck"
checks

}





