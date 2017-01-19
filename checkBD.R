
library(readxl)
library(plyr)


########################################################################################
########################################################################################
#This script is intended to cross validate the different data bases for the TRSW project
#
########################################################################################
########################################################################################


path<-"C:/Users/God/Documents/UdeS/Hiver2017/ELefol/Doc/BD 2004-2015"

checks<-list(NULL)



###############################################
### Build coloumn types
###############################################

couv_col<-c(rep("text",4),rep("numeric",19),rep("text",6),"text")
adul_col<-c(rep("text",3),"numeric","numeric","text","numeric","numeric",rep("text",3),"numeric",rep("text",5),rep("numeric",10),rep("text",3))

couvee<-read_excel(file.path(path,"Couvee2016.xlsx"),sheet=1,na="NA",col_types=couv_col)
adulte<-read_excel(file.path(path,"Adulte2016.xlsx"),sheet="Adultes2016",na="NA",col_types=adul_col)
oisillon<-read_excel(file.path(path,"oisillons2016.xlsx"),sheet=1,na="NA") 


##########################################################################################
### delete empty lines included in the excel file by assuming all entries have a ferme id
##########################################################################################

checkNArows<-function(x){
  res<-x[!is.na(x$ferme),]
  res
}
warning("All entries are assumed to have a \"ferme\" id")
couvee<-checkNArows(couvee)
adulte<-checkNArows(adulte)
oisillon<-checkNArows(oisillon)


################################################
### Check for females and brood assignment
################################################

x<-merge(couvee[,c("idcouvee","idF1")],adulte[adulte$sexe_morpho=="F",c("idcouvee", "idadult")],by="idcouvee",all.x=TRUE)


w<-which(is.na(x$idF1) & !is.na(x$idadult))
res<-x[w, ]
msg<-c("Females without matches in the couvee file")
checks[[length(checks)]]<-res
names(checks)[length(checks)]<-msg

w<-which(x$idF1!=x$idadult)
res<-x[w, ]
msg<-c("Adult females wwrongly assigned to couvee")
checks[[length(checks)+1]]<-res
names(checks)[length(checks)]<-msg


################################################
### Check for males and brood assignment
################################################

x<-merge(couvee[,c("idcouvee","idM1")],adulte[adulte$sexe_morpho=="F",c("idcouvee", "idadult")],by="idcouvee",all.x=TRUE)


msg<-c("Males without matches in the couvee file")
w<-which(is.na(x$idM1) & !is.na(x$idadult))
res<-x[w, ]
checks[[length(checks)]]<-res
names(checks)[length(checks)]<-msg

msg<-c("Adult males wwrongly assigned to couvee")
w<-which(x$idM1!=x$idadult)
res<-x[w, ]
checks[[length(checks)+1]]<-res
names(checks)[length(checks)]<-msg


##########################################################
### Check if the nnich correspond
##########################################################

x<-merge(adulte,couvee[couvee$codesp==1,c("idcouvee","codesp", "abandon", "dponte", "declomin", "declomax", "denvomin", "denvomax", "dabanmin","dabanmax")],by="idcouvee",all.x = TRUE)


msg<-"Capture date is lower than the laying date and the individual has not been found dead"
w<-which(x$jjulien < x$dponte & x$condition == 1)
res<-x[w, ]
checks[length(checks)+1]<-if(nrow(res)){res}else{list(NULL)}
names(checks)[length(checks)]<-msg


#3.2) 
#Check if some lines from the adult DB are not associated with a line that do not 
#correspond to a TRSW in the couvee file
#Not sure what that means!!!
msg<-"Adults in the adult DB that are not in the couvee DB"
w<-which(x$codesp != 1 & x$condition == 1)
res <- x[w,]
checks[length(checks)+1]<-if(nrow(res)){res}else{list(NULL)}
names(checks)[length(checks)]<-msg


#3.3)
msg<-"Capture date is later than the min or max departure date from the nest"
w<-which((x$jjulien > x$denvomin) | (x$jjulien > x$denvomax))
res<-x[w,]
checks[length(checks)+1]<-if(nrow(res)){res}else{list(NULL)}
names(checks)[length(checks)]<-msg


#3.3.2) Theses cases should be checked thoroughly as date of abandonment is tracked back to the first day when the eggs were cold.
# e.g. Incubation is declared on day 145. 147 and 149 eggs are cold but female is caught anyway on day 149. On field, we consider that 
# the nest was abandonned on day 151 (if incubation was declared previously and eggs are cold for 3 consecutive visits) and stop 
# following this nest (i.e. adult manipulations) from this date. However, in the data base, dabanmin = 146 and dabanmax = 147. 
# That is, the first day when the eggs were cold during that 3 visits sequence of cold eggs... probably not clear...
msg<-"Capture date is later than the min or max date of nest abandonment"
w<-which(((x$jjulien > x$dabanmin) | (x$jjulien > x$dabanmax)) & x$condition == 1)
res<-x[w, ]
checks[length(checks)+1]<-if(nrow(res)){res}else{list(NULL)}
names(checks)[length(checks)]<-msg


############################################## 
### Is nnich assigned correctly
##############################################

x<-merge(oisillon,couvee[couvee$codesp==1,c("idcouvee","dponte","dincub","declomin","declomax","dabanmin","dabanmax")],by="idcouvee",all.x=TRUE)


msg<-"Capture date of young is later than the minimal abandonment date if nest was abandoned"
w<-which(x$jjulien > (x$dabanmin + 1))
res <- x[w, ]
checks[length(checks)+1]<-if(nrow(res)){res}else{list(NULL)}
names(checks)[length(checks)]<-msg

msg<-"Capture date of young is before the laying date"
w<-which(x$jjulien < x$dponte)
res<-x[w,]
checks[length(checks)+1]<-if(nrow(res)){res}else{list(NULL)}
names(checks)[length(checks)]<-msg


##########################################################
### Summarize brood information
##########################################################


## Summarise nestling informations ==> idcouvee, number of nestlings, number of fledglings, number of dead nestlings, number of nestling dispareared

SumOis <- data.frame(idcouvee = character(0), Nois = numeric(0), Nenvol = numeric(0),  NDead = numeric(0) , NDispa = numeric(0))

z <- unique(oisillons$idcouvee)

for(i in z){
  
  # i = z[1]
  
  id <- subset(oisillons, idcouvee == i)
  
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

Errors4 <- couveeOis[0,]
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















