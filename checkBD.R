
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
couv_col<-c(rep("text",4),rep("numeric",19),rep("text",6),"text")
adul_col<-c(rep("text",3),"numeric","numeric","text","numeric","numeric",rep("text",3),"numeric",rep("text",5),rep("numeric",10),rep("text",3))

couvee<-read_excel(file.path(path,"Couvee2016.xlsx"),sheet=1,na="NA",col_types=couv_col)
adulte<-read_excel(file.path(path,"Adulte2016.xlsx"),sheet="Adultes2016",na="NA",col_types=adul_col)
oisillon<-read_excel(file.path(path,"oisillons2016.xlsx"),sheet=1,na="NA") 


##########################################################################################
### delete empty lines included in the excel file by assuming all entries have a ferme id
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

x<-merge(adulte,couvee[couvee$codesp==1,c("idcouvee","codesp", "abandon", "dponte", "declomin", "declomax", "denvomin", "denvomax", "dabanmin","dabanmax")],by="idcouvee",all.x = TRUE)


msg<-"Capture date is lower than the laying date and the individual has not been found dead"
w<-which(x$jjulien < x$dponte & x$condition == 1)
res<-x[w, ]
checks[length(checks)+1]<-if(nrow(res)){res}else{list(NULL)}
names(checks)[length(checks)]<-msg


#3.2) 
#Check if some lines from the adult DB are not associated with a line that do not 
#correspond to a TRSW in the couvee file
which(adulte2$codesp != 1)
check3.2 <- adulte2[which(adulte2$codesp != 1),]
#Those individuals might have been found dead. Check condition.
which(check3.2$condition != 1)

#3.3)
which(adulte2$jjulien > adulte2$denvomin | adulte2$jjulien > adulte2$denvomax)
check3.3.1 <- adulte2[which(adulte2$jjulien > adulte2$denvomin | adulte2$jjulien > adulte2$denvomax), ]

#3.3.2) Theses cases should be checked thoroughly as date of abandonment is tracked back to the first day when the eggs were cold.
# e.g. Incubation is declared on day 145. 147 and 149 eggs are cold but female is caught anyway on day 149. On field, we consider that 
# the nest was abandonned on day 151 (if incubation was declared previously and eggs are cold for 3 consecutive visits) and stop 
# following this nest (i.e. adult manipulations) from this date. However, in the data base, dabanmin = 146 and dabanmax = 147. 
# That is, the first day when the eggs were cold during that 3 visits sequence of cold eggs... probably not clear...
which(adulte2$jjulien > adulte2$dabanmin | adulte2$jjulien > adulte2$dabanmax)
check3.3.2 <- adulte2[which(adulte2$jjulien > adulte2$dabanmin | adulte2$jjulien > adulte2$dabanmax), ]
#again, check for condition and comments
which(check3.3.2$condition != 1)
check3.3.2.2 <- check3.3.2[which(check3.3.2$condition != 1), ]


