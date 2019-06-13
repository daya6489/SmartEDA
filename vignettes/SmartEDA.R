## ----setup, include=FALSE------------------------------------------------
library(rmarkdown)
library(SmartEDA)
library(knitr)
library(ISLR)
library(scales)
library(gridExtra)
library(ggplot2)


## ----eda-c3-r, warning=FALSE,eval=F--------------------------------------
#  install.packages("ISLR")
#  library("ISLR")
#  install.packages("SmartEDA")
#  library("SmartEDA")
#  ## Load sample dataset from ISLR pacakge
#  Carseats= ISLR::Carseats

## ----od_1,warning=FALSE,eval=F,include=T---------------------------------
#  # Overview of the data - Type = 1
#  ExpData(data=Carseats,type=1)
#  
#  # Structure of the data - Type = 2
#  ExpData(data=Carseats,type=2)

## ----od_2,warning=FALSE,eval=T,include=F---------------------------------
ovw_tabl <- ExpData(data=Carseats,type=1)
ovw_tab2 <- ExpData(data=Carseats,type=2)

## ----od_3,warning=FALSE,eval=T,render=ovw_tabl,echo=F--------------------
kable(ovw_tabl, "html")

## ----od_31,warning=FALSE,eval=T,render=ovw_tab2,echo=F-------------------
kable(ovw_tab2, "html")

## ----c1.1,warning=FALSE,eval=T,include=F---------------------------------
ec1 = ExpNumStat(Carseats,by="A",gp=NULL,Qnt=seq(0,1,0.1),MesofShape=2,Outlier=TRUE,round=2,Nlim=3)
rownames(ec1)<-NULL

## ----c1.11, warning=FALSE,eval=F,include=T-------------------------------
#  ExpNumStat(Carseats,by="A",gp=NULL,Qnt=seq(0,1,0.1),MesofShape=2,Outlier=TRUE,round=2,Nlim=10)
#  

## ----c1.12,warning=FALSE,eval=T,render=ec1,echo=F------------------------
paged_table(ec1)

## ----c1.2 ,warning=FALSE,eval=T,include=T,fig.align='center',fig.height=7,fig.width=7----
# Note: Variable excluded (if unique value of variable which is less than or eaual to 10 [nlim=10])
plot1 <- ExpNumViz(Carseats,target=NULL,nlim=10,Page=c(2,2),sample=4)
plot1[[1]]

## ----ec13, eval=T,include=F----------------------------------------------
et1 <- ExpCTable(Carseats,Target=NULL,margin=1,clim=10,nlim=5,round=2,bin=NULL,per=T)
rownames(et1)<-NULL

## ----ec14, warning=FALSE,eval=F,include=T--------------------------------
#  ExpCTable(Carseats,Target=NULL,margin=1,clim=10,nlim=3,round=2,bin=NULL,per=T)

## ----ec14.1,warning=FALSE,eval=T,render=et1,echo=F-----------------------
kable(et1,"html")

## ----bp1,warning=FALSE,eval=T,include=T,fig.align='center',fig.height=7,fig.width=7----
plot2 <- ExpCatViz(Carseats,target=NULL,col ="slateblue4",clim=10,margin=2,Page = c(2,1),sample=4)
plot2[[1]]

## ----tbd0,warning=FALSE,eval=T,include=T---------------------------------
summary(Carseats[,"Price"])

## ----con_1,warning=FALSE,eval=T,include=F--------------------------------
cpp = ExpNumStat(Carseats,by="A",gp="Price",Qnt=seq(0,1,0.1),MesofShape=1,Outlier=TRUE,round=2)
rownames(cpp)<-NULL

## ----con_2, warning=FALSE,eval=F,include=T-------------------------------
#  ExpNumStat(Carseats,by="A",gp="Price",Qnt=seq(0,1,0.1),MesofShape=1,Outlier=TRUE,round=2)

## ----con_3,warning=FALSE,eval=T,render=cpp,echo=F------------------------
paged_table(cpp)

## ----snv1,warning=FALSE,eval=T,include=T,fig.align='center',fig.height=7,fig.width=7----
#Note: sample=8 means randomly selected 8 scatter plots
#Note: nlim=4 means included numeric variable with unique value is more than 4
plot3 <- ExpNumViz(Carseats,target="Price",nlim=4,scatter=FALSE,fname=NULL,col="green",Page=c(2,2),sample=8)
plot3[[1]]

## ----snv1_1,warning=FALSE,eval=T,include=T,fig.align='center',fig.height=7,fig.width=7----
#Note: sample=8 means randomly selected 8 scatter plots
#Note: nlim=4 means included numeric variable with unique value is more than 4
plot31 <- ExpNumViz(Carseats,target="US",nlim=4,scatter=TRUE,fname=NULL,Page=c(2,1),sample=4)
plot31[[1]]

## ----eda_41, eval=T,include=F--------------------------------------------
et11 <- ExpCTable(Carseats,Target="Price",margin=1,clim=10,round=2,bin=4,per=F)
rownames(et11)<-NULL

## ----e4.2, warning=FALSE,eval=F,include=T--------------------------------
#  ##bin=4, descretized 4 categories based on quantiles
#  ExpCTable(Carseats,Target="Price",margin=1,clim=10,round=2,bin=4,per=F)

## ----e4.2.1,warning=FALSE,eval=T,render=et11,echo=F----------------------
paged_table(et11)

## ----dd,warning=FALSE,eval=T,include=F-----------------------------------
tab_tar <- data.frame(table(Carseats[,"Urban"]))
tab_tar$Descriptions <- "Store location"
names(tab_tar) <- c("Urban","Frequency","Descriptions")
rownames(tab_tar)<-NULL

## ----dv-r,warning=FALSE,eval=T,render=tab_tar,echo=F---------------------
kable(tab_tar, "html")

## ----snc1,warning=FALSE,eval=T,include=F---------------------------------
snc = ExpNumStat(Carseats,by="GA",gp="Urban",Qnt=seq(0,1,0.1),MesofShape=2,Outlier=TRUE,round=2)
rownames(snc)<-NULL

## ----snc2, warning=FALSE,eval=F,include=T--------------------------------
#  ExpNumStat(Carseats,by="GA",gp="Urban",Qnt=seq(0,1,0.1),MesofShape=2,Outlier=TRUE,round=2)

## ----snc3,warning=FALSE,eval=T,render=snc,echo=F-------------------------
paged_table(snc)

## ----bp3.1,warning=FALSE,eval=T,include=T,fig.align='center',fig.height=7,fig.width=7----
plot4 <- ExpNumViz(Carseats,target="Urban",type=1,nlim=3,fname=NULL,col=c("darkgreen","springgreen3","springgreen1"),Page=c(2,2),sample=8)
plot4[[1]]

## ----ed3.3, eval=T,include=F---------------------------------------------
et100 <- ExpCTable(Carseats,Target="Urban",margin=1,clim=10,nlim=3,round=2,bin=NULL,per=F)
rownames(et100)<-NULL

et4 <- ExpCatStat(Carseats,Target="Urban",result = "Stat",clim=3,nlim=3,bins=10,Pclass="Yes",plot=FALSE,top=20,Round=2)
rownames(et4)<-NULL


et5 <- ExpCatStat(Carseats,Target="Urban",result = "IV",clim=10,nlim=5,bins=10,Pclass="Yes",plot=FALSE,top=20,Round=2)
rownames(et5)<-NULL
et5 <- et5[1:15,]

## ----ed3.4, warning=FALSE,eval=F,include=T-------------------------------
#  ExpCTable(Carseats,Target="Urban",margin=1,clim=10,nlim=3,round=2,bin=NULL,per=F)

## ----ed3.5,warning=FALSE,eval=T,render=et100,echo=F,out.height=8,out.width=8----
kable(et100,"html")

## ----ed3.6, warning=FALSE,eval=F,include=T-------------------------------
#  ExpCatStat(Carseats,Target="Urban",result = "IV",clim=10,nlim=5,bins=10,Pclass="Yes",plot=FALSE,top=20,Round=2)
#  

## ----ed3.7,warning=FALSE,eval=T,render=et5,echo=F,out.height=8,out.width=8----
kable(et5,"html")

## ----ed3.8, warning=FALSE,eval=F,include=T-------------------------------
#  et4 <- ExpCatStat(Carseats,Target="Urban",result = "Stat",clim=10,nlim=5,bins=10,Pclass="Yes",plot=FALSE,top=20,Round=2)

## ----ed3.9,warning=FALSE,eval=T,render=et4,echo=F,out.height=8,out.width=8----
kable(et4,"html")

## ----ed3.91,warning=FALSE,eval=T,fig.align='center',fig.height=7,fig.width=7----
varimp <- ExpCatStat(Carseats,Target="Urban",result = "Stat",clim=10,nlim=5,bins=10,Pclass="Yes",plot=TRUE,top=10,Round=2)

## ----ed3.10,warning=FALSE,eval=T,include=T,fig.align='center',fig.height=7,fig.width=7----
plot5 <- ExpCatViz(Carseats,target="Urban",fname=NULL,clim=5,col=c("slateblue4","slateblue1"),margin=2,Page = c(2,1),sample=2)
plot5[[1]]

## ----warning=FALSE,eval=T,include=T,fig.align='center',fig.height=7,fig.width=7---------------------------------------------------------------------
options(width = 150)
CData = ISLR::Carseats
qqp <- ExpOutQQ(CData,nlim=10,fname=NULL,Page=c(2,2),sample=4)
qqp[[1]]

## ----warning=FALSE,eval=T,include=T,fig.align='center',fig.height=3,fig.width=7---------------------------------------------------------------------
ExpParcoord(CData,Group=NULL,Stsize=NULL,Nvar=c("Price","Income","Advertising","Population","Age","Education"))

## ----warning=FALSE,eval=T,include=T,fig.align='center',fig.height=3,fig.width=7---------------------------------------------------------------------
ExpParcoord(CData,Group="ShelveLoc",Stsize=c(10,15,20),Nvar=c("Price","Income"),Cvar=c("Urban","US"))


## ----warning=FALSE,eval=T,include=T,fig.align='center',fig.height=3,fig.width=7---------------------------------------------------------------------
ExpParcoord(CData,Group="ShelveLoc",Nvar=c("Price","Income"),Cvar=c("Urban","US"),scale=NULL)


## ----warning=FALSE,eval=T,include=T,fig.align='center',fig.height=3,fig.width=7---------------------------------------------------------------------
ExpParcoord(CData,Group="US",Nvar=c("Price","Income"),Cvar=c("ShelveLoc"),scale="std")


## ----warning=FALSE,eval=T,include=T,fig.align='center',fig.height=3,fig.width=7---------------------------------------------------------------------
ExpParcoord(CData,Group="ShelveLoc",Stsize=c(10,15,20),Nvar=c("Price","Income","Advertising","Population","Age","Education"))

## ----warning=FALSE,eval=T,include=T,fig.align='center',fig.height=3,fig.width=7---------------------------------------------------------------------
ExpParcoord(CData,Group="US",Stsize=c(15,50),Cvar=c("ShelveLoc","Urban"))


## ----dudu, eval=T,include=F-------------------------------------------------------------------------------------------------------------------------
e1du <- ExpCustomStat(Carseats,Cvar="Urban",Nvar=c("Age","Price"),stat=c("mean","count"),gpby=TRUE,dcast=F)
rownames(e1du)<-NULL

e1du1 <- ExpCustomStat(Carseats,Cvar="Urban",Nvar=c("Age","Price"),stat=c("mean","count"),gpby=TRUE,dcast=T)
rownames(e1du1)<-NULL

e1du2 <- ExpCustomStat(Carseats,Cvar=c("Urban","ShelveLoc"),Nvar=c("Age","Price","Advertising","Sales"),stat=c("mean"),gpby=FALSE,dcast=T)
rownames(e1du2)<-NULL


## ----dud1, warning=FALSE,eval=F,include=T-----------------------------------------------------------------------------------------------------------
#  ExpCustomStat(Carseats,Cvar="Urban",Nvar=c("Age","Price"),stat=c("mean","count"),gpby=TRUE,dcast=F)
#  

## ----dud12,warning=FALSE,eval=T,render=e1du,echo=F,out.height=8,out.width=8-------------------------------------------------------------------------
kable(e1du,"html")

## ----dud2, warning=FALSE,eval=F,include=T-----------------------------------------------------------------------------------------------------------
#  ExpCustomStat(Carseats,Cvar="Urban",Nvar=c("Age","Price"),stat=c("mean","count"),gpby=TRUE,dcast=T)
#  

## ----dud21,warning=FALSE,eval=T,render=e1du1,echo=F,out.height=8,out.width=8------------------------------------------------------------------------
kable(e1du1,"html")

## ----dud3, warning=FALSE,eval=F,include=T-----------------------------------------------------------------------------------------------------------
#  ExpCustomStat(Carseats,Cvar=c("Urban","ShelveLoc"),Nvar=c("Age","Price","Advertising","Sales"),stat=c("mean"),gpby=FALSE,dcast=T)
#  

## ----dud31,warning=FALSE,eval=T,render=e1du2,echo=F,out.height=8,out.width=8------------------------------------------------------------------------
kable(e1du2,"html")

