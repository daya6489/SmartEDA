## ----setup, include=FALSE-----------------------------------------------------
library(rmarkdown)
library(SmartEDA)
library(knitr)
library(ISLR)
library(scales)
library(gridExtra)
library(ggplot2)


## ---------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 150)
CData = ISLR::Carseats
head(CData,5)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
ExpCustomStat(Carseats,Cvar=c("US","Urban","ShelveLoc","Education"),gpby=FALSE)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
ExpCTable(Carseats,Target=NULL,clim=5,nlim=15,round=2,bin=NULL,per=F)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
ExpCustomStat(Carseats,Cvar=c("US","Urban","ShelveLoc"),gpby=FALSE)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
ExpCustomStat(Carseats,Cvar=c("US","Urban"),gpby=TRUE,filt=NULL)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
ExpCustomStat(Carseats,Cvar=c("US","Urban","ShelveLoc"),gpby=TRUE,filt=NULL)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
ExpCustomStat(Carseats,Cvar=c("US","Urban"),gpby=TRUE,filt="Population>150")

## ---------------------------------------------------------------------------------------------------------------------------------------------------
ExpCustomStat(Carseats,Cvar=c("US","ShelveLoc"),gpby=TRUE,filt="Urban=='Yes' & Population>150")

## ---------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 150)
ExpCustomStat(Carseats,Nvar=c("Population","Sales","CompPrice","Income"),stat = c('Count','mean','sum','var','sd','min','max','IQR'))

## ---------------------------------------------------------------------------------------------------------------------------------------------------
ExpCustomStat(Carseats,Nvar=c("Population","Sales","CompPrice","Income"),stat = c('min','p0.25','median','p0.75','max'))

## ---------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 150)
ExpCustomStat(Carseats,Nvar=c("Population","Sales","CompPrice","Income"),stat = c('Count','mean','sum','var','min','median','max'),filt="Urban=='Yes'")

## ---------------------------------------------------------------------------------------------------------------------------------------------------
options(width=150)
ExpCustomStat(Carseats,Nvar=c("Population","Sales","CompPrice","Income"),stat = c('Count','mean','sum','median','IQR'),filt="Urban=='Yes' & Population>150")

## ---------------------------------------------------------------------------------------------------------------------------------------------------
data_sam = Carseats[,]
data_sam[sample(1:400,30),"Sales"] <- 999
data_sam[sample(1:400,20),"CompPrice"] <- -9
data_sam[sample(1:400,45),"Income"] <- 999
ExpCustomStat(data_sam,Nvar=c("Population","Sales","CompPrice","Income"),stat = c('Count','mean','sum','min'),filt="All %ni% c(999,-9)")

## ---------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 150)
ExpCustomStat(Carseats,Nvar=c("Population","Sales","CompPrice","Education","Income"),stat = c('Count','mean','sum','var','sd','IQR','median'),filt=c("ShelveLoc=='Good'^Urban=='Yes'^Price>=150^All^US=='Yes'"))

## ---------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 150)
ExpCustomStat(Carseats,Cvar = c("Urban","ShelveLoc"), Nvar=c("Population","Sales"), stat = c('Count','Prop','mean','min','P0.25','median','p0.75','max'),gpby=FALSE)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 150)
ExpCustomStat(Carseats,Cvar = c("Urban","US","ShelveLoc"), Nvar=c("CompPrice","Income"), stat = c('Count','Prop','mean','sum','PS','min','max','IQR','sd'), gpby = TRUE)

## ---------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 150)
ExpCustomStat(Carseats,Cvar = c("Urban","US","ShelveLoc"), Nvar=c("CompPrice","Income"), stat = c('Count','Prop','mean','sum','PS','median','IQR'), gpby = TRUE,filt="Urban=='Yes'")

## ---------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 150)
data_sam = Carseats[,]
data_sam[sample(1:400,30),"Sales"] <- 888
data_sam[sample(1:400,20),"CompPrice"] <- 999
data_sam[sample(1:400,45),"Income"] <- 999
ExpCustomStat(data_sam,Cvar = c("Urban","US","ShelveLoc"), Nvar=c("Sales","CompPrice","Income"), stat = c('Count','Prop','mean','sum','PS'), gpby = TRUE,filt="All %ni% c(888,999)")

## ---------------------------------------------------------------------------------------------------------------------------------------------------
ExpCustomStat(Carseats,Cvar = c("Urban","US"), Nvar=c("Population","Sales","CompPrice"), stat = c('Count','Prop','mean','sum','var','IQR'), filt=c("ShelveLoc=='Good'^Urban=='Yes'^Price>=150"))

## ---------------------------------------------------------------------------------------------------------------------------------------------------
options(width = 150)
ExpCustomStat(Carseats,Cvar = c("Urban"), Nvar=c("Population","Sales"), stat = c('Count','Prop'),gpby=TRUE,dcast=TRUE)

## ----warning=FALSE,eval=F,include=T-----------------------------------------------------------------------------------------------------------------
#  ##Frequency table for categorical variables
#  ExpCustomStat(Carseats,Cvar=c("US","Urban","ShelveLoc"),gpby=FALSE)
#  
#  ##Crosstabulation between categorical variables
#  ExpCustomStat(Carseats,Cvar=c("US","Urban"),gpby=TRUE,filt=NULL)
#  ExpCustomStat(Carseats,Cvar=c("US","Urban","ShelveLoc"),gpby=TRUE,filt=NULL)
#  
#  ##Adding filters for custom tables
#  ExpCustomStat(Carseats,Cvar=c("US","Urban"),gpby=TRUE,filt="Population>150")
#  ExpCustomStat(Carseats,Cvar=c("US","ShelveLoc"),gpby=TRUE,filt="Urban=='Yes' & Population>150")
#  
#  ## Numeric variable summary
#  ExpCustomStat(Carseats,Nvar=c("Population","Sales","CompPrice","Income"),stat = c('Count','mean','sum','var','min','max'))
#  ExpCustomStat(Carseats,Nvar=c("Population","Sales","CompPrice","Income"),stat = c('min','p0.25','median','p0.75','max'))
#  
#  ## Adding filters for complete data (like base Subset)
#  ExpCustomStat(Carseats,Nvar=c("Population","Sales","CompPrice","Income"),stat = c('Count','mean','sum','var'),filt="Urban=='Yes'")
#  ExpCustomStat(Carseats,Nvar=c("Population","Sales","CompPrice","Income"),stat = c('Count','mean','sum'),filt="Urban=='Yes' & Population>150")
#  
#  ## Filter unique value from all the numeric variables
#  ExpCustomStat(data_sam,Nvar=c("Population","Sales","CompPrice","Income"),stat = c('Count','mean','sum','min'),filt="All %ni% c(999,-9)")
#  
#  ## Adding filters at variable level
#  ExpCustomStat(Carseats,Nvar=c("Population","Sales","CompPrice","Education","Income"),stat = c('Count','mean','sum','var','sd','IQR','median'),filt=c("ShelveLoc=='Good'^Urban=='Yes'^Price>=150^ ^US=='Yes'"))
#  
#  ##Numerical summaries by category
#  ##Variable summary report (One group variable)
#  ExpCustomStat(Carseats,Cvar = c("Urban","ShelveLoc"), Nvar=c("Population","Sales"), stat = c('Count','Prop','mean','min','P0.25','median','p0.75','max'),gpby=FALSE)
#  
#  ##Variable summary report (More than One group variable)
#  ExpCustomStat(Carseats,Cvar = c("Urban","US","ShelveLoc"), Nvar=c("CompPrice","Income"), stat = c('Count','Prop','mean','sum','PS','min','max','IQR','sd'), gpby = TRUE)
#  
#  ##Variable summary report (More than One group variable) with filter
#  ExpCustomStat(Carseats,Cvar = c("Urban","US","ShelveLoc"), Nvar=c("CompPrice","Income"), stat = c('Count','Prop','mean','sum','PS','P0.25','median','p0.75'), gpby = TRUE,filt="Urban=='Yes'")
#  ExpCustomStat(data_sam,Cvar = c("Urban","US","ShelveLoc"), Nvar=c("Sales","CompPrice","Income"), stat = c('Count','Prop','mean','sum','PS'), gpby = TRUE,filt="All %ni% c(888,999)")
#  ExpCustomStat(Carseats,Cvar = c("Urban","US"), Nvar=c("Population","Sales","CompPrice"), stat = c('Count','Prop','mean','sum','var','min','max'), filt=c("ShelveLoc=='Good'^Urban=='Yes'^Price>=150"))
#  

