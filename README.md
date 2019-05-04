# SmartEDA [![CRAN status](https://www.r-pkg.org/badges/version/SmartEDA)](https://cran.r-project.org/package=SmartEDA)
<!--
[![CRAN Downloads](http://cranlogs.r-pkg.org/badges/SmartEDA)](https://cran.r-project.org/package=SmartEDA)
[![CRAN Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/SmartEDA)](https://cran.r-project.org/package=SmartEDA)
-->

---

## Background
In a quality statistical data analysis the initial step has to be exploratory. Exploratory data analysis begins with the univariate exploratory analyis - examining the variable one at a time. Next comes bivariate analysis followed by multivariate analyis. SmartEDA package helps in getting the complete exploratory data analysis just by running the function instead of writing lengthy r code.

## Installation

The package can be installed directly from CRAN.

    install.packages("SmartEDA")

You can install SmartEDA from github with:
	
```{r gh-installation, eval = FALSE}
# install.packages("devtools")
devtools::install_github("cran/SmartEDA")
```

## Example

#### Data
In this vignette, we will be using a simulated data set containing sales of child car seats at 400 different stores. 

Data Source [ISLR package](https://www.rdocumentation.org/packages/ISLR/versions/1.2/topics/Carseats).

Install the package "ISLR" to get the example data set.

	install.packages("ISLR")
	library("ISLR")
	install.packages("SmartEDA")
	library("SmartEDA")
	## Load sample dataset from ISLR pacakge
	Carseats= ISLR::Carseats

## Overview of the data
Understanding the dimensions of the dataset, variable names, overall missing summary and data types of each variables

	ExpData(data=Carseats,type=1)
	ExpData(data=Carseats,type=2)

## Summary of numerical variables
To summarise the numeric variables, you can use following r codes from this pacakge

	ExpNumStat(Carseats,by="A",gp=NULL,Qnt=seq(0,1,0.1),MesofShape=2,Outlier=TRUE,round=2)
	ExpNumStat(Carseats,by="A",gp="Price",Qnt=seq(0,1,0.1),MesofShape=1,Outlier=TRUE,round=2)
	ExpNumStat(Carseats,by="GA",gp="Urban",Qnt=seq(0,1,0.1),MesofShape=2,Outlier=TRUE,round=2)

## Graphical representation of all numeric features

	ExpNumViz(Carseats,gp=NULL,nlim=10,Page=c(2,2),sample=8)
	ExpNumViz(Carseats,gp="Urban",type=1,nlim=NULL,fname=NULL,col=c("pink","yellow","orange"),Page=c(2,2),sample=8)	

## Summary of Categorical variables	

	ExpCTable(Carseats,Target=NULL,margin=1,clim=10,nlim=5,round=2,bin=NULL,per=T)
	ExpCatViz(Carseats,target=NULL,fname=NULL,clim=10,margin=2,Page = c(2,1),sample=4)
	ExpCTable(Carseats,Target="Price",margin=1,clim=10,nlim=NULL,round=2,bin=4,per=F)
	ExpCatStat(Carseats,Target="Urban",result = "Stat",clim=10,nlim=5,Pclass="Yes")
	ExpCatStat(Carseats,Target="Urban",result = "IV",clim=10,nlim=5,Pclass="Yes")
	ExpCTable(Carseats,Target="Urban",margin=1,clim=10,nlim=NULL,round=2,bin=NULL,per=F)

## Graphical representation of all categorical variables

	ExpCatViz(Carseats,target="Urban",fname=NULL,clim=10,col=NULL,margin=2,Page = c(2,1),sample=2)
	
## Create HTML EDA report
Create a exploratory data analysis report in HTML format

	ExpReport(Carseats,Target="Urban",label=NULL,op_file="test.html",op_dir=getwd(),sc=2,sn=2,Rc="Yes")

## Exploratory analysis - Custom tables, summary statistics
Descriptive summary on all input variables for each level/combination of group variable. Also while running the analysis we can filter row/cases of the data. 

	ExpCustomStat(Carseats,Cvar=c("US","Urban","ShelveLoc"),gpby=FALSE)
	ExpCustomStat(Carseats,Cvar=c("US","Urban"),gpby=TRUE,filt=NULL)
	ExpCustomStat(Carseats,Cvar=c("US","Urban","ShelveLoc"),gpby=TRUE,filt=NULL)
	ExpCustomStat(Carseats,Cvar=c("US","Urban"),gpby=TRUE,filt="Population>150")
	ExpCustomStat(Carseats,Cvar=c("US","ShelveLoc"),gpby=TRUE,filt="Urban=='Yes' & Population>150")
	ExpCustomStat(Carseats,Nvar=c("Population","Sales","CompPrice","Income"),stat = c('Count','mean','sum','var','min','max'))
	ExpCustomStat(Carseats,Nvar=c("Population","Sales","CompPrice","Income"),stat = c('min','p0.25','median','p0.75','max'))
	ExpCustomStat(Carseats,Nvar=c("Population","Sales","CompPrice","Income"),stat = c('Count','mean','sum','var'),filt="Urban=='Yes'")
	ExpCustomStat(Carseats,Nvar=c("Population","Sales","CompPrice","Income"),stat = c('Count','mean','sum'),filt="Urban=='Yes' & Population>150")
	ExpCustomStat(data_sam,Nvar=c("Population","Sales","CompPrice","Income"),stat = c('Count','mean','sum','min'),filt="All %ni% c(999,-9)")
	ExpCustomStat(Carseats,Nvar=c("Population","Sales","CompPrice","Education","Income"),stat = c('Count','mean','sum','var','sd','IQR','median'),filt=c("ShelveLoc=='Good'^Urban=='Yes'^Price>=150^ ^US=='Yes'"))
	ExpCustomStat(Carseats,Cvar = c("Urban","ShelveLoc"), Nvar=c("Population","Sales"), stat = c('Count','Prop','mean','min','P0.25','median','p0.75','max'),gpby=FALSE)
	ExpCustomStat(Carseats,Cvar = c("Urban","US","ShelveLoc"), Nvar=c("CompPrice","Income"), stat = c('Count','Prop','mean','sum','PS','min','max','IQR','sd'), gpby = TRUE)
	ExpCustomStat(Carseats,Cvar = c("Urban","US","ShelveLoc"), Nvar=c("CompPrice","Income"), stat = c('Count','Prop','mean','sum','PS','P0.25','median','p0.75'), gpby = TRUE,filt="Urban=='Yes'")
	ExpCustomStat(data_sam,Cvar = c("Urban","US","ShelveLoc"), Nvar=c("Sales","CompPrice","Income"), stat = c('Count','Prop','mean','sum','PS'), gpby = TRUE,filt="All %ni% c(888,999)")
	ExpCustomStat(Carseats,Cvar = c("Urban","US"), Nvar=c("Population","Sales","CompPrice"), stat = c('Count','Prop','mean','sum','var','min','max'), filt=c("ShelveLoc=='Good'^Urban=='Yes'^Price>=150"))


