# SmartEDA [![CRAN status](https://www.r-pkg.org/badges/version/SmartEDA)](https://cran.r-project.org/package=SmartEDA)

[![Downloads](http://cranlogs.r-pkg.org/badges/SmartEDA)](https://cran.r-project.org/package=SmartEDA)
[![Total Downloads](http://cranlogs.r-pkg.org/badges/grand-total/SmartEDA)](https://cran.r-project.org/package=SmartEDA)

**Authors:** [Dayanand Ubrangala](https://www.researchgate.net/profile/Dayananda_Ubrangala), [Kiran R](https://www.researchgate.net/profile/Kiran_Rama2), Ravi Prasad Kondapalli and [Sayan Putatunda](https://www.researchgate.net/profile/Sayan_Putatunda)

-----

## Background
In a quality statistical data analysis the initial step has to be exploratory. Exploratory data 
analysis begins with the univariate exploratory analyis - examining the variable one at a time. 
Next comes bivariate analysis followed by multivariate analyis. SmartEDA package helps in getting 
the complete exploratory data analysis just by running the function instead of writing lengthy r code.

-----

## Functionalities of SmartEDA

The SmartEDA R package has four unique functionalities as

* Descriptive statistics
* Data visualisation
* Custom table
* HTML EDA report

![SmartEDA](https://github.com/daya6489/SmartEDA/blob/master/man/figures/smarteda_funtions.PNG)

-----

## Journal of Open Source Software Article
An article describing SmartEDA pacakge for exploratory data analysis approach has been published in [arxiv](https://arxiv.org/pdf/1903.04754.pdf) and currently it is under review at The Journal of Open 
Source Software. Please cite the paper if you use SmartEDA in your work!

-----

## Installation

The package can be installed directly from CRAN.

```R
install.packages("SmartEDA")
```

To contribute, download the latest development version of [SmartEDA](https://github.com/daya6489/SmartEDA) 
from GitHub via devtools:
	
```R
install.packages("devtools")
devtools::install_github("daya6489/SmartEDA",ref = "develop")
```

-----

## Example

### Data
In this vignette, we will be using a simulated data set containing sales of child car seats at 400 different 
stores. 

Data Source [ISLR package](https://www.rdocumentation.org/packages/ISLR/versions/1.2/topics/Carseats).

Install the package "ISLR" to get the example data set.

```R
	install.packages("ISLR")
	library("ISLR")
	install.packages("SmartEDA")
	library("SmartEDA")
	## Load sample dataset from ISLR pacakge
	Carseats= ISLR::Carseats
```

### Overview of the data
Understanding the dimensions of the dataset, variable names, overall missing summary and data types of each variables

```R
## overview of the data; 
	ExpData(data=Carseats,type=1)
## structure of the data	
	ExpData(data=Carseats,type=2)
```

### Summary of numerical variables
To summarise the numeric variables, you can use following r codes from this pacakge

```R
## Summary statistics by – overall
	ExpNumStat(Carseats,by="A",gp=NULL,Qnt=seq(0,1,0.1),MesofShape=2,Outlier=TRUE,round=2)
## Summary statistics by – overall with correlation	
	ExpNumStat(Carseats,by="A",gp="Price",Qnt=seq(0,1,0.1),MesofShape=1,Outlier=TRUE,round=2)
## Summary statistics by – category
	ExpNumStat(Carseats,by="GA",gp="Urban",Qnt=seq(0,1,0.1),MesofShape=2,Outlier=TRUE,round=2)
```

### Graphical representation of all numeric features

```R
## Generate Boxplot by category
ExpNumViz(mtcars,target="gear",type=2,nlim=25,fname = file.path(tempdir(),"Mtcars2"),Page = c(2,2))
## Generate Density plot
ExpNumViz(mtcars,target=NULL,type=3,nlim=25,fname = file.path(tempdir(),"Mtcars3"),Page = c(2,2))
## Generate Scatter plot
ExpNumViz(mtcars,target="carb",type=3,nlim=25,fname = file.path(tempdir(),"Mtcars4"),Page = c(2,2))
ExpNumViz(mtcars,target="am",scatter=TRUE)
```

### Summary of Categorical variables	

```R
## Frequency or custom tables for categorical variables
	ExpCTable(Carseats,Target=NULL,margin=1,clim=10,nlim=5,round=2,bin=NULL,per=T)
	ExpCTable(Carseats,Target="Price",margin=1,clim=10,nlim=NULL,round=2,bin=4,per=F)
## Summary statistics of categorical variables
	ExpCatStat(Carseats,Target="Urban",result = "Stat",clim=10,nlim=5,Pclass="Yes")
## Inforamtion value and Odds value
	ExpCatStat(Carseats,Target="Urban",result = "IV",clim=10,nlim=5,Pclass="Yes")
```

### Graphical representation of all categorical variables

```R
## column chart
	ExpCatViz(Carseats,target="Urban",fname=NULL,clim=10,col=NULL,margin=2,Page = c(2,1),sample=2)
## Stacked bar graph
	ExpCatViz(Carseats,target="Urban",fname=NULL,clim=10,col=NULL,margin=2,Page = c(2,1),sample=2)
## Variable importance graph using information values
  ExpCatStat(Carseats,Target="Urban",result="Stat",Pclass="Yes",plot=TURE,top=20,Round=2)
```
### Variable importance based on Information value

```R
  ExpCatStat(Carseats,Target="Urban",result = "Stat",clim=10,nlim=5,bins=10,Pclass="Yes",plot=TRUE,top=10,
  Round=2)
```

### Create HTML EDA report
Create a exploratory data analysis report in HTML format

```R
	ExpReport(Carseats,Target="Urban",label=NULL,theme="Default",op_file="test.html",op_dir=getwd(),sc=2,
	sn=2,Rc="Yes")
```

### Quantile-quantile plot for numeric variables

```R
	ExpOutQQ(CData,nlim=10,fname=NULL,Page=c(2,2),sample=4)
```

### Parallel Co-ordinate plots

```R
## Defualt ExpParcoord funciton
	ExpParcoord(CData,Group=NULL,Stsize=NULL,Nvar=c("Price","Income","Advertising","Population","Age",
	"Education"))
## With Stratified rows and selected columns only
  ExpParcoord(CData,Group="ShelveLoc",Stsize=c(10,15,20),Nvar=c("Price","Income"),Cvar=c("Urban","US"))
## Without stratification
  ExpParcoord(CData,Group="ShelveLoc",Nvar=c("Price","Income"),Cvar=c("Urban","US"),scale=NULL)
```


### Exploratory analysis - Custom tables, summary statistics
Descriptive summary on all input variables for each level/combination of group variable. Also while 
running the analysis we can filter row/cases of the data. 

```R
	ExpCustomStat(Carseats,Cvar=c("US","Urban","ShelveLoc"),gpby=FALSE)
	ExpCustomStat(Carseats,Cvar=c("US","Urban"),gpby=TRUE,filt=NULL)
	ExpCustomStat(Carseats,Cvar=c("US","Urban","ShelveLoc"),gpby=TRUE,filt=NULL)
	ExpCustomStat(Carseats,Cvar=c("US","Urban"),gpby=TRUE,filt="Population>150")
	ExpCustomStat(Carseats,Cvar=c("US","ShelveLoc"),gpby=TRUE,filt="Urban=='Yes' & Population>150")
```

-----

## Issues

  - Need some help? 
  - Found a bug? 
  - Request a new feature?
Just open an [issue](https://github.com/daya6489/SmartEDA/issues).

-----

## Contributions

  - Want to add a feature? 
  - Correct a bug? 
You're more than welcome to contribute

Please read the [contribution guidelines](https://github.com/daya6489/SmartEDA/blob/master/CONTRIBUTING.md)
prior to submitting a pull request. Try to code and submit a new pull request (PR). Even if not perfect, 
we will help you to make a great PR

-----

## Articles

See [article wiki page](https://github.com/daya6489/SmartEDA/wiki/Articles).

## References

<div id="refs" class="references">

<div id="ref-chon2010">

Chon Ho, Y. (2010). Exploratory data analysis in the context of data mining and resampling.
International Journal of Psychological Research, 3(1), 9–22. doi:<https://doi.org/10.21500/20112084.819>

</div>

<div id="ref-coates2016">

Coates, M. (2016). exploreR: Tools for Quickly Exploring Data. Retrieved from <https://CRAN.R-project.org/package=exploreR>

</div>

<div id="ref-comtois2018">

Comtois, D. (2018). summarytools: *Tools to Quickly and Neatly Summarize Data*. Retrieved from <https://CRAN.R-project.org/package=summarytools>

</div>

<div id="ref-cui2018">

Cui, B. (2018). DataExplorer: *Data Explorer*. Retrieved from <https://CRAN.Rproject.org/package=DataExplorer>

</div>

<div id="ref-diCerbo2015">

DiCerbo et al. (2015). *Serious Games Analytics. Advances in Game-Based Learning*. In C. Loh, Y. Sheng, & D. Ifenthaler (Eds.),. Cham: Springer. doi:10.1007/978-3-319-05834-4

</div>

<div id="ref-harrell2018">

Harrell et al. (2018). Hmisc: *Harrell Miscellaneous*, Retrieved from <https://CRAN.Rproject.org/package=Hmisc>

</div>

<div id="ref-hoaglin1983">

Hoaglin, D., Mosteller, F., & Tukey, J. (1983). *Understanding robust and exploratory data analysis. Wiley Series in probability and mathematical statistics*, New-York.

</div>

<div id="ref-jaggi2013">

Jaggi, S. (2013). *Descriptive statistics and exploratory data analysis. Indian Agricultural Statistics Research Institute*. Retrieved from <http://www.iasri.res.in/ebook/EB_SMAR/ebook_pdf%20files/Manual%20II/1-Descriptive%20Statistics.pdf>

</div>

<div id="ref-james2017">

James, G., Witten, D., Hastie, T., & Tibshirani, R. (2017). ISLR: *Data for an Introduction to Statistical Learning with Applications in R*. doi:<https://doi.org/10.1007/978-1-4614-7138-7_1>

</div>

<div id="ref-konopka2018">

Konopka et al. (2018). *Exploratory data analysis of a clinical study group: Development of a procedure for exploring multidimensional data*. PLoS ONE, 13(8).

</div>

<div id="ref-liu2014">

Liu, Q. (2014, October). The Application of Exploratory Data Analysis in Auditing (PhD thesis). Newark Rutgers, The State University of New Jersey, Newark, New Jersey.

</div>

<div id="ref-maet2017">

Ma, X., Hummer, D., Golden, J. J., Fox, P. A., Hazen, R. M., Morrison, S. M., Downs, R.T., et al. (2017). *Using Visual Exploratory Data Analysis to Facilitate Collaboration and Hypothesis Generation in Cross-Disciplinary Research*. International Journal of Geo-Information, 6(368), 1–11. doi:<https://doi.org/10.3390/ijgi6110368>

</div>

<div id="ref-nair2018">

Nair, A. (2018). RtutoR: *Shiny Apps for Plotting and Exploratory Analysis*. Retrieved from <https://CRAN.R-project.org/package=RtutoR>

</div>

<div id="ref-ryu2018">

Ryu, C. (2018). dlookr: *Tools for Data Diagnosis, Exploration, Transformation*. Retrieved from <https://CRAN.R-project.org/package=dlookr>

</div>

<div id="ref-tukey1977">

Tukey, J. W. (1977). Exploratory Data Analysis. Addison-Wesley.

</div>

<div id="ref-seda2018">

Ubrangala, D., Rama, K., Kondapalli, R. P., & Putatunda, S. (2018). *SmartEDA: Summarize and Explore the Data*. Retrieved from <https://CRAN.R-project.org/package=SmartEDA>

</div>
