## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------------
library(rmarkdown)
library(SmartEDA)
library(knitr)
library(ISLR)
library(scales)
library(gridExtra)
library(ggplot2)


## ---------------------------------------------------------------------------------------------------------------------------------------------------
target = "gear"
categorical_features <- c("vs", "carb") # we can add as many categorical variables
numeircal_features <- c("mpg", "qsec") # we can add as many numerical variables

## ----c11 ,warning=FALSE,eval=T,include=T,fig.align='center',fig.height=7,fig.width=7----------------------------------------------------------------
num_1 <- ExpTwoPlots(mtcars, 
                     plot_type = "numeric",
                     iv_variables = numeircal_features,
                     target = NULL,
                     lp_arg_list = list(fill="orange"),
                     lp_geom_type = 'boxplot',
                     rp_arg_list = list(alpha=0.5, fill="white", color = "red", binwidth=1),
                     rp_geom_type = 'histogram',
                     page = c(2,1),
                     theme = "Default")
num_1

## ----c12 ,warning=FALSE,eval=T,include=T,fig.align='center',fig.height=7,fig.width=7----------------------------------------------------------------
num_2 <- ExpTwoPlots(mtcars, 
                     plot_type = "numeric",
                     iv_variables = numeircal_features,
                     target = NULL,
                     lp_arg_list = list(fill = "white",color = "red",  binwidth=1),
                     lp_geom_type = 'histogram',
                     rp_arg_list = list(alpha=0.5, fill="red"),
                     rp_geom_type = 'density',
                     page = c(2,1),
                     theme = "Default")
num_2

## ----c13 ,warning=FALSE,eval=T,include=T,fig.align='center',fig.height=7,fig.width=7----------------------------------------------------------------
num_3 <- ExpTwoPlots(mtcars, 
                     plot_type = "numeric",
                     iv_variables = numeircal_features,
                     target = NULL,
                     lp_arg_list = list(color = "red"),
                     lp_geom_type = 'density',
                     rp_arg_list = list(fill="orange"),
                     rp_geom_type = 'boxplot',
                     page = c(2,1),
                     theme = "Default")
num_3

## ----c14 ,warning=FALSE,eval=T,include=T,fig.align='center',fig.height=7,fig.width=7----------------------------------------------------------------
num_4 <- ExpTwoPlots(mtcars, 
                     plot_type = "numeric",
                     iv_variables = numeircal_features,
                     target = NULL,
                     lp_arg_list = list(fill = "blue"),
                     lp_geom_type = 'qqplot',
                     rp_arg_list = list(fill="orange"),
                     rp_geom_type = 'boxplot',
                     page = c(2,1),
                     theme = "Default")
num_4

## ----c21 ,warning=FALSE,eval=T,include=T,fig.align='center',fig.height=7,fig.width=7----------------------------------------------------------------
num_21 <- ExpTwoPlots(mtcars, 
                     plot_type = "numeric",
                     iv_variables = numeircal_features,
                     target = "gear",
                     lp_arg_list = list(fill="pink"),
                     lp_geom_type = 'boxplot',
                     rp_arg_list = list(alpha=0.5, fill = c("grey", "orange", "lightblue"), binwidth=1),
                     rp_geom_type = 'histogram',
                     page = c(2,1),
                     theme = "Default")
num_21

## ----c22 ,warning=FALSE,eval=T,include=T,fig.align='center',fig.height=7,fig.width=7----------------------------------------------------------------
num_22 <- ExpTwoPlots(mtcars, 
                     plot_type = "numeric",
                     iv_variables = numeircal_features,
                     target = "gear",
                     lp_arg_list = list(fill = "white",color = "red",  binwidth=1),
                     lp_geom_type = 'histogram',
                     rp_arg_list = list(alpha=0.5, fill = c("red", "orange", "pink")),
                     rp_geom_type = 'density',
                     page = c(2,1),
                     theme = "Default")
num_22

## ----c23 ,warning=FALSE,eval=T,include=T,fig.align='center',fig.height=7,fig.width=7----------------------------------------------------------------
num_23 <- ExpTwoPlots(mtcars, 
                     plot_type = "numeric",
                     iv_variables = numeircal_features,
                     target = "gear",
                     lp_arg_list = list(fill = "grey"),
                     lp_geom_type = 'density',
                     rp_arg_list = list(fill = c("blue", "orange", "pink"), alpha=0.5),
                     rp_geom_type = 'boxplot',
                     page = c(2,1),
                     theme = "Default")
num_23

## ----c24 ,warning=FALSE,eval=T,include=T,fig.align='center',fig.height=7,fig.width=7----------------------------------------------------------------
num_24 <- ExpTwoPlots(mtcars, 
                     plot_type = "numeric",
                     iv_variables = numeircal_features,
                     target = "gear",
                     lp_arg_list = list(fill = "grey"),
                     lp_geom_type = 'qqplot',
                     rp_arg_list = list(fill = c("blue", "orange", "pink"), alpha=0.5),
                     rp_geom_type = 'density',
                     page = c(2,1),
                     theme = "Default")
num_24

## ----c25 ,warning=FALSE,eval=T,include=T,fig.align='center',fig.height=7,fig.width=7----------------------------------------------------------------
num_25 <- ExpTwoPlots(mtcars, 
                     plot_type = "numeric",
                     iv_variables = numeircal_features,
                     target = "gear",
                     lp_arg_list = list(fill = "orange"),
                     lp_geom_type = 'boxplot',
                     rp_arg_list = list(fill = c("blue", "green", "red"), alpha=0.5),
                     rp_geom_type = 'qqplot',
                     page = c(2,1),
                     theme = "Default")
num_25

## ----c31 ,warning=FALSE,eval=T,include=T,fig.align='center',fig.height=7,fig.width=7----------------------------------------------------------------
cat_1 <- ExpTwoPlots(mtcars, 
                     plot_type = "categorical",
                     iv_variables = categorical_features,
                     target = NULL,
                     lp_arg_list = list(),
                     lp_geom_type = 'donut',
                     rp_arg_list = list(stat = 'identity'),
                     rp_geom_type = 'bar',
                     page = c(2,1),
                     theme = "Default")
cat_1

## ----c32 ,warning=FALSE,eval=T,include=T,fig.align='center',fig.height=7,fig.width=7----------------------------------------------------------------
cat_2 <- ExpTwoPlots(mtcars, 
                     plot_type = "categorical",
                     iv_variables = categorical_features,
                     target = NULL,
                     lp_arg_list = list(),
                     lp_geom_type = 'donut',
                     rp_arg_list = list(),
                     rp_geom_type = 'pie',
                     page = c(2,1),
                     theme = "Default")
cat_2

## ----c33 ,warning=FALSE,eval=T,include=T,fig.align='center',fig.height=7,fig.width=7----------------------------------------------------------------
cat_3 <- ExpTwoPlots(mtcars, 
                     plot_type = "categorical",
                     iv_variables = categorical_features,
                     target = NULL,
                     lp_arg_list = list(stat = 'identity'),
                     lp_geom_type = 'barh',
                     rp_arg_list = list(),
                     rp_geom_type = 'pie',
                     page = c(2,1),
                     theme = "Default")
cat_3

## ----c41 ,warning=FALSE,eval=T,include=T,fig.align='center',fig.height=7,fig.width=7----------------------------------------------------------------
cat_41 <- ExpTwoPlots(mtcars, 
                     plot_type = "categorical",
                     iv_variables = categorical_features,
                     target = 'gear',
                     lp_arg_list = list(),
                     lp_geom_type = 'donut',
                     rp_arg_list = list(stat = 'identity'),
                     rp_geom_type = 'bar',
                     page = c(2,1),
                     theme = "Default")
cat_41

## ----c42 ,warning=FALSE,eval=T,include=T,fig.align='center',fig.height=7,fig.width=7----------------------------------------------------------------
cat_42 <- ExpTwoPlots(mtcars, 
                     plot_type = "categorical",
                     iv_variables = categorical_features,
                     target = 'gear',
                     lp_arg_list = list(),
                     lp_geom_type = 'pie',
                     rp_arg_list = list(stat = 'identity'),
                     rp_geom_type = 'barh',
                     page = c(2,1),
                     theme = "Default")
cat_42

