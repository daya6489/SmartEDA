#' Summary statistics for numerical variables
#'
#'
#' @description Function provides summary statistics for all numerical variable. This function automatically scans through each variable and select only numeric/integer variables. Also if we know the target variable, function will generate relationship between target variable and each independent variable.
#' @usage ExpNumStat(data,by=NULL,gp=NULL,Qnt=NULL,Nlim=10,MesofShape=2,
#' Outlier=FALSE,round=3,dcast=FALSE,val=NULL)
##' @param data dataframe or matrix
##' @param by group by A (summary statistics by All), G (summary statistics by group), GA (summary statistics by group and Overall)
##' @param gp target variable if any, default NULL
##' @param Qnt default NULL. Specified quantiles [c(.25,0.75) will find 25th and 75th percentiles]
##' @param Nlim numeric variable limit (default value is 10 which means it will only consider those variable having more than 10 unique values and variable type is numeric/integer)
##' @param MesofShape Measures of shapes (Skewness and kurtosis).
##' @param Outlier Calculate the lower hinge, upper hinge and number of outliers
##' @param round round off
##' @param dcast fast dcast from data.table
##' @param val Name of the column whose values will be filled to cast (see Detials sections for list of column names)
##' @seealso \code{\link[psych:describe.by]{describe.by}}
##' @return summary statistics for numeric independent variables
##'
##'@details
##'
##' Summary by – overall
##'
##' Summary by – group (target variable)
##'
##' Summary by – overall and group (target variable)
##'
##' coloumn descriptions
##'
##' •	Vname – Variable name
##'
##' •	Group – Target variable
##'
##' •	TN – Total sample (inculded NA observations)
##'
##' •	nNeg – Total negative observations
##'
##' •	nZero – Total zero observations
##'
##' •	nPos – Total positive observations
##'
##' •	NegInf – Negative infinite count
##'
##' •	PosInf – Positive infinite count
##'
##' •	NA_value – Not Applicable count
##'
##' •	Per_of_Missing – Percentage of missings
##'
##' •	Min – minimum value
##'
##' •	Max – maximum value
##'
##' •	Mean – average value
##'
##' •	Median – median value
##'
##' •	SD – Standard deviation
##'
##' •	CV – coefficient of variations (SD/mean)*100
##'
##' •	IQR – Inter quartile range
##'
##' •	Qnt – Specified quantiles
##'
##' •	MesofShape – Skewness and Kurtosis
##'
##' •	Outlier – Number of outliers
##'
##' •	Cor – Correlation b/w target and independent variables
##'
##' @examples
#' ## Descriptive summary of numeric variables - Summary by Target variables
#' ExpNumStat(mtcars,by="G",gp="gear",Qnt=c(0.1,0.2),MesofShape=2,
#' Outlier=TRUE,round=3)
#' ## Descriptive summary of numeric variables - Summary by Overall
#' ExpNumStat(mtcars,by="A",gp="gear",Qnt=c(0.1,0.2),MesofShape=2,
#' Outlier=TRUE,round=3)
#' ## Descriptive summary of numeric variables - Summary by Overall and Group
#' ExpNumStat(mtcars,by="GA",gp="gear",Qnt=seq(0,1,.1),MesofShape=1,
#' Outlier=TRUE,round=2)
#' ## Summary by specific statistics for all numeric variables
#' ExpNumStat(mtcars,by="GA",gp="gear",Qnt=c(0.1,0.2),MesofShape=2,
#' Outlier=FALSE,round=2,dcast = TRUE,val = "IQR")
##' @author dubrangala
##' @importFrom stats quantile median IQR var reorder sd cor
##' @export ExpNumStat



ExpNumStat = function(data,by=NULL,gp=NULL,Qnt=NULL,Nlim=10,MesofShape=2,Outlier=FALSE,round=3,dcast=FALSE,val=NULL) {
  ds_fun <- function(x,r){
    BasDST <- c(TN = length(x),
                nNeg = length(which(x < 0)),
                nZero = length(which(x == 0)),
                nPos = length(which(x > 0)),
                NegInf = length(which(x==-Inf)),
                PosInf = length(which(x==Inf)),
                NA_Value = length(x[is.na(x)]),
                Per_of_Missing = round((length(x[is.na(x)])/length(x))*100,r),
                sum = round(sum(x,na.rm = T),r),
                min = round(min(x,na.rm = T),r),
                max = round(max(x,na.rm = T),r),
                mean = round(mean(x,na.rm = T),r),
                median = round(median(x,na.rm = T),r),
                SD = round(sd(x,na.rm = T),r),
                CV = round(sd(x,na.rm = T)/mean(x,na.rm = T),r),
                IQR = round(IQR(x,na.rm = T),r))
    Skw_kurt <- c(
      Skweness = round(ExpSkew(x,type="moment"),r),
      Kurtosis = round(ExpKurtosis(x,type="excess"),r)
      # MesofShape= ""
    )
    Out_rp <- c(
      `LB` = round(quantile(x,0.25,na.rm=TRUE)-(1.5*IQR(x,na.rm = T)),r),
      `UB` = round(quantile(x,0.75,na.rm=TRUE)+(1.5*IQR(x,na.rm = T)),r),
      nOutliers = length  (which(x > (quantile(x,0.75,na.rm=TRUE)+(1.5*IQR(x,na.rm = T))) | x <(quantile(x,0.25,na.rm=TRUE)-(1.5*IQR(x,na.rm = T))))))

    switch (MesofShape,

            {if(!is.null(Qnt) & Outlier==F)
            { qntil <- round(quantile(x,prob = Qnt,na.rm=T),r);
            vect_value=c(BasDST,qntil);
            return(vect_value)}
              else
                if(Outlier==T & !is.null(Qnt))
                {qntil <- round(quantile(x,prob = Qnt,na.rm=T),r);
                vect_value=c(BasDST,qntil,Out_rp);
                return(vect_value)}
              else
                if(Outlier==T & is.null(Qnt))
                {
                  vect_value=c(BasDST,Out_rp);
                  return(vect_value)}
              else
              { vect_value=c(BasDST);return(vect_value)}
            },
            {if(!is.null(Qnt) & Outlier==F)
            { qntil <- round(quantile(x,prob = Qnt,na.rm=T),r);
            vect_value=c(BasDST,Skw_kurt,qntil);
            return(vect_value)}
              else
                if(Outlier==T & !is.null(Qnt))
                {qntil <- round(quantile(x,prob = Qnt,na.rm=T),r);
                vect_value=c(BasDST,Skw_kurt,qntil,Out_rp);
                return(vect_value)}
              else
                if(Outlier==T & is.null(Qnt))
                {
                  vect_value=c(BasDST,Skw_kurt,Out_rp);
                  return(vect_value)}
              else
              { vect_value=c(BasDST,Skw_kurt);return(vect_value)}
            })
  } ## Ds_fun close
  options(scipen = 999)
  options(warn = -1)

  r <- round
  QCNT <- Qnt
  Out <- Outlier

  if (is.numeric(data)) {
    desc_sum =ds_fun(data,r=r)
    return(desc_sum)
  }

  if(!is.data.frame(data)) stop("'data must be a numeric vector or data.frame'")
  xx <- as.data.frame(data)

  if(!by %in% c("A","G","GA")) stop("'by label should be like A, G or GA'")
  if(!is.null(gp)) {
    grp = xx[,gp]
    if(is.numeric(grp)&length(unique(grp))>5) {by="corr"}
    else {by = by
    grp = unique(as.character(paste0(xx[,gp])))

    }
  }
  if(is.null(by)) {by ="A"; message("Default summary by all sample") }
  if(is.null(gp) & by %in% c("G","GA")) stop("'gp variable is missing for group level summary'")

  if(!MesofShape %in% c(1,2)) stop("'value of MesofShape should be either 1 or 2'")

  num_var = names(xx)[sapply(xx, is.numeric)]
  num_var <- num_var[sapply(xx[,num_var], function(x){length(unique(na.omit(x)))>=Nlim})]
  if (by=="A"){
    ccc = sapply(xx[,num_var], function(x) ds_fun(x,r=r),USE.NAMES = TRUE)
    cname = rownames(ccc)
    tb_op = data.frame(t(ccc))
    names(tb_op) <- cname
    varn_gp = cbind(Vname= rownames(tb_op),Group="All")
    rownames(tb_op)<-NULL
    tb_op = cbind(varn_gp,tb_op)
    tb_op <- tb_op[order(tb_op$Vname),]
    class(tb_op) = c("SmartEDA","ExpNumStat","data.frame")
    #return(tb_op)
  }
  else
    if (by=="corr"){
      message(paste0("Note: Target variable is continuous","\n","Summary statistics excluded group by statement","\n","Results generated with correlation value against target variable"))
      ccc = sapply(xx[,num_var], function(x) ds_fun(x,r=r),USE.NAMES = TRUE)
      cor = round(cor(xx[,num_var])[,gp],r)
      ccc = rbind(ccc,cor)
      cname = rownames(ccc)
      tb_op = data.frame(t(ccc))
      names(tb_op) <- cname
      varn_gp = cbind(Vname= rownames(tb_op),Group=gp,Note=paste0("Cor b/w ",gp))
      rownames(tb_op)<-NULL
      tb_op = cbind(varn_gp,tb_op)
      tb_op <- tb_op[order(tb_op$Vname),]
      class(tb_op) = c("SmartEDA","ExpNumStat","data.frame")
      #return(tb_op)
    }
  else
    if (by=="G") {
      tb_op = data.frame()
      for (j in grp){
        xx_1 = subset(xx,xx[,gp]==j);xx_1=xx_1[,num_var]
        ccc = sapply(xx_1, function(x) ds_fun(x,r=r),USE.NAMES = TRUE)
        tcc = data.frame(t(ccc))
        varn_gp = cbind(Vname= rownames(tcc),Group=paste0(gp,":",j))
        rownames(tcc)<-NULL
        tb_op1 = cbind(varn_gp,tcc)
        cname = c("Vname","Group",rownames(ccc))
        names(tb_op1)<- cname
        tb_op =rbind(tb_op,tb_op1)

      }
      tb_op <- tb_op[order(tb_op$Vname),]
      class(tb_op) = c("SmartEDA","ExpNumStat","data.frame")
      #return(tb_op)
    } else
      if (by=="GA") {
        tb_op = data.frame()
        grp = c("All",grp)
        for (j in grp){
          if(j=="All") {xx_1=subset(xx,select = num_var)} else
          {xx_1 = subset(xx,xx[,gp]==j,select = num_var)}
          ccc = sapply(xx_1, function(x) ds_fun(x,r=r),USE.NAMES = TRUE)
          tcc = data.frame(t(ccc))
          varn_gp = cbind(Vname= rownames(tcc),Group=paste0(gp,":",j))
          rownames(tcc)<-NULL
          tb_op1 = cbind(varn_gp,tcc)
          cname = c("Vname","Group",rownames(ccc))
          names(tb_op1)<- cname
          tb_op =rbind(tb_op,tb_op1)
        }
        tb_op <- tb_op[order(tb_op$Vname),]
        names(tb_op) <- cname
        class(tb_op) = c("SmartEDA","ExpNumStat","data.frame")
        #return(tb_op)
      }

  if(isTRUE(dcast)){
    cf<-formula(paste("Vname","Group",sep="~"))
    cp=data.frame(Stat=val,dcast(tb_op,cf,value.var=val))
    return(cp)
  } else {return(tb_op)}

}

#' Measures of Shape - Skewness
#'
#' @description Measures of shape to give a detailed evaluation of data. Explains the amount and direction of skew. Kurotsis explains how tall and sharp the central peak is. Skewness has no units: but a number, like a z score
#' @usage ExpSkew(x,type)
##' @param x A numeric object or data.frame
##' @param type a character which specifies the method of computation. Options are "moment" or "sample"
##' @return ExpSkew returns Skewness values
##' @examples
#' ExpSkew(mtcars,type="moment")
#' ExpSkew(mtcars,type="sample")
##' @author dubrangala
##' @export

ExpSkew = function(x,type){

  skew_fun = function(x){
    x <- x[!is.na(x)]
    xbar <- mean(x)
    sdx <- sd(x)
    n <- length(x)
    m2 <- sum((x - xbar)^2)/n #is the variance of the data set
    m3 <- sum((x - xbar)^3)/n #is third moment of the data set
    skw_out =list(moment = (m3)/(m2^(3/2)),
                  sample = sum((x - xbar)^3/sdx^3) * n/((n - 1) * (n - 2)))
    return(skw_out)
  }

  if (!is.data.frame(x))
  {
    if (!is.numeric(x))
      stop("The selected variable is not numeric")
    skw_op <- skew_fun(x)
    switch(type,
           moment = as.numeric(skw_op[1]),
           sample = as.numeric(skw_op[2]))
  } else
  {
    xx = as.data.frame(x)
    num_var = names(xx)[sapply(xx, is.numeric)]
    if(is.null(num_var))
      stop("There is no numeric object found in data frame")
    num_var <- num_var[sapply(xx[,num_var], function(x){length(unique(na.omit(x)))>3})]
    op_ut = data.frame(sapply(xx[,num_var], function(x)skew_fun(x)))

    switch (type,
            moment = op_ut[1,],
            sample = op_ut[2,])

  }
}


#' Measures of Shape - Kurtosis
#'
#' @description Measures of shape to give a detailed evaluation of data. Explains the amount and direction of skew. Kurotsis explains how tall and sharp the central peak is. Skewness has no units: but a number, like a z score
#' @usage ExpKurtosis(x,type)
##' @param x A numeric object or data.frame
##' @param type a character which specifies the method of computation. Options are "moment" or "excess"
##' @return ExpKurtosis returns Kurtosis values
##' @examples
#' ExpKurtosis(mtcars$hp,type="excess")
#' ExpKurtosis(mtcars$carb,type="moment")
#' ExpKurtosis(mtcars,type="excess")
##' @author dubrangala
##' @export

ExpKurtosis = function(x,type){

  Kurt_fun = function(x){
    x <- x[!is.na(x)]
    xbar <- mean(x)
    n <- length(x)
    m2 <- sum((x - xbar)^2)/n #is the variance of the data set
    m4 <- sum((x - xbar)^4)/n #is fourth moment of the data set
    a4 <- round(m4/m2^2,3)
    g2 <- round(a4-3,3)
    kurt_op = list(mom=a4,exc=g2)
    return(kurt_op)}

  if (!is.data.frame(x))
  {
    if (!is.numeric(x))
      stop("The selected variable is not numeric")
    skw_op <-  Kurt_fun(x)

    switch(type,
           excess = as.numeric(skw_op[2]),
           moment = as.numeric(skw_op[1]))
  } else

  {
    xx = as.data.frame(x)
    num_var = names(xx)[sapply(xx, is.numeric)]
    if(is.null(num_var))
      stop("There is no numeric object found in data frame")
    num_var <- num_var[sapply(xx[,num_var], function(x){length(unique(na.omit(x)))>3})]
    op_ut = data.frame(sapply(xx[,num_var], function(x)Kurt_fun(x)))

    switch (type,
            excess = op_ut[2,],
            moment = op_ut[1,])
  }

}
