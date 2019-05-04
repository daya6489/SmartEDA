#' Customized summary statistics
#'
#'
#' @description Table of descriptive statistics. Output returns matrix object containing descriptive information on all input variables for each level or combination of levels in categorical/group variable. Also while running the analysis user can filter out the data by individual variable level or across data level.
#' @usage ExpCustomStat(data,Cvar=NULL,Nvar=NULL,stat=NULL,gpby=TRUE,filt=NULL,dcast=FALSE,
#' value=NULL)
##' @param data dataframe or Matrix
##' @param Cvar qualitative variables on which to stratify / subgroup or run categorical summaries
##' @param Nvar quantitative variables on which to run summary statistics for.
##' @param stat descriptive statistics. Sepecify which summary statistics required (Included all base stat functions like 'mean','medain','max','min','sum','IQR','sd','var',quantile like P0.1, P0.2 etc'). Also added two more stat here are 'PS' - percentage of shares and 'Prop' - column percentage
##' @param gpby default value is True. Group level summary will be created based on list of categorical variable. If summary required at each categorical variable level then keep this option as FALSE
##' @param filt filter out data while running the summary statistics. Filter can apply accross data or individual variable level using filt option. If there are multiple filters, seperate the conditons by using '^'. Ex: Nvar = c("X1","X2","X3","X4"), let say we need to exclude data X1>900 for X1 variable, X2==10 for X2 variable, Gender !='Male' for X3 variable and all data for X4 then filt should be, filt = c("X1>900"^"X2==10"^"Gender!='Male'"^all) or  c("X1>900"^"X2==10"^"Gender!='Male'"^ ^). in case if you want to keep all data for some of the variable listed in Nvar, then specify inside the filt like ^all^ or ^ ^(single space)
##' @param dcast fast dcast from data.table
##' @param value If dcast is TRUE, pass the variable name which needs to come on column
##' @return
##' summary statistics as dataframe. Usage of this function is detailed in user guide vignettes document.
##'@details
##'Filter unique value from all the numeric variables
##'
##'This will be usefull when we need to exclude some unique imputed or outliers values like '999' or '9999' or '-9' or '-1111', or '888' etc from each selected variables.
##'
##'Eg:dat = data.frame(x = c(23,24,34,999,12,12,23,999,45),
##'                  y = c(1,3,4,999,0,999,0,8,999,0)
##'
##'Exclude 999:
##'x = c(23,24,34,12,12,23,45)
##'y = c(1,3,4,0,0,8,0)
##'
##'The complete functionality of `ExpCustomStat` function is detailed in vignette help page with example code.
##'@examples
##'ExpCustomStat(mtcars,Cvar=c("vs","am","gear"),Nvar=c("disp","mpg"),
##'stat=c("Count","sum","PS"),gpby=TRUE,filt =NULL)
##' @export ExpCustomStat
##'@import data.table
##'@importFrom data.table dcast .N
##'@importFrom stats formula

ExpCustomStat = function(data,Cvar=NULL,Nvar=NULL,stat=NULL,gpby=TRUE,filt=NULL,dcast=FALSE,value=NULL) {
  options(warn = -1)
  '%ni%' <- Negate('%in%')
  `:=` <- NULL
  `.` <- NULL
  Overall <- NULL
  TP <- NULL
  Group_by <- NULL
  Attribute <- NULL
  Nvartp <- 0
  if(is.null(Cvar)&is.null(Nvar)) stop("Input variable is missing")
  if(!is.null(Cvar)&!is.null(Nvar)&is.null(stat)) stop("'stat' value is NULL, Specify the required statistics")
  # options(warn = -1)
  ## Group by
  if(is.null(Cvar)) {Cv="Overall"
  setDT(data)
  data[,Overall:="All"]
  } else {Cv=Cvar
  setDT(data)}

  if(is.null(Nvar)) {Nvar<-"TP"
  data[,TP:=0]
  if(is.null(stat)) {stat=c("Count","Prop")} else {stat=stat}
  Nvartp <- 1
  }
  else
  {
    num_var = Nvar[sapply(data[,Nvar,with=F],is.numeric)]
    not_num <- Nvar[Nvar%ni%num_var]
    if(length(not_num)>=1)  cat(paste0(not_num,collapse=", "),"variable/s not in numeric type", "\n","Either convert it into numeric or remove that from 'Nvar' list",'\n\n')
    if(length(not_num)==1 & length(Nvar)==1)  stop(cat(not_num,"is not numeric \n"))
    Nvar=Nvar}

  if (isTRUE(gpby)) {
    Final_data <- NULL
    fk = 0L
    for(k in Nvar) {
      if(sapply(data[,k,with=F],is.numeric)==F) next
      fk=fk+1L
      if (is.null(filt)) {
        data1=data;filter="NA"}
      else {
        #cc1 <- unlist(stri_split_fixed(filt,pattern ="^",omit_empty=TRUE,simplify=TRUE))
        cc1 <- unlist(strsplit(filt,"^",fixed=TRUE))
        ccd =trimws(cc1)
        if (length(ccd)==1) {
          allft = substr(ccd,1,3)
          if(tolower(allft)=="all") {
            allft1 = trimws(gsub("All","",ccd,ignore.case = T))
            filter=as.character(paste0(k,allft1))} else
              filter=as.character(ccd)}
        else {
          # if(k==substr(ccd[fk],1,nchar(k))) {filter = (ccd[fk])} else stop("Filter variable name is not matching \n correct the logic")
          filter = (ccd[fk])
        }
        # attach(data,warn.conflicts = F)
        # assign(k,data[,get(k)])
        # data1 <- data[eval(parse(text = filter)),]

        if(tolower(filter)==""|tolower(filter)=="all"){
          data1=data
        } else {
          data1 <- data[with(data,eval(parse(text = filter))),]}

        # detach(data)
      }
      nn=0L
      summatab <- NULL
      for (j in stat) {
        nn=1L+nn
        cp = j
        qnt = as.numeric(gsub("P","",j,ignore.case = T))
        if (tolower(j) %in% paste0("p",seq(0,1,0.01)))
        { sum1 = data1[,.(cnt=quantile(get(k),probs = qnt,na.rm=T)),by=Cv]} else

          if (tolower(cp)=="count") {
            sum1 = data1[,.(cnt=.N),by=Cv]
          } else
            if (tolower(cp)=="ps") {
              Tot =data1[,.(sum(get(k),na.rm=T))]
              sum1 = data1[,.(cnt=round((sum(get(k),na.rm=T)/Tot$V1)*100,2)),by=Cv] }
        else
          if (tolower(cp)=="prop") {
            TN =nrow(data1)
            sum1 = data1[,.(cnt=round(.N/TN*100,2)),by=Cv] }
        else
        {
          sum1 = data1[,.(cnt=get(cp)(get(k),na.rm=T)),by=Cv]
        }
        setnames(sum1,"cnt",cp)
        if (nn==1) {summatab = sum1} else
        {
          nc = ncol(sum1)
          summatab=cbind(summatab,sum1[,nc,with=F])
        }
        if (nn==length(stat)) {summatab[,Attribute:=k];summatab[,Filter:=filter] }
      }
      Final_data = rbind(Final_data,summatab)
      rm(summatab,sum1,nn)
    }
    if(is.null(filt)) {Final_data[,Filter:=NULL]}
    if(Nvartp>0) {Final_data[,Attribute:=NULL]}
    if(is.null(Cvar)) {Final_data[,Overall:=NULL]}
    nam1 = stat
    nam2 = names(Final_data)[names(Final_data)%ni%nam1]
    Final_data <- Final_data[,c(nam2,nam1),with=F]
    if(isTRUE(dcast))
    { if(is.null(Cvar)) stop("if dcast option is TRUE then 'Cvar' should not be empty")
      if(!is.null(value) & (is.null(Nvar)|length(Nvar)==1)){
        #if(is.null(Cvar)) stop("if dcast option is TRUE then 'Cvar' should not be empty")
        colun_nam = value
        Rownam = Cvar[!(Cvar%in%value)]

        cf<-formula(paste(paste0(colun_nam,collapse = "+"),paste0(Rownam,collapse = "+"),sep="~"))

        # Final_data[,(Cvar):=lapply(Cvar,function(x) paste(x,Final_data[,get(x)],sep=":"))]
        cp=dcast(Final_data,cf,value.var=stat)
        cat(paste0("Row value   : ",paste0(colun_nam,collapse = " + "),"\n", "Column value: ",paste0(Rownam,collapse = " + "),"\n","Statistics  : ",paste0(stat,collapse = " + "),"\n","\n"))
        return(cp)
      } else
        if(length(Nvar)>=1)
        {
          colun_nam <- "Attribute"
          Rownam <- Cvar
          cf<-formula(paste(paste0(colun_nam,collapse = "+"),paste0(Rownam,collapse = "+"),sep="~"))
          cp=dcast(Final_data,cf,value.var=stat)
          cat(paste0("Row value   : ",paste0(colun_nam,collapse = " + "),"\n", "Column value: ",paste0(Rownam,collapse = " + "),"\n","Statistics  : ",paste0(stat,collapse = " + "),"\n","\n"))
          return(cp)

        } else

          if(!(is.null(Cvar)) & is.null(Nvar)) {print("Input error")} else {print('No reshape for this input, correct the input')}
    }

    else
    {
      return(Final_data)
    }

  }
  else ## Without Group
  {
    if(is.null(Cvar)) stop("Group variable is missing")
    Final_data1 <- NULL
    for(g in Cv) {
      Final_data <- NULL
      fk = 0L
      for(k in Nvar) {
        if(sapply(data[,k,with=F],is.numeric)==F) next

        fk=fk+1L
        if (is.null(filt)) {
          data1=data;filter="NA"
        }
        else
        {
          #cc1 <- unlist(stri_split_fixed(filt,pattern ="^",omit_empty=TRUE,simplify=TRUE))
          cc1 <- unlist(strsplit(filt,"^",fixed=TRUE))
          ccd =trimws(cc1)
          if (length(ccd)==1) {
            allft = substr(ccd,1,3)
            if(tolower(allft)=="all") {
              allft1 = trimws(gsub("All","",ccd,ignore.case = T))
              filter=as.character(paste0(k,allft1))
            } else
              filter=as.character(ccd)
          }
          else
          {
            # if(k==substr(ccd[fk],1,nchar(k))) {filter = (ccd[fk])} else stop("Filter variable name is not matching \n correct the logic")
            filter = (ccd[fk])
          }
          # attach(data,warn.conflicts = F)
          # assign(k,data[,get(k)])
          # data1 <- data[eval(parse(text = filter)),]
          if(tolower(filter)==""|tolower(filter)=="all"){
            data1=data
          } else {
            data1 <- data[with(data,eval(parse(text = filter))),]}
          # detach(data)
        }
        nn=0L
        summatab <- NULL
        for (j in stat) {
          nn=nn+1L
          cp = j
          qnt = as.numeric(gsub("P","",j,ignore.case = T))
          if (tolower(j) %in% paste0("p",seq(0,1,0.01)))
          { sum1 = data1[,.(cnt=quantile(get(k),probs = qnt,na.rm=T)),by=g]} else
            if (tolower(cp)=="count") {
              sum1 = data1[,.(cnt=.N),by=g]
            } else
              if (tolower(cp)=="ps") {
                Tot =data1[,.(sum(get(k),na.rm=T))]
                sum1 = data1[,.(cnt=round((sum(get(k),na.rm=T)/Tot$V1)*100,2)),by=g] }
          else
            if (tolower(cp)=="prop") {
              TN =nrow(data1)
              sum1 = data1[,.(cnt=round(.N/TN*100,2)),by=g] }
          else
          {
            sum1 = data1[,.(cnt=get(cp)(get(k),na.rm=T)),by=g]
          }
          setnames(sum1,"cnt",cp)
          if (nn==1) {summatab = sum1} else
          {
            nc = ncol(sum1)
            summatab=cbind(summatab,sum1[,nc,with=F])
          }
          if (nn==length(stat)) {summatab[,Attribute:=k];summatab[,Filter:=filter] }
        }
        Final_data = rbind(Final_data,summatab)
        rm(summatab,sum1,nn)
      }
      setnames(Final_data,g,"Level")
      Final_data[,Group_by:=g]
      Final_data1 = rbind(Final_data1,Final_data)
      rm(Final_data)
    }
    if(is.null(filt)) {Final_data1[,Filter:=NULL]}
    if(Nvartp>0) {Final_data1[,Attribute:=NULL]}
    if(is.null(Cvar)) {Final_data1[,Group_by:=NULL]
      Final_data[,Overall:=NULL]}

    nam1 = stat
    nam2 = names(Final_data1)[names(Final_data1)%ni%nam1]
    Final_data1 <- Final_data1[,c(nam2,nam1),with=F]

    if(isTRUE(dcast)){
      if(is.null(Cvar)) stop("Group variable 'Cvar' is null")
      cf<-formula(paste("Attribute",paste0(c("Group_by","Level"),collapse = "+"),sep="~"))
      cp=dcast(Final_data1,cf,value.var=stat)
      return(cp)
    } else {return(Final_data1)}

  }
}
