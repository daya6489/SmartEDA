#' Distributions of categorical variables
#'
#'
#' @description This function automatically scans through each variable and creates bar plot for categorical variable.
#' @usage ExpCatViz(data,target=NULL,fname=NULL,clim=10,col=NULL,margin=1,Page=NULL,
#' Flip=F,sample=NULL,rdata=FALSE,value=NULL,gtitle=NULL,theme="Default")
##' @param data dataframe or matrix
##' @param target target variable. This is not a mandatory field.
##' @param fname output file name. Output will be generated in PDF format
##' @param clim maximum categories to be considered to include in bar graphs.
##' @param col define the colors to fill the bars, default it will take sample colours.
##' @param margin index, 1 for row based proportions and 2 for column based proportions
##' @param Page output pattern. if Page=c(3,2), It will generate 6 plots with 3 rows and 2 columns
##' @param Flip default vertical bars. It will be used to flip the axis vertical to horizontal
##' @param sample random selection of categorical variable
##' @param rdata to plot bar graph for frequency/aggregated table
##' @param value value coloumn name. This is mandatory if 'rdata' is TRUE
##' @param gtitle graph title
##' @param theme extra themes, geoms, and scales for 'ggplot2' (use theme from ggthemes package)
##' @seealso \code{\link[ggplot2:geom_bar]{geom_bar}}
##' @return This function returns collated graphs in grid format in PDF or JPEG format. All the files will be stored in the working directory
##'
##'Bar graph - for raw data(this function will dynamically pick all the categorical variable and plot the bar chart)
##'
##'Bar graph - aggregated data
##'
##'Stacked Bar graph by target variable
##'
##' @examples
#' ## Bar graph for specified variable
#' mtdata <- mtcars
#' mtdata$carname <- rownames(mtcars)
#' ExpCatViz(data=mtdata,target="carname",col="blue",rdata=TRUE,value="mpg")
##' @importFrom grDevices colors pdf dev.off
##' @importFrom gridExtra marrangeGrob
##' @importFrom sampling srswor
##' @export ExpCatViz

ExpCatViz <- function(data,target=NULL,fname=NULL,clim=10,col=NULL,margin=1,Page=NULL,Flip=F,sample=NULL,rdata=FALSE,value=NULL,gtitle=NULL,theme="Default") {
  if(!is.data.frame(data)) stop("'data must be a data.frame'")
  if(is.null(data))stop("There is no input data frame")

  r<-0
  xx = as.data.frame(data)

  if(theme=="Default") {
    mytheme <- theme(axis.text.x=element_text(angle=90,vjust=.5,hjust=0.95,size=8,colour='grey20'),
                     axis.text.y=element_text(vjust=.5,hjust=0.95,size=8,colour='grey20'),
                     plot.title = element_text(hjust = 0.5, face = "bold", colour = "#5F9EA0", size = 12))+
      theme(axis.line = element_line(size=1, colour = "black"),
            panel.grid.major = element_line(colour = "#d3d3d3",linetype = "dashed"), panel.grid.minor = element_blank(),
            panel.border = element_blank(), panel.background = element_blank())
  } else {mytheme <- theme }

  if(rdata==FALSE){
    num_var = names(xx)[sapply(xx, is.numeric)]
    Cat_var <- c(names(xx)[sapply(xx, is.character)],names(xx)[sapply(xx, is.factor)])

    if (length(num_var)>0){
      num_var <- num_var[sapply(xx[,num_var], function(x){length(unique(na.omit(x)))>1 & length(unique(na.omit(x)))<=clim})]}


    if((length(num_var)+length(Cat_var))==0) stop("there is no categorical variable in the data")

    wrap_40 <- wrap_format(40)
  if(!is.null(target)) {
      # Tar_var = xx[,target]
      # if(is.numeric(Tar_var)&length(unique(Tar_var))>5)
      #
      Yvar = as.character(paste0(xx[,target]))
      nlevel = length(unique(Yvar))
      if(is.null(col) & nlevel==1){fill_1 <- c("#5F9EA0")} else
        if(is.null(col) & nlevel==2){fill_1 <- c("#5F9EA0", "#E1B378")} else
          if(is.null(col) & nlevel>2){fill_1 <- colors()[srswor(nlevel,length(colors()))==1]} else
            if(!is.null(col) & nlevel>2){fill_1 <- col} else {fill_1 <- col}
      if(length(Cat_var) >0){
        Cat_varlst <- Cat_var[sapply(xx[,Cat_var], function(x){length(unique(x))<=clim & length(unique(x))>=2})]
        Cat_varlst <- c(Cat_varlst,num_var)
        Cat_varlst <- Cat_varlst[!(Cat_varlst %in% target)]} else
        {Cat_varlst <- num_var[!(num_var %in% target)]}

    }
    else {

      if(length(Cat_var) >0){
        Cat_varlst <- Cat_var[sapply(xx[,Cat_var], function(x){length(unique(x))<=clim & length(unique(x))>=2})]
        Cat_varlst <- c(Cat_varlst,num_var)
      } else {

        Cat_varlst <- num_var}

    }
    options(scipen = 100)

    if(!is.null(sample))
      {
      if(sample>length(Cat_varlst)) {Cat_varlst <- Cat_varlst} else {
        Cat_varlst <- Cat_varlst[srswor(sample,length(Cat_varlst))==1]
      }
              }
  } ## Bar plot ends

  else
  {
    if(is.null(value)) stop("value column is missing")
    Cat_varlst <- value
  }

  ### GGPLOT graph start from here
  plot_l <- lapply(Cat_varlst, function(j){
    Xvar = as.character(paste0(xx[,j]))
    if(!is.null(gtitle)) {chart_title <- gtitle} else chart_title<-NULL
    if (rdata==TRUE){
      data = as.data.frame(data)
      tbl <- data[,c(target,value)]
      names(tbl) <- c("Xvar","Freq")
      tbl$Freq <- round(tbl$Freq,1)
      nlevel <- length(unique(tbl$Xvar))
      if(is.null(col)& nlevel==1){fill_1 <- c("#5F9EA0")}
      else
        if(is.null(col) & nlevel>=2){fill_1 <- colors()[srswor(nlevel,length(colors()))==1]}
      else
        fill_1=col

      pp = ggplot(tbl, aes(y=Freq, x=reorder(Xvar,Freq),label = paste0(Freq))) +
        geom_bar( stat="identity", position="dodge",fill=fill_1)+xlab("")+ggtitle(chart_title)+
        ylab(value)+ geom_text(size = 3, position=position_dodge(width=0.9),vjust=0)+
        scale_x_discrete(labels = wrap_format(8))+
        scale_y_continuous(labels = dollar_format(suffix = "", prefix = "")) +mytheme

      if(Flip==TRUE) {return(pp+coord_flip())} else {return(pp)}
    }
    else
      if(!is.null(target)) {
        Tar_var = xx[,target]
        if(is.numeric(Tar_var)&length(unique(Tar_var))>5) {
          nlevel = length(unique(Xvar))

          if(is.null(col) & nlevel==2){fill_1 <- c("#5F9EA0", "#E1B378")} else
            if(is.null(col) & nlevel>2){fill_1 <- colors()[srswor(nlevel,length(colors()))==1]}


          tb <- table(Xvar)

          tbl = data.frame(round(tb/sum(tb)*100,r))
          wrap_40 <- wrap_format(40)
          pp = ggplot(tbl, aes(y=Freq, x=Xvar,label = paste0(Freq,"%"))) +
            geom_bar( stat="identity", position="dodge",fill=fill_1)+xlab("")+
            ylab("Percentage (%)")+ggtitle(wrap_40(paste(chart_title," ",j)))+
            geom_text(size = 4, position=position_dodge(width=0.9),vjust=0)+
            scale_x_discrete(labels = wrap_format(8))+
            scale_y_continuous(labels = dollar_format(suffix = "%", prefix = ""))+mytheme

          if(Flip==TRUE) {return(pp+coord_flip())} else {return(pp)}

        } else {

          tb <- table(Xvar,Yvar)
          Freq <- NULL
          switch (margin,
                  {tbl = data.frame(round(prop.table(tb,2)*100,r))},
                  {tbl = data.frame(round(prop.table(tb,1)*100,r))}
          )
          pp = ggplot(tbl, aes(fill=Yvar, y=Freq, x=Xvar,label = paste0(Freq,"%"))) +
            geom_bar( stat="identity", position="dodge")+xlab("")+
            ylab("Percentage (%)")+ggtitle(wrap_40(paste(chart_title," ",j," vs ",target,"[Target]")))+
            geom_text(size = 4, position=position_dodge(width=0.9),vjust=0)+
            scale_x_discrete(labels = wrap_format(8))+
            scale_y_continuous(labels = dollar_format(suffix = "%", prefix = ""))+
            scale_fill_manual("Target",values=fill_1)+mytheme

          if(Flip==TRUE) {return(pp+coord_flip())} else {return(pp)}

        }
      }
    else
    {
      nlevel = length(unique(Xvar))
      if(!is.null(col) & length(col)==1){fill_1 <- col} else
        if(!is.null(col) & length(col)==2 & nlevel==2){fill_1 <- col} else
          if(is.null(col) & nlevel==2){fill_1 <- c("#5F9EA0", "#E1B378")} else
            if(is.null(col) & nlevel>2){fill_1 <- colors()[srswor(nlevel,length(colors()))==1]}


      tb <- table(Xvar)

      tbl = data.frame(round(tb/sum(tb)*100,r))

      pp = ggplot(tbl, aes(y=Freq, x=Xvar,label = paste0(Freq,"%"))) +
        geom_bar( stat="identity", position="dodge",fill=fill_1)+xlab("")+
        ylab("Percentage (%)")+ggtitle(wrap_40(paste(chart_title," ",j)))+
        geom_text(size = 4, position=position_dodge(width=0.9),vjust=0)+
        scale_x_discrete(labels = wrap_format(8))+
        scale_y_continuous(labels = dollar_format(suffix = "%", prefix = ""))+mytheme



      if(Flip==TRUE) {return(pp+coord_flip())} else {return(pp)}

    }

  })

  if (!is.null(fname)) {
    filname <- paste(fname,"pdf",sep = ".")
    if (!is.null(Page)) {
      pn = length(plot_l)
      nc = Page[2]
      nr = Page[1]
      if((nc*nr)>pn+3)stop("reduce the matrix dimension from Page(r,c)")
      pg = ceiling((nc*nr)/pn)
      gspl <- split(plot_l, (seq_along(plot_l)-1) %/% pn)
      gplt <- lapply(gspl, function(g) marrangeGrob(grobs = g, layout_matrix = matrix(data = seq(1,pn),nrow = nr,ncol = nc)))
      options(warn = -1)
      pdf(file=filname,width=10,height = 14,onefile=T,title="Summary analysis",paper = "a4r")
      print(gplt)
      dev.off()
      message(paste0("\n\n",filname," is generated at \"", getwd(),"\"."))
    } else {
      pdf(file=filname,width=10,height = 6,onefile=T,title="Summary analysis",paper = "USr")
      print(plot_l)
      dev.off()}
  } else
  {if (!is.null(Page)) {
    pn = length(plot_l)
    nc = Page[2]
    nr = Page[1]
    if((nc*nr)>pn+3)stop("reduce the matrix dimension from Page(r,c)")
    pg = ceiling((nc*nr)/pn)
    gspl <- split(plot_l, (seq_along(plot_l)-1) %/% pn)
    gplt <- lapply(gspl, function(g) marrangeGrob(grobs = g, layout_matrix = matrix(data = seq(1,pn),nrow = nr,ncol = nc)))
    return(gplt)} else {return(plot_l)}
  }

}
