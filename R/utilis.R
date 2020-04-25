# default ggplot theme used in SmartEDA

smtheme <- function(theme){
  themedf <- theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 0.95, size = 8, colour = "grey20"),
                   axis.text.y = element_text(vjust = .5, hjust = 0.95, size = 8, colour = "grey20"),
                   plot.title = element_text(hjust = 0.5, face = "bold", colour = "#5F9EA0", size = 12),
                   axis.line = element_line(size = 1, colour = "black"),
                   panel.grid.major = element_line(colour = "#d3d3d3", linetype = "dashed"),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(),
                   panel.background = element_blank())
  if (theme == "Default") kp <- 1
  if (theme != "Default") kp <- 2

  switch (kp,
          mytheme <- themedf,
          mytheme <- theme
  )
#if (class(mytheme) != "theme") stop("Input ggplot theme is incorrect")
  return(mytheme)
}

# save smartEDA image output into PDF format
swritepdf <- function(fname, plot_l, Page){
  filname <- paste(fname, "pdf", sep = ".")
  if (!is.null(Page)) {
      pn <- length(plot_l)
      nc <- Page[2]
      nr <- Page[1]
      if ( (nc * nr) > pn + 3) stop("reduce the matrix dimension from Page(r,c)")
      gspl <- split(plot_l, (seq_along(plot_l) - 1) %/% pn)
      gplt <- lapply(gspl, function(g) marrangeGrob(grobs = g, layout_matrix = matrix(data = seq(1, pn), nrow = nr, ncol = nc)))
      options(warn = -1)
      pdf(file = filname, width = 10, height = 14, onefile = T, title = "Summary analysis", paper = "a4r")
      print(gplt)
      dev.off()
      message(paste0("\n\n", filname, " is generated at \"", getwd(), "\"."))
  } else {
      pdf(file = filname, width = 10, height = 6, onefile = T, title = "Summary analysis", paper = "USr")
      print(plot_l)
      message(paste0("\n\n", filname, " is generated at \"", getwd(), "\"."))
      dev.off()
      }
}

# chart colour selection function
# Selecting multiple colours based on the chart input
hcl_colors <- c("#ED90A4","#EC919F","#EC929A","#EB9396","#E99491","#E8968C","#E79787","#E59883","#E3997E","#E19B79","#DF9C75","#DC9E70","#DA9F6C","#D7A168","#D4A264","#D1A360","#CEA55C","#CBA659","#C7A857","#C3A954","#C0AB52"
                ,"#BCAC51","#B8AD50","#B4AF4F","#AFB050","#ABB150","#A6B352","#A1B454","#9CB556","#97B658","#92B75B","#8CB85F","#86B962","#80BA66","#7ABB6A","#74BC6E","#6DBD73","#66BD77","#5FBE7C","#57BF80","#4FBF85","#46C089"
                ,"#3CC08E","#31C193","#24C197","#12C19C","#00C1A0","#00C1A5","#00C1A9","#00C1AE","#00C1B2","#00C1B6","#00C1BA","#00C0BE","#00C0C2","#00BFC6","#00BECA","#00BECD","#00BDD1","#15BCD4","#28BBD7","#36B9DA","#42B8DD"
                ,"#4DB7DF","#57B6E2","#60B4E4","#69B2E6","#72B1E7","#7AAFE9","#82ADEA","#8AACEB","#91AAEC","#98A8EC","#9FA6EC","#A5A4EC","#ACA2EC","#B2A0EC","#B79FEB","#BD9DEA","#C29BE9","#C699E7","#CB98E6","#CF96E4","#D395E2"
                ,"#D793DF","#DA92DD","#DD91DA","#E090D7","#E28FD4","#E58FD1","#E78ECD","#E88ECA","#EA8DC6","#EB8DC2","#EC8DBE","#ED8EBA","#ED8EB6","#EE8EB1","#EE8FAD","#ED90A8")

scolorsel <- function(col = NULL, nlevel = 0){
  if (is.null(col)){
    if (nlevel <= 1) {
      fill_1 <- hcl_colors[1]
    } else
      if (nlevel == 2) {
        fill_1 <- hcl_colors[1:2]
      } else
        if (nlevel > 2) {
          fill_1 <- colors()[srswor(nlevel, length(colors()) ) == 1]
        }
  } else {
    if (nlevel == length(col)) {
      fill_1 <- col
    } else
      if (length(col) == 1) {
        fill_1 <- col
      } else
        if (length(col) > nlevel) {
          fill_1 <- col[1 : nlevel]
        } else
          if (nlevel > 0) {
            fill_1 <- hcl_colors[1:nlevel]
          }
  }
  return(fill_1)
}

# Numerical variable summary statistics call function
# descriptive statistics function
ds_fun <- function(x, r, MesofShape, Qnt, Outlier){
  BasDST <- c(TN = length(x),
              nNeg = length(which(x < 0)),
              nZero = length(which(x == 0)),
              nPos = length(which(x > 0)),
              NegInf = length(which(x == -Inf)),
              PosInf = length(which(x == Inf)),
              NA_Value = length(x[is.na(x)]),
              Per_of_Missing = round( (length(x[is.na(x)]) / length(x)) * 100, r),
              sum = round(sum(x, na.rm = TRUE), r),
              min = round(min(x, na.rm = TRUE), r),
              max = round(max(x, na.rm = TRUE), r),
              mean = round(mean(x, na.rm = TRUE), r),
              median = round(median(x, na.rm = TRUE), r),
              SD = round(sd(x, na.rm = TRUE), r),
              CV = round(sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE), r),
              IQR = round(IQR(x, na.rm = TRUE), r))
  Skw_kurt <- c(
    Skewness = round(ExpSkew(x, type = "moment"), r),
    Kurtosis = round(ExpKurtosis(x, type = "excess"), r)
  )
  Out_rp <- c(
    LB = round(quantile(x, 0.25, na.rm = TRUE) - (1.5 * IQR(x, na.rm = TRUE)), r),
    UB = round(quantile(x, 0.75, na.rm = TRUE) + (1.5 * IQR(x, na.rm = TRUE)), r),
    nOutliers = length(which(x > (quantile(x, 0.75, na.rm = TRUE) + (1.5 * IQR(x, na.rm = TRUE ))) | x < (quantile(x, 0.25, na.rm = TRUE) - (1.5 * IQR(x, na.rm = T)))))
    )

     if (MesofShape == 1){
         if (!is.null(Qnt) & Outlier == FALSE){
           qntil <- round(quantile(x, prob = Qnt, na.rm = TRUE), r);
           vect_value <- c(BasDST, qntil);
           return(vect_value)
           } else
              if (Outlier == TRUE & !is.null(Qnt)){
                qntil <- round(quantile(x, prob = Qnt, na.rm = TRUE), r)
                vect_value <- c(BasDST, qntil, Out_rp)
                return(vect_value)
                } else
                  if (Outlier == TRUE & is.null(Qnt)){
                      vect_value <- c(BasDST, Out_rp)
                      return(vect_value)
                  } else {
                        vect_value <- c(BasDST)
                        return(vect_value)
                        }
         }
       if (MesofShape == 2){
         if (Outlier == FALSE & !is.null(Qnt)){
            qntil <- round(quantile(x, prob = Qnt, na.rm = TRUE), r)
            vect_value <- c(BasDST, Skw_kurt, qntil)
            return(vect_value)
            } else
              if (Outlier == TRUE & !is.null(Qnt)){
                qntil <- round(quantile(x, prob = Qnt, na.rm = TRUE), r)
                vect_value <- c(BasDST, Skw_kurt, qntil, Out_rp)
                return(vect_value)
              } else
                if (Outlier == TRUE & is.null(Qnt)){
                vect_value <- c(BasDST, Skw_kurt, Out_rp)
                return(vect_value)
                } else {
              vect_value <- c(BasDST, Skw_kurt)
              return(vect_value)
            }
          }
  }

## Skewness function
skew_fun <- function(x){
  x <- x[!is.na(x)]
  xbar <- mean(x)
  sdx <- sd(x)
  n <- length(x)
  m2 <- sum( (x - xbar) ^ 2) / n #is the variance of the data set
  m3 <- sum( (x - xbar) ^ 3) / n #is third moment of the data set
  skw_out <- list(moment = (m3) / (m2 ^ (3 / 2)),
                sample = sum( (x - xbar) ^ 3 / sdx ^ 3) * n / ( (n - 1) * (n - 2)))
  return(skw_out)
}

## Kurtosis function
Kurt_fun <- function(x){
  x <- x[!is.na(x)]
  xbar <- mean(x)
  n <- length(x)
  m2 <- sum( (x - xbar) ^ 2) / n #is the variance of the data set
  m4 <- sum( (x - xbar) ^ 4) / n #is fourth moment of the data set
  a4 <- round(m4 / m2 ^ 2, 3)
  g2 <- round(a4 - 3, 3)
  kurt_op <- list(mom = a4, exc = g2)
  return(kurt_op)
}

## Making dummy variables
make_dummies <- function(v, prefix = NULL, sep=" ") {
  if (is.data.frame(v)){
    pref_nma <- names(v)
    df <- NULL
    for (j in pref_nma){
      x <- v[, j]
      s <- sort(unique(x))
      d <- outer(x, s, function(x, s) 1L * (x == s))
      colnames(d) <- paste0(j, sep, s)
      df <- cbind(df, d)
    }
  } else {
    s <- sort(unique(v))
    df <- outer(v, s, function(v, s) 1L * (v == s))
    colnames(df) <- paste0(prefix, s)
  }
  return(df)
}

## Custom tab call fucntion
# stat - input statistics like 'mean','median','sum' etc
# data1 - input data set
# k - numeric varaible name
# g - categorical varaible name
# filter - Filter if any
ctab_stat <- function(data1, stat, k, g, filter) {
  nn <- 0L
for (j in stat) {
  nn <- nn + 1L
  cp <- j
  tt <- strsplit(toupper(j), "[P]")[[1]][1]
  if (tt == ""){
    options(warn = -1)
    qnt <- as.numeric(strsplit(toupper(j),"[P]")[[1]][2])
  }
  if (toupper(j) %in% paste0("P", seq(0, 1, 0.01))){
    sum1 <- data1[, .(cnt = quantile(get(k), probs = qnt, na.rm = T)), by = g]
    } else
    if (tolower(cp) == "count"){
      sum1 <- data1[, .(cnt = .N), by = g]
      } else
      if (tolower(cp) == "ps"){
        Tot <- data1[, .(sum(get(k), na.rm = T))]
        sum1 <- data1[, .(cnt = round( (sum(get(k), na.rm = T) / Tot$V1) * 100, 2)), by = g]
        } else
        if (tolower(cp) == "prop"){
        TN <- nrow(data1)
        sum1 <- data1[, .(cnt = round(.N / TN * 100, 2)), by = g]
        } else {
        sum1 <- data1[, .(cnt = get(cp) (get(k), na.rm = T)), by = g]
        }
  setnames(sum1, "cnt", cp)
  if (nn == 1) {
    summatab <- sum1
    } else {
      nc <- ncol(sum1)
      summatab <- cbind(summatab, sum1[, nc, with = F])
      }
  if (nn == length(stat)){
    setDT(summatab)
    summatab[, Attribute := k]
    summatab[, Filter := filter]
    }
  }
  return(summatab)
}

# Filter extraction
# input
# data : input data set
# filt : filter conditions
# fk : 0
# k : Numeric variable

ctab_filter <- function(data, filt, fk, k) {
  cc1 <- unlist(strsplit(filt, "^", fixed = TRUE))
  ccd <- trimws(cc1)
  if (length(ccd) == 1){
    allft <- substr(ccd, 1, 3)
    if (tolower(allft) == "all"){
      allft1 <- trimws(gsub("All", "", ccd, ignore.case = T))
      filter <- as.character(paste0(k, allft1))
      } else
      filter <- as.character(ccd)
      } else {
      filter <- (ccd[fk])
      }
  if (tolower(filter) == "" | tolower(filter) == "all"){
    data1 <- data
  } else {
    data1 <- data[with(data, eval(parse(text = filter))), ]
    }
  return(list(dataset = data1, filter = filter))
}

"%ni%" <- Negate("%in%")

# Expdata support function
expdatatype2 <- function(xx, x, myfun=NULL){
  Xvar <- xx[, x]
  if(is.null(myfun)) ccd <- NULL
  cla_var <- as.character(paste0(class(xx[, x]), collapse = ":"))

  if(!is.numeric(Xvar)) {
    if(is.character(Xvar)){
      Xvar[Xvar==''] <- "missing_row"
      Xvar1 <- Xvar[complete.cases(Xvar)]
      missing_count <- length(Xvar[is.na(Xvar)]) + length(Xvar1[Xvar1 == "missing_row"])
      Per_missing <- round(missing_count / length(Xvar), 3)
      sample_n <- length(Xvar) - missing_count
    } else
      {
      missing_count <- length(Xvar[is.na(Xvar)])
      Per_missing <- round(missing_count / length(Xvar), 3)
      sample_n <- length(Xvar) - missing_count
    }
    if(!is.null(myfun)) {
      ccd = sapply(myfun, function(x){
        return(0)
      })
    }
  } else {
    missing_count <- length(Xvar[is.na(Xvar)])
    Per_missing <- round(missing_count / length(Xvar), 3)
    sample_n <- length(Xvar) - missing_count
    if(!is.null(myfun)) {
      ccd = sapply(myfun, function(x){
        return(round(get(x)(Xvar[complete.cases(Xvar)]), 2))
      })
    }
  }

  Per_Unique <- length(unique(Xvar[!is.na(Xvar)]))
  mydata <- c(Index = 1, VarName = x, VarClass = cla_var, sample_n=sample_n,
              missing_count = missing_count, Per_mis = Per_missing, Unique = Per_Unique, ccd)
  return(mydata)
}

### Univariate Numeirc plots
# data - data frame
# var - independent numeric variable
# arg_list - ggplot argument list
# geom_type - type of ggplot (histogram, density, boxplot)

univariate_num_plot <- function(data, var, arg_list, geom_type){
  wrap_40 <- wrap_format(40)
  if(geom_type == 'boxplot') {
    ggplotd <- ggplot(data, aes(x = "", y = get(var))) + xlab(" ") + ylab(var)+ ggtitle(paste0("Boxplot for ", var))
  } else
    if(geom_type == 'qqplot'){
      ggplotd <- ggplot(data, aes(sample = get(var))) +
        xlab("theoretical quantiles") +
        ylab("sample quantiles") +
        ggtitle(paste0("quantile-quantile plot : ", var)) +
        stat_qq() + stat_qq_line()
      return(ggplotd)
    } else
      {
    ggplotd <- ggplot(data, aes(x = get(var))) + xlab(var) + ggtitle(paste0(geom_type, " for ", var))
  }
  my_rplot <- do.call(paste0("geom_",geom_type), c(arg_list))
  return(ggplotd + my_rplot)
}

univariate_cat_plot <- function(data, var, arg_list, geom_type){
  setDT(data)
  tot_sample <- nrow(data)
  wrap_40 <- wrap_format(40)
  myinput <- data[, .(count = .N, fraction = .N/tot_sample), .(group_by = get(var))]
  geom_type1 <- copy(geom_type)
  if(geom_type %in% c("bar", "barh")) {
    ggplotd <- ggplot(myinput, aes(x = reorder(group_by,-count), y = count, fill = group_by)) +
      geom_text(aes(label = paste(count)), size = 3, check_overlap = F,  position = position_stack(vjust = 1.1) ) +
      xlab(var) +
      theme(legend.position = "none") +
      ylab("Frequency")+
      ggtitle(paste0("Distributions of ", var))
    if(geom_type == "barh") ggplotd <- ggplotd + coord_flip()
    geom_type1 <- "bar"
  } else
    if(geom_type == 'donut') {
      geom_type1 <- "rect"
      myinput[, ymax := cumsum(fraction)]
      myinput[, ymin := c(0, head(ymax, n=-1))]
      myinput[, labelPosition := (ymax + ymin) / 2]
      myinput[, label1 := paste0(group_by, "\n ", round(fraction * 100,1), "%")]
      ggplotd <- ggplot(myinput, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=group_by)) +
        geom_rect() +
        geom_label( x=3.5, aes(y=labelPosition, label=label1), size=3) +
        scale_fill_brewer(palette=4) +
        coord_polar(theta="y") +
        xlim(c(2, 4)) +
        theme_void() +
        ggtitle(var) +
        theme(legend.position = "none")
      return(ggplotd)
  } else
    if(geom_type == 'pie') {
      ggplotd <- ggplot(myinput, aes(x="", y=count, fill=group_by)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +
        theme_void() +
        ggtitle(var) +
        scale_fill_discrete(name = var)
      return(ggplotd)
        } else stop("selected chart option is not found")
  my_rplot <- do.call(paste0("geom_", geom_type1), c(arg_list))
  return(ggplotd + my_rplot)
}

globalVariables(c("group_by", "count", "wrap_40", "ymax", "fraction", "ymin", "label1", "labelPosition"))

# library(data.table)
#univariate_cat_plot(Carseats, var = "ShelveLoc", arg_list = list(stat = 'identity'), geom_type="pie")

### Bivariate plots with Categorical target
# data - data frame
# target - categorical dependent variable
# var - independent numeric variable
# arg_list - ggplot argument list
# geom_type - type of ggplot (histogram, density, boxplot)
# col - fill color for each category from target

bivariate_num_plot <- function(data, target, var, arg_list, geom_type, col=NULL){
  data <- as.data.frame(data)
  nlevel <- length(unique(data[, target]))
  fill_1 <- scolorsel(col, nlevel)
  wrap_40 <- wrap_format(40)
  if (! "fill" %in% names(arg_list)) arg_list[['fill']] <- fill_1
  if (geom_type %in% c("histogram", "density", "qqplot") & "fill" %in% names(arg_list)) {
    fill_1 <- arg_list[['fill']] # manual fill
    arg_list[['fill']] <- NULL # remove fill from argument list for histogram
    }

  if (geom_type %in% c('boxplot', 'violin')) {
     ggplotd <- ggplot(data, aes(x = get(target), y = get(var))) +
      xlab(target) +
      ylab(var) +
      ggtitle(wrap_40(paste0(geom_type, " for ", var, " Vs ", target)))
  } else
    if(geom_type == 'qqplot'){
    ggplotd <- ggplot(data, aes(sample = get(var), colour = get(target))) +
      xlab("theoretical quantiles") +
      ylab("sample quantiles") +
      ggtitle(paste0("qq plot : ", var, " Vs ", target)) +
      stat_qq() + stat_qq_line() +
      scale_color_manual(target, values=fill_1)
    return(ggplotd)
  } else {
    ggplotd <- ggplot(data, aes(x = get(var), fill=get(target))) +
      xlab(var) +
      ggtitle(wrap_40(paste0(geom_type, " for ", var, " Vs ", target)))+
      scale_fill_manual(target, values=fill_1)
  }
  my_rplot <- do.call(paste0("geom_",geom_type), c(arg_list))
  return(ggplotd + my_rplot)
}

bivariate_cat_plot <- function(data, target, var, arg_list, geom_type, col=NULL){
  setDT(data)
  tot_sample <- nrow(data)
  wrap_40 <- wrap_format(40)
  myinput <- data[, .(count = .N, fraction = .N/tot_sample), .(group_by = get(var), target = get(target))]
  geom_type1 <- copy(geom_type)
  if(geom_type %in% c("bar", "barh")) {
    ggplotd <- ggplot(myinput, aes(x = reorder(group_by,-count), y = count, fill = target)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste(count)), size = 3, check_overlap = T,  position = position_stack(vjust = 0.5) ) +
      xlab(var) +
      scale_fill_discrete(name = target) +
      ylab("Frequency") +
      ggtitle(paste0("Distributions of ", var, " Vs ", target))
    if(geom_type == "barh") ggplotd <- ggplotd + coord_flip()
    geom_type1 <- "bar"
  } else stop("selected geom plot is not available")
  # my_rplot <- do.call(paste0("geom_", geom_type1), c(arg_list))
  return(ggplotd)
}


