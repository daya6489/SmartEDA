# defualt ggplot theme used in SmartEDA

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
if (class(mytheme) != "theme") stop("Input ggplot theme is incorrect")
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
scolorsel <- function(col = NULL, nlevel = 0){
  if (is.null(col)){
    if (nlevel <= 1) {
      fill_1 <- hcl.colors(1, "Set 2")
    } else
      if (nlevel == 2) {
        fill_1 <- hcl.colors(2, "Set 2")
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
            fill_1 <- hcl.colors(nlevel, "Set 2")
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
              sum = round(sum(x, na.rm = T), r),
              min = round(min(x, na.rm = T), r),
              max = round(max(x, na.rm = T), r),
              mean = round(mean(x, na.rm = T), r),
              median = round(median(x, na.rm = T), r),
              SD = round(sd(x, na.rm = T), r),
              CV = round(sd(x, na.rm = T) / mean(x, na.rm = T), r),
              IQR = round(IQR(x, na.rm = T), r))
  Skw_kurt <- c(
    Skewness = round(ExpSkew(x, type = "moment"), r),
    Kurtosis = round(ExpKurtosis(x, type = "excess"), r)
  )
  Out_rp <- c(
    `LB` <- round(quantile(x, 0.25, na.rm = TRUE) - (1.5 * IQR(x, na.rm = T)), r),
    `UB` <- round(quantile(x, 0.75, na.rm = TRUE) + (1.5 * IQR(x, na.rm = T)), r),
    nOutliers <- length(which(x > (quantile(x, 0.75, na.rm = TRUE) + (1.5 * IQR(x, na.rm = T ))) | x < (quantile(x, 0.25, na.rm = TRUE) - (1.5 * IQR(x, na.rm = T))))))

     if (MesofShape == 1){
         if (!is.null(Qnt) & Outlier == F){
           qntil <- round(quantile(x, prob = Qnt, na.rm = T), r);
           vect_value <- c(BasDST, qntil);
           return(vect_value)
           } else
              if (Outlier == T & !is.null(Qnt)){
                qntil <- round(quantile(x, prob = Qnt, na.rm = T), r)
                vect_value <- c(BasDST, qntil, Out_rp)
                return(vect_value)
                } else
                  if (Outlier == T & is.null(Qnt)){
                      vect_value <- c(BasDST, Out_rp)
                      return(vect_value)
                  } else {
                        vect_value <- c(BasDST)
                        return(vect_value)
                        }
         }
       if (MesofShape == 2){
         if (Outlier == F & !is.null(Qnt)){
            qntil <- round(quantile(x, prob = Qnt, na.rm = T), r)
            vect_value <- c(BasDST, Skw_kurt, qntil)
            return(vect_value)
            } else
              if (Outlier == T & !is.null(Qnt)){
                qntil <- round(quantile(x, prob = Qnt, na.rm = T), r)
                vect_value <- c(BasDST, Skw_kurt, qntil, Out_rp)
                return(vect_value)
              } else
                if (Outlier == T & is.null(Qnt)){
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
