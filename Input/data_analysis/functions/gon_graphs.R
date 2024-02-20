## GRAPH FUNCTIONS

# This file is designed to be a portable list of graph formats that we are interested in
# replicating.

# Set of graphs (auxiliary for the other files, loaded whenever needed)

grafico.linea <- function(df,xaxis,yaxis,xlabel,ylabel,graphname,ylim=NULL,xticks=NULL,lwidth=0.3,
                          fsize=14){
  
  if (is.null(xticks)){
    xticks <- waiver()
    minticks <- NULL
    maxticks <- NULL
    } else {
     minticks <- min(xticks)
     maxticks <- max(xticks)
    }
  
  y <- ggplot(df,aes(x=get(xaxis,as.environment(df)),
                               y=get(yaxis,as.environment(df)))) +
    geom_line() +
    scale_x_continuous(name=xlabel,expand=c(0,0),breaks=xticks,labels=xticks,lim=c(minticks,
                                                                                   maxticks)) + 
    scale_y_continuous(name=ylabel,lim=ylim) + 
    theme_light() + geom_line(size=lwidth) +
    theme(axis.text=element_text(size=fsize,face="bold"),
          axis.title=element_text(size=fsize,face="bold"))+
    theme(plot.margin=unit(c(0.25,0.4,0.25,0.25),"cm"))
  
  ggsave(filename=paste(folder,graphname,sep=""),plot=y,width=8,height=6)
  
  return(y)
}

grafico.varias.lineas <- function(df,xaxis,yaxis,groupvar,xlabel,ylabel,graphname,ylim=NULL,xticks=NULL,
                                  lwidth=0.3,fsize=15){
  
  if (is.null(xticks)){
    xticks <- waiver()
    minticks <- NULL
    maxticks <- NULL
  } else {
    minticks <- min(xticks)
    maxticks <- max(xticks)
  }
  
  y <- ggplot(df,aes(x=get(xaxis,as.environment(df)),
                     y=get(yaxis,as.environment(df)),
                     colour=get(groupvar,as.environment(df)),
                    group=get(groupvar,as.environment(df)))) +
    geom_line() +
    scale_x_continuous(name=xlabel,expand=c(0,0),breaks=xticks,labels=xticks,lim=c(minticks,
                                                                                   maxticks)) +
    scale_y_continuous(name=ylabel,lim=ylim) +
    theme_light() + geom_line(size=lwidth) +
    theme(axis.text=element_text(size=fsize,face="bold"),
          axis.title=element_text(size=fsize,face="bold"),
          legend.position="bottom",legend.title=element_blank(),
          legend.text=element_text(size=fsize))+
    theme(plot.margin=unit(c(0.25,0.4,0.25,0.25),"cm"))

  ggsave(filename=paste(folder,graphname,sep=""),plot=y,width=8,height=6)
  
  return(y)
}

grafico.varias.lineas3 <- function(df,xaxis,yaxis,groupvar,xlabel,ylabel,graphname,ylim=NULL,xticks=NULL,
                                     lwidth=0.3,fsize=15){
  
  if (is.null(xticks)){
    xticks <- waiver()
    minticks <- NULL
    maxticks <- NULL
  } else {
    minticks <- min(xticks)
    maxticks <- max(xticks)
  }
  
  y <- ggplot(df,aes(x=get(xaxis,as.environment(df)),
                     y=get(yaxis,as.environment(df)),
                     linetype=get(groupvar,as.environment(df)),
                     group=get(groupvar,as.environment(df))),
                    color=get(groupvar,as.environment(df))) +
    geom_line(size=lwidth,aes(color=get(groupvar,as.environment(df)),linetype=get(groupvar,as.environment(df)))) +
    scale_x_continuous(name=xlabel,expand=c(0,0),breaks=xticks,labels=xticks,lim=c(minticks,
                                                                                   maxticks)) +
    scale_y_continuous(name=ylabel,lim=ylim) +
    scale_colour_manual(values = c("orange", "brown","pink","red","green","purple","black"))+
    scale_linetype_manual(values=c("solid","longdash","twodash","dotdash","longdash","twodash",
                                   "solid"))+
    theme_light() +
    theme(axis.text=element_text(size=fsize,face="bold"),
          axis.title=element_text(size=fsize,face="bold"),
          legend.position="bottom",legend.title=element_blank(),
          legend.text=element_text(size=fsize),legend.key.size=unit(2.9,'lines'))+
    theme(plot.margin=unit(c(0.25,0.4,0.25,0.25),"cm"))
  
  ggsave(filename=paste(folder,graphname,sep=""),plot=y,width=8,height=6)
  
  return(y)
}

is.even <- function(x) x %% 2 == 0


smooth.val3 <- function(x,vars.to.smooth,var.by,var.order,k) {
  x <- as.data.table(x)
  x <- subset(x,is.na(get(vars.to.smooth))==0)
  setkeyv(x,c(var.by,var.order))
  y <- data.table()
  
  for (i in seq(1,length(levels(get(var.by,envir=as.environment(x)))))) {
    rub1 <- subset(x,get(var.by)==levels(get(var.by,envir=as.environment(x)))[i])
    setkeyv(rub1,var.order)
    rub1$V2 <- rollmean(get(vars.to.smooth,envir=as.environment(rub1)),k=k,fill=NA)
    j <- 1
    while (j<k) {
      rub1$V3 <- rollmean(get(vars.to.smooth,envir=as.environment(rub1)),k=(k-j),fill=NA)
        #data.frame(llply(mget(vars.to.smooth,envir=as.environment(x)),rollmean,k=(k-i),fill=NA))
      rub1$V2[is.na(rub1$V2)] <- rub1$V3[is.na(rub1$V2)]
      j <- j+1
    }
    ytemp <- subset(x,get(var.by)==levels(get(var.by,envir=as.environment(x)))[i])
    ytemp$smoo <- rub1$V2
    y <- rbind(y,ytemp)
  }

  return(y)
  
}

smooth.val4 <- function(x,vars.to.smooth,var.order,k) {
  x <- as.data.table(x)
  x <- subset(x,is.na(get(vars.to.smooth))==0)
  setkeyv(x,c(var.order))
  y <- data.table()
  
    rub1 <- x
    setkeyv(rub1,var.order)
    rub1$V2 <- rollmean(get(vars.to.smooth,envir=as.environment(rub1)),k=k,fill=NA)
    j <- 1
    while (j<k) {
      rub1$V3 <- rollmean(get(vars.to.smooth,envir=as.environment(rub1)),k=(k-j),fill=NA)
      #data.frame(llply(mget(vars.to.smooth,envir=as.environment(x)),rollmean,k=(k-i),fill=NA))
      rub1$V2[is.na(rub1$V2)] <- rub1$V3[is.na(rub1$V2)]
      j <- j+1
    }
    ytemp <- x
    ytemp$smoo <- rub1$V2
    y <- ytemp
  
  return(y)
  
}

smooth.val.na <- function(x,var.to.smooth,var.by,k) {
  x <- as.data.table(x)
  x <- subset(x,is.na(get(var.to.smooth))==0)
  rub1 <- x[,rollmean(get(var.to.smooth),k=k,fill=NA),
            by=var.by]
  names(rub1) <- c(var.by,"smoo")
  x$smoo <- rub1$smoo

  return(x)
  
}