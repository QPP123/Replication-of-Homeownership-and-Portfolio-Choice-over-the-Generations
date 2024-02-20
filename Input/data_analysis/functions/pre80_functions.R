varlines <- function(path) {
  
  
  vn <- read.table(file=paste0(path,'varlines.txt'),as.is=T,fill=T)
  
  vn2 <- as.data.table(c(vn$V1,vn$V3,vn$V5))
  vn2$columns <- c(vn$V2,vn$V4,vn$V6)
  vn2 <- subset(vn2,V1!="")
  mira <- strsplit(vn2$columns,"-")
  mira2 <- unlist(mira)
  begin <- seq(1,length(mira2)-1,2)
  end <- seq(2,length(mira2),2)
  col.start <- as.numeric(mira2[begin])
  col.end <- as.numeric(mira2[end])
  
  mil <- sort(col.start,index.return=T)
  begin <- mil$x
  ind <- mil$ix
  end <- col.end[ind]
  col.name <- vn2$V1[ind]
  
  mi.vars <- as.data.table(cbind(col.name,begin,end))
  mi.vars$begin <- as.numeric(mi.vars$begin)
  mi.vars$end <- as.numeric(mi.vars$end)
  mi.vars$col.length <- mi.vars$end-mi.vars$begin+1
  
  return(mi.vars)
}