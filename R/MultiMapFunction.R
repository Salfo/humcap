########## Write a function for maps ##############
# ==============================================================================
# ========================= Single map function ================================
#' A Map construction function
#'
#' This function allows the construction of a single map.
#' @param
#' @keywords construct a map
#' @export
#' @examples
#' unimap()
#'
unimap <- function(df){
  statecnts <- count(acc$STATE)
  states <- merge(statepop, statecnts, by.x="code", by.y="x")
  states$accrate <- df
  # Match values to database region names
  mapnames <- map("state", plot=FALSE)$names
  regionlist <- strsplit(mapnames, ":")
  mapnames.fin <- sapply(regionlist, "[", 1)
  m <- match(mapnames.fin, tolower(states$name))
  maprates <- states$accrate[m]

  # Helper function to get color (annual), based on actions per million population
  mini<-min(states$accrate)
  maxi<-max(states$accrate)
  yyy<-(maxi-mini)/4

  getColor <- function(x) {
    if (x > (maxi-yyy)) {
      col <- "#000000"

    } else if (x > (maxi-2*yyy)) {
      col <- "#666666"
    } else if (x > (maxi-3*yyy)) {
      col <- "#999999"
    } else {
      col <- "#CCCCCC"
    }

    return(col)
  }
  statecols <- sapply(maprates, FUN=getColor)

  par(mfrow=c(2,1), mar=c(1,1,1,1))
  # Lower resolution map
  map("state", regions=states$name[m], proj="albers", param=c(39,45), fill=TRUE,
      col=statecols, border=NA, resolution=1)
  #title("Study Period average")


  plot(0, 0, type="n", axes=FALSE, xlim=c(0,30), ylim=c(0,2))
  rect(c(5,10,15,20), c(1,1,1,1), c(10,15,20,25), c(1.25,1.25,1.25,1.25),
       col=sapply(c(6,11,16,21), getColor), border="white", lwd=0.4)
  text(15, 1.35, "legend")
  text(c(10,15,20), c(0.9,0.9,0.9), c("1/4","1/2","3/4"), cex=0.8) # Tick labels

}

# ==============================================================================
# ======================== Multiple map function ===============================

#' A Multi Map construction function
#'
#' This function allows the construction of a several maps in the same frame.
#' @param
#' @keywords construct maps
#' @export
#' @examples
#' multimap()
#'

multimap <- function(df, periods = c("1990-1992", "1991-1993", "1992-1994", "1993-1995", "1994-1996",
                                     "1995-1997", "1996-1998", "1997-1999", "1998-2000", "1999-2001",
                                     "2000-2002", "2001-2003", "2002-2004", "2003-2005", "2004-2006",
                                     "2005-2007", "2006-2008", "2007-2009", "2008-2010", "2009-2011",
                                     "2010-2012", "2011-2013", "2012-2014", "2013-2015")){
  statecnts <- count(acc$STATE)
  period = periods
  statecode <- statepop$code[-40]
  df <- as.data.frame(df)
  df$statecodes<-statecode

  insert <-rep(NA, ncol(df))
  insert[ncol(df)]<-43 # Insert puerto rico state code
  names(insert) <- "Puerto Rico"

  dfNew <-rbind(df[1:39,],"Puerto Rico" = insert,df[40:51,]) # New Dataset containing Puerto rico
    #periods <- rep(NA, (ncol(dfNew)-1))
    statepopNew <- merge(statepop, dfNew, by.x="code", by.y="statecodes")
    states <- merge(statepopNew, statecnts, by.x="code", by.y="x")
    mapnames <- map("state", plot=FALSE)$names
    regionlist <- strsplit(mapnames, ":")
    mapnames.fin <- sapply(regionlist, "[", 1)
    m <- match(mapnames.fin, tolower(states$name))
    ifelse(ncol(df) <= 3, par(mfrow=c(2,2), mar=c(0,0,0,0)),
           ifelse(ncol(df) <= 5, par(mfrow=c(2,3), mar=c(0,0,0,0)),
                  ifelse(ncol(df) <= 8, par(mfrow=c(3,3), mar=c(0,0,0,0)),
                         ifelse(ncol(df) <= 11, par(mfrow=c(3,4), mar=c(0,0,0,0)),
                                ifelse(ncol(df) <= 15, par(mfrow=c(4,4), mar=c(0,0,0,0)),
                                       ifelse(ncol(df) < 20, par(mfrow=c(4,5), mar=c(0,0,0,0)),
                                              ifelse(ncol(df) < 25, par(mfrow=c(5,5), mar=c(0,0,0,0)),
                                                     par(mfrow=c(6,5), mar=c(0,0,0,0)))
           ))))))

    for(j in 1:(ncol(dfNew)-1)){
      states$mapdata <-states[,(j+4)]
      maprates <- states$mapdata[m]

      ## A helper for the color
      mini<-min(states$mapdata)
      maxi<-max(states$mapdata)
      yyy<-(maxi-mini)/4

      getColor <- function(x) {
        if (x > (maxi-yyy)) {
          col <- "#000000"

        } else if (x > (maxi-2*yyy)) {
          col <- "#666666"
        } else if (x > (maxi-3*yyy)) {
          col <- "#999999"
        } else {
          col <- "#CCCCCC"
        }

        return(col)
      }
      statecols <- sapply(maprates, FUN=getColor)

      pp<- map("state", regions=states$name[m], proj="albers", param=c(39,45),
               fill=TRUE, col=statecols, border=NA, resolution=0)
      text(mapproject(-99, 55),paste(periods[j]), cex=1)
  }
  plot(0, 0, type="n", axes=FALSE, xlim=c(0,30), ylim=c(0,2))
  rect(c(5,10,15,20), c(1,1,1,1), c(10,15,20,25), c(1.25,1.25,1.25,1.25),
       col=sapply(c(6,11,16,21), getColor), border="white", lwd=0.4)
  text(15, 1.35, "legend")
  text(c(10,15,20), c(0.9,0.9,0.9), c("1/4","1/2","3/4"), cex=0.8) # Tick labels

}

# ==============================================================================
# =============== A wrapper function for the two function ======================

#' A Wrapper function for constructing maps
#'
#' This function allows the construction of a single or several maps in the same frame.
#' @param
#' @keywords construct maps
#' @export
#' @examples
#' hc_map()
#'

hc_map <- function(df, period  = c("1990-1992", "1991-1993", "1992-1994", "1993-1995", "1994-1996",
                                   "1995-1997", "1996-1998", "1997-1999", "1998-2000", "1999-2001",
                                   "2000-2002", "2001-2003", "2002-2004", "2003-2005", "2004-2006",
                                   "2005-2007", "2006-2008", "2007-2009", "2008-2010", "2009-2011",
                                   "2010-2012", "2011-2013", "2012-2014", "2013-2015")){
  df <- data.frame(df)
  if(ncol(df) == 1){
    return(unimap(as.matrix(df)))
  } else {
    return(multimap(df, period))
  }
}

