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
       col= c("#CCCCCC", "#999999", "#666666", "#000000"), border="white", lwd=0.4)
  text(15, 1.35, "legend")
  text(c(10,15,20), c(0.9,0.9,0.9), c("1/4","1/2","3/4"), cex=0.8) # Tick labels

}
