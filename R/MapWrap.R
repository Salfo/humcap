
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
