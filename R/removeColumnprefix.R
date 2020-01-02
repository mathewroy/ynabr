#' Fix column names
#' 
#' Removes the prefix "data.{endpoint}." from column names in data frames created by getYNAB()
#' The assumption is that the name will follow the structure "data.{x}.something"
#' eg.  "data.{x}.something" > "something" or
#'      "data.something" > "something" or
#'      "something" > "something"
#' @param x an R object, specifically an endpoint data frame
#' @keywords removeColumnprefix
#' @export
#' @examples
#' df_transactions <- getYNAB("https://api.youneedabudget.com/v1/transactions") %>% removeColumnprefix()
removeColumnprefix <- function(x) {
  df_name <- deparse(substitute(x))
  
  col_oldnames <- colnames(x)[grepl("^data.", colnames(x))]
  
  list_col_newnames <- lapply(col_oldnames, function(y) {
    
    period_locs <- which(strsplit(y, "")[[1]] == ".")
    
    if (length(period_locs) > 1) {
      period_ofinterest_loc = period_locs[2]
    } else if (length(period_locs) == 1) {
      period_ofinterest_loc = period_locs[1]
    } else {
      period_ofinterest_loc = 0
    }
    
    col_newname <- substring(y, period_ofinterest_loc + 1)
    return(col_newname)
  })
  
  colnames(x)[grepl("^data.", colnames(x))] <- list_col_newnames
  return(as.data.frame(x))
}