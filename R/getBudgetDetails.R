#' Get budget details
#' 
#' Gets the following YNAB data for a pre-specified budget:
#'  "accounts", "categories", "months", "payees", "payee_locations", 
#'  "subcategories", "scheduled_transactions", "transactions"
#' 
#' @param i name of endpoint (one of: "accounts", "categories", "months", "payees", "payee_locations", "subcategories", "scheduled_transactions", "transactions")
#' @keywords getBudgetDetails
#' @export
#' 
#' @examples
#' df_transactions <- getBudgetDetails("transactions")
getBudgetDetails <- function(i) {
  
  if (exists("budget_name_id") == FALSE) {
    budget_name_id <<- selectBudget()
  }
  
  valid_i <-  c("accounts", "categories", "months", "payees", "payee_locations", 
                "subcategories", "scheduled_transactions", "transactions")
  
  basepoint <- c("https://api.youneedabudget.com/v1")
  
  k  <-  ""
  
  if (!(i %in% valid_i)) {
    stop(paste0(c("Argument must be one of: ", valid_i), collapse = " ")) 
  }
  
  if (i == "subcategories") {
    i = "categories"
    k = "subcategories"
  }
  
  print(paste0(basepoint, "/budgets/", budget_name_id[1], "/", i))
  df <- getYNAB(paste0(basepoint, "/budgets/", budget_name_id[1], "/", i)) %>% 
    removeColumnprefix()
  
  if (i == "categories") {
    df <- df %>% rename(subcategories = categories)
  } else if (i == "months") {
    df <- df %>% mutate(month = lubridate::date(as.Date(month, "%Y-%m-%d")),
                        yearmo = strftime(month, "%y-%m"))
  } else if (i == "transactions") {
    
    df <- df %>%
      mutate(
        date = as.Date(date, "%Y-%m-%d"),
        yearmo = strftime(date, "%y-%m"),
        dayofmonth = lubridate::day(as.Date(date, "%Y-%m-%d")),
        category_name = trimws(gsub("[^[:alnum:][:space:][:punct:]]", "", category_name))
      )
    
    df <- getUnsplit(df)
    
  }
  
  if (k == "subcategories") {
    df <- lapply(df$subcategories, as.data.frame) %>% bind_rows %>%
      mutate(name = trimws(gsub("[^[:alnum:][:space:][:punct:]]", "", name)))
  }
  
  return(df)
}