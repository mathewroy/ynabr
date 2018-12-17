#' Gets the following YNAB data for a pre-specified budget:
#'  "accounts", "categories", "months", "payees", "payee_locations", 
#'  "subcategories", "scheduled_transactions", "transactions"
#' @param i name of endpoint
#' @keywords getBudgetDetails
#' @examples
#' df_accounts <- getBudgetDetails("accounts")
#' df_categories <- getBudgetDetails("categories")
#' df_subcategories <- getBudgetDetails("subcategories")
#' df_months <- getBudgetDetails("months")
#' df_payees <- getBudgetDetails("payees")
#' df_payee_locations <- getBudgetDetails("payee_locations")
#' df_scheduled_transactions <- getBudgetDetails("scheduled_transactions")
#' df_transactions <- getBudgetDetails("transactions")
getBudgetDetails <- function(i) {
  
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
    
    df0 <- df %>%
      mutate(
        date = as.Date(date, "%Y-%m-%d"),
        yearmo = strftime(date, "%y-%m"),
        dayofmonth = lubridate::day(as.Date(date, "%Y-%m-%d")),
        category_name = trimws(gsub("[^[:alnum:][:space:][:punct:]]", "", category_name))
      )
    
    df <- df0 %>% 
      select(subtransactions) %>% 
      bind_rows() %>% 
      rename(subtransaction_id = "id", id = "transaction_id") %>%
      bind_rows(select(df, -subtransactions), .) %>%
      group_by(id) %>% 
      filter(all("Split (Multiple Categories)..." %in% category_name)) %>% 
      arrange(id, category_name) %>%
      fill(date, memo, cleared, approved, flag_color, account_id, account_name, payee_id, 
           import_id, deleted, server_knowledge, yearmo, dayofmonth, .direction = "down") %>% 
      left_join(x = ., y = select(ynabr:::getBudgetDetails("payees"), c(payee_id = "id", payee_name = "name")), 
                by = "payee_id") %>%
      left_join(x = ., y = select(ynabr:::getBudgetDetails("subcategories"), c(category_id = "id", category_name = "name")), 
                by = "category_id") %>%
      select(-c(payee_name.x, category_name.x), payee_name = "payee_name.y", category_name = "category_name.y") %>% 
      bind_rows(select(df, -subtransactions), .) %>% 
      filter(!grepl('^Split', category_name)) %>%
      arrange(desc(date))
  }
  
  if (k == "subcategories") {
    df <- lapply(df$subcategories, as.data.frame) %>% bind_rows %>%
      mutate(name = trimws(gsub("[^[:alnum:][:space:][:punct:]]", "", name)))
  }
  
  return(df)
}
