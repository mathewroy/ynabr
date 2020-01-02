#' Splits multiple category transactions stored in one row into individual rows
#' @param d name of transactions data frame of interest
#' @keywords getUnsplit
#' @export
#' @examples
#' df <- getUnsplit(df_transactions) 
getUnsplit <- function(d) {
  ifelse(
    test = any(grep('^Split', d[,c("category_name")], value = FALSE)), 
    yes = {e <- d[, c("subtransactions")] %>% 
      bind_rows() %>% 
      rename(subtransaction_id = "id", id = "transaction_id") %>%
      bind_rows(select(d, -subtransactions), .) %>%
      group_by(id) %>% 
      filter(all("Split (Multiple Categories)..." %in% category_name)) %>% 
      arrange(id, category_name) %>%
      tidyr::fill(date, memo, cleared, approved, flag_color, account_id, account_name, payee_id, 
           import_id, deleted, server_knowledge, yearmo, dayofmonth, .direction = "down") %>% 
      left_join(x = ., y = select(getBudgetDetails("payees"), c(payee_id = "id", payee_name = "name")), 
                by = "payee_id") %>%
      left_join(x = ., y = select(getBudgetDetails("subcategories"), c(category_id = "id", category_name = "name")), 
                by = "category_id") %>%
      select(-c(payee_name.x, category_name.x), payee_name = "payee_name.y", category_name = "category_name.y") %>% 
      bind_rows(select(d, -subtransactions), .) %>% 
      filter(!grepl('^Split', category_name)) %>%
      arrange(desc(date)) %>% as.data.frame()
    return(e)
    },
    no = {e <- d %>% select(-subtransactions) %>% arrange(desc(date)) %>% as.data.frame()
    return(e)
    }
  )
}
