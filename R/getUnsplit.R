#' Unsplits multiple category transactions
#' 
#' Splits multiple category transactions stored in one row into individual rows
#' @name getUnsplit
#' @param d name of transactions data frame of interest
#' @keywords getUnsplit
#' @export
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @importFrom rlang .data
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

# Version: 0.1.1.0000
# Added new parameter for token environment, rename param.token to param.token.code and mytoken to mytoken.code
getUnsplit <- function(d, param.token.code, param.token.env, param.budgetid) {
  ifelse(
    test = any(grep('^Split', d[,c("category_name")], value = FALSE)), 
    yes = {
      mytoken.code <- param.token.code
      mybudgetid <- param.budgetid
      mytoken.env <- param.token.env
      e <- d[, c("subtransactions")] %>% 
      bind_rows() %>% 
      rename(subtransaction_id = "id", id = "transaction_id") %>%
      bind_rows(select(.data = d, -.data$subtransactions), .) %>%
      group_by(id) %>% 
      filter(.data = ., all("Split (Multiple Categories)..." %in% .data$category_name)) %>% 
      arrange(.data = ., id, .data$category_name) %>%
      tidyr::fill(data = ., 
                  .data$date, .data$memo, .data$cleared, .data$approved, .data$flag_color,
                  .data$account_id, .data$account_name, .data$payee_id, .data$import_id, 
                  .data$deleted, .data$server_knowledge, .data$yearmo, .data$dayofmonth, 
                  .direction = "down") %>% 
      left_join(x = ., 
                y = select(.data = getBudgetDetails(i = "payees", param.token.code = mytoken.code, param.token.env = mytoken.env, param.budgetid = mybudgetid), 
                           c(payee_id = "id", payee_name = "name")), 
                by = "payee_id") %>%
      left_join(x = ., 
                y = select(.data = getBudgetDetails("subcategories", param.token.code = mytoken.code, param.token.env = mytoken.env, param.budgetid = mybudgetid), 
                           c(category_id = "id", category_name = "name")), 
                by = "category_id") %>%
      select(.data = ., -.data$payee_name.x, .data$category_name.x, payee_name = "payee_name.y", category_name = "category_name.y") %>% 
      bind_rows(select(.data = d, -.data$subtransactions), .) %>% 
      filter(.data = ., !grepl('^Split', x = .data$category_name)) %>%
      arrange(.data = ., desc(.data$date)) %>% as.data.frame()
    return(e)
    },
      no = {e <- d %>% select(.data = ., -.data$subtransactions) %>% 
        arrange(.data = ., desc(.data$date)) %>% as.data.frame()
    return(e)
    }
  )
}
