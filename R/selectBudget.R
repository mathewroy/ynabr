#' Select a budget
#' 
#' A function that asks the user to select the budget of interest
#' @name selectBudget
#' @keywords selectBudget
#' @param param.token Your YNAB API personal access token
#' @export
#' @import dplyr
#' @importFrom rlang .data

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

# Version: 0.0.1.0000
# selectBudget <- function() {
#   
#   ## Create df_budgets if it doesn't exist
#   if (exists("df_budgets") == FALSE) {
#     df_budgets <- getStartingData("budgets")
#   }
#   
#   print("Enter the number associated with the budgest of interest:")
#   print(as.list(df_budgets$name))
#   
#   input_budget_no <- readline(prompt = "Enter budget number: ") %>%
#     as.integer()
#   
#   if (!is.integer(input_budget_no)) {
#     stop("Input an integer")
#   }
#   
#   if (input_budget_no <= 0) {
#     stop("Input should be an integer greater than/equal to 1")
#   }
#   
#   budget_name <- as.character(df_budgets$name[input_budget_no])
#   
#   budget_id <- df_budgets %>% filter(.data$name == budget_name) %>% select(id) %>%
#     as.character()
#   
#   return(c(budget_id, budget_name))
# }

# Version: 0.1.1.0000
# Added new parameter for token environment, rename param.token to param.token.code and mytoken to mytoken.code
# Version: 0.1.0.0000
# Added parameter token
# Changed how budget names are printed
selectBudget <- function(param.token.code, param.token.env) {
  mytoken.code <- param.token.code
  mytoken.env <- param.token.env
  
  df_budgets <- getStartingData(i = "budgets", param.token.code = mytoken.code, param.token.env = mytoken.env)
  
  if (length(df_budgets$id) > 1) {
    print("Enter the number associated with the budgest of interest:")
    print(paste0(dplyr::row_number(df_budgets$name)," = ", df_budgets$name))
    
    input_budget_no <- readline(prompt = "Enter budget number: ") %>%
      as.integer()
    
    if (!is.integer(input_budget_no)) stop("Input an integer")
    
    if (input_budget_no <= 0) stop("Input should be an integer greater than/equal to 1")
    
    budget_name <- as.character(df_budgets$name[input_budget_no])
    
    budget_id <- df_budgets %>% filter(.data$name == budget_name) %>% select(id) %>%
      as.character()
    
    return(budget_id)
  } else if (length(df_budgets$id) == 1) {
    budget_id <- df_budgets$id[1]
    return(budget_id)  
  } else {
    print(paste0("No budgets in your YNAB account."))
    return(NULL)
  }
}
