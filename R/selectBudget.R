#' A function that asks the user to select the budget of interest
#' @keywords selectBudget
#' @examples
#' budget_name_id <- selectBudget()
selectBudget <- function() {
  
  ## Create df_budgets if it doesn't exist
  if (exists("df_budgets") == FALSE) {
    df_budgets <- getStartingData("budgets")
  }
  
  print("Enter the number associated with the budgest of interest:")
  print(as.list(df_budgets$name))
  
  input_budget_no <- readline(prompt = "Enter budget number: ") %>%
    as.integer()
  
  if (!is.integer(input_budget_no)) {
    stop("Input an integer")
  }
  
  if (input_budget_no <= 0) {
    stop("Input should be an integer greater than/equal to 1")
  }
  
  budget_name <- as.character(df_budgets$name[input_budget_no])
  
  budget_id <- df_budgets %>% filter(name == budget_name) %>% select(id) %>%
    as.character()
  
  return(c(budget_id, budget_name))
}