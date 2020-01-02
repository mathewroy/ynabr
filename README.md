## YNAB meets R: ynabr

* **What is ynabr?** 

  ynabr is an R package and a set of scripts that demonstrates how data from the popular budgeting software, You Need A Budget (YNAB), can be imported, analyzed, and visualized.

* **How does it work?**

  The package and scripts rely on connecting to YNAB's Application Program Interface (API), and extracting the available data related to accounts, users, monthly budgets, payees, transactions etc... After the appropriate data is extracted, the scripts will focus on analyzing and visualizing such data (e.g. looking to see if month-to-day spending for any budget category is on track with past spending).
 
* **How do I install ynabr?**  
  ```r
  devtools::install_github("mathewroy/ynabr")  
  library(ynabr)  
  ```   
  Note that this requires the _devtools_ package.  
  
* **Who will use ynabr**

  This project is for any YNAB user interested in learning about their own budget data. Some basic level of R programming or an interest in learning R programming is necessary.
  
* **What is the goal of this project?**  

  The goal of this project is to enable YNAB users to get a prespective on their own budgets and spending activities. It also exists to illustrate how R can be used. I'm using it as an opportunity to do both.

### Functions
| Usage                                        | Description                                        |
|----------------------------------------------|----------------------------------------------------|
| `getStartingData("user")`                    | Returns the user associated with the token         |
| `getStartingData("budgets")`                 | Returns budgets associated with the user           |
| `getBudgetDetails("accounts")`               | Returns accounts associated with a selected budget |
| `getBudgetDetails("categories")`             | Returns the larger category groups                 |
| `getBudgetDetails("subcategories")`          | Returns the detailed budget categories             |
| `getBudgetDetails("months")`                 | Returns all budget months                          |
| `getBudgetDetails("payees")`                 | Returns all payees                                 |
| `getBudgetDetails("payee_locations")`        | Returns the latitude and longitude of each payee   |
| `getBudgetDetails("scheduled_transactions")` | Returns all scheduled transactions                 |
| `refreshTransactions()`                      | Returns all transactions                           |

### Sample syntax
```r
# Load packages
packages <- c("devtools","dplyr","httr","jsonlite","magrittr","tidyr", "ynabr")
sapply(packages, require, character.only = T)

# Authorization token code
auth_token <- "12342424242424242"

# Usage of functions
getEndpoints()
df_user <- getStartingData("user")
df_budgets <- getStartingData("budgets")
df_accounts <- getBudgetDetails("accounts")
df_categories <- getBudgetDetails("categories")
df_subcategories <- getBudgetDetails("subcategories")
df_months <- getBudgetDetails("months")
df_payees <- getBudgetDetails("payees")
df_payee_locations <- getBudgetDetails("payee_locations")
df_scheduled_transactions <- getBudgetDetails("scheduled_transactions")
df_transactions <- refreshTransactions() # OR
df_transactions <- getBudgetDetails("transactions")
```

### Monthly spending tracker (Shiny App)
Tracks the month-to-date spending by budget category.
```r
# Load packages
packages <- c("devtools","dplyr","ggplot2","htmltools","httr","jsonlite",
              "magrittr","plotly","shiny","tidyr", "ynabr")
sapply(packages, require, character.only = T)

runGitHub(repo = "ynabr", username = "mathewroy", subdir = "scripts/monthly_spending")
```