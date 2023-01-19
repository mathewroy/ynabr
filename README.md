## YNAB meets R: ynabr

* **Update: 2023-01-19**

  Some functions may no longer function as expected as I haven't had the time to maintain this package.

* **What is ynabr?** 

  ynabr is a R package and a set of scripts that demonstrates how data from the popular budgeting software, You Need A Budget (YNAB), can be imported, analyzed, and visualized.

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

### Main Functions
* getStartingData(i, param.token)
* selectBudget(param.token)
* getBudgetDetails(i, param.token, param.budget)
* refreshTransactions(i, param.token, param.budget)

### Sample syntax
```r
# Load package
library(ynabr)

# Personal access token code
# Set full path of a one-lined .txt file containing token
token_txt_file <- "E:/Users/User/Documents/token.txt"
mytoken <- readChar(con = token_txt_file, nchars = file.info(token_txt_file)$size)
#mytoken <- "12342424242424242"

# Functions
mybudget <- selectBudget(param.token = mytoken)

# getStartingData (i can be one of: user or budgets)
df_budgets <- getStartingData(i = "budgets", param.token = mytoken)

# getBudgetDetails (i can be one of: accounts, categories, subcategories, months, payees, payee_locations, or transactions)
df_transactions <- getBudgetDetails("transactions", param.token = mytoken, param.budgetid = mybudget)

# refreshTransactions (param.dfname is the name of the existing data frame with transactional data)
df_transactions <- refreshTransactions(param.token = mytoken, param.budgetid = mybudget, param.dfname = df_transactions)

```

### Monthly spending tracker (Shiny App)
Tracks the month-to-date spending by budget category. 

#### Run on the web: [Monthly Spending Tracker](https://flash.shinyapps.io/spendingtracker/ "Monthly Spending Tracker") 

#### Run Local:

```r
# Load packages
library(ynabr)
library(shiny)
runGitHub(repo = "ynabr", username = "mathewroy", subdir = "scripts/monthly_spending")
```
