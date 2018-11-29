## ROUGH
## Three variable dataset - month x activity x amount
df_months_threevar  <- df_months %>%
  select(data.months.yearmo, data.months.income, data.months.activity) %>%
  gather(c("data.months.income", "data.months.activity"),
         key = "type",
         value = "dollars")



## Spending by payee x month
df_transactions_payee_grouped <- df_transactions %>%
  group_by(data.transactions.payee_name, data.transactions.yearmo) %>%
  summarize(activity = sum(data.transactions.amount))

## Spending by category x month
df_transactions_categories_grouped <- df_transactions %>%
  group_by(data.transactions.category_name, data.transactions.yearmo) %>%
  summarize(activity = sum(data.transactions.amount))

## Get detailed category names
df_subcategories <-
  lapply(df_categories$data.category_groups.categories, as.data.frame) %>% 
  bind_rows


## Plot monthly imcome and spending
ggplot(df_months_threevar,
       aes(x = data.months.yearmo, y = dollars / 1E3, fill = type)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_x_discrete(name = "Year-month") +
  scale_y_continuous(name = "Income", labels = scales::dollar)

## Plot monthly spending by category and month (as as of day of month)
## Create dataset of interest
df_of_interest <- df_transactions %>%
  filter(
    data.transactions.date >= as.Date("2018-01-01"),
    data.transactions.date <= as.Date(Sys.Date()),
    data.transactions.dayofmonth <= lubridate::day(Sys.Date()),
    str_detect(data.transactions.category_name, "Groceries")
  ) %>%
  group_by(data.transactions.category_name,
           data.transactions.yearmo) %>%
  summarize(activity = sum(data.transactions.amount))

## Create variables of interest
meanofinterest = mean(-1 * df_of_interest$activity / 1E3)
medianofinterest = median(-1 * df_of_interest$activity / 1E3)
sdofinterest = sd (-1 * df_of_interest$activity / 1E3)

## Create plot
ggplot(df_of_interest,
       mapping = aes(x = data.transactions.yearmo, y = -1 * activity / 1E3)) +
  geom_col() +
  geom_hline(yintercept = meanofinterest - (2 * sdofinterest)) +
  geom_hline(yintercept = meanofinterest - (1 * sdofinterest)) +
  geom_hline(yintercept = medianofinterest) +
  geom_hline(yintercept = meanofinterest + (1 * sdofinterest)) +
  geom_hline(yintercept = meanofinterest + (2 * sdofinterest)) +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_x_discrete(name = "Year-Month") +
  scale_y_continuous(name = "Spending", labels = scales::dollar)
