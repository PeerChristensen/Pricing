## Analyse af optimering via QL
# Maj 2020
# Peer Christensen

library(tidyverse)
library(RODBC)
library(readxl)

query <- 
  "SELECT  [DateOrdered_Key]
  ,Customer_Key
	,case when DateOrdered_Key >= 20200406 and DateOrdered_Key <= 20200419 then 'Period1'
	      when DateOrdered_Key >= 20200427 and DateOrdered_Key <= 20200510 then 'Period2'
		  end as Period
      ,orders.Product_Key
	  ,products.isbn13
	  ,[1_GrossRevenue]
      ,[3_NetRevenue]
	  ,ProductSales
      ,[4_DB1]
      ,[5_DB2]
      ,[SubscriptionInit_Key]
      ,[SubscriptionPurchase_Key] -- 72515285 = ja, premium, 72515284 = ja, plus, 72515286 = nej
  FROM [EDW].[edw].[OrderFactCombined] orders
  inner join [EDW].[edw].[Product] products on products.Product_Key = orders.Product_Key

  where ((DateOrdered_Key >= 20200406 and DateOrdered_Key <= 20200419)
  or    (DateOrdered_Key >= 20200427 and DateOrdered_Key <= 20200510))
  and isbn13 != 'Other'"


credentials <- read_rds("credentials.rds")

channel <- odbcConnect(credentials[1],credentials[2],credentials[3])

df <- sqlQuery(channel,query) %>%
  as_tibble() %>%
  filter(SubscriptionInit_Key != -1)

df2 <- read_excel("Top50 24-4.xlsx")

isbns <- df2 %>% pull(isbn13)

df <- df %>%
  filter(isbn13 %in% isbns)

# antal transaktioner
transactions <- df %>%
  count(Period) %>%
  rename(transactions = n)

# den samlede omsætning, DB1, DB2
revenue <- df %>%
  group_by(Period) %>%
  summarise(
    Revenue = sum(ProductSales),
    DB1     = sum(`4_DB1`),
    DB2     = sum(`5_DB2`)
  )

# antal nye medlemmer
new_members <- df %>%
  filter(SubscriptionInit_Key == 1) %>%
  group_by(Period) %>%
  count() %>%
  rename(new_members = n)
  
# medlemskonvertering
conversion <- df %>%
  group_by(Period,SubscriptionInit_Key) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = SubscriptionInit_Key,values_from = n) %>%
  mutate(Premium_conv_rate = `1` / `0` * 100) %>%
  select(Period, Premium_conv_rate)

overview <- reduce(list(transactions,revenue,new_members,conversion),inner_join)

overview %>%
  select(-Period) %>%
  rownames_to_column() %>% 
  gather(var, value, -rowname) %>% 
  spread(rowname, value) %>%
  rename(Period1 = `1`, Period2 = `2`) %>%
  mutate(order = c(3,4,5,6,2,1)) %>%
  arrange(order) %>%
  select(-order)

overview %>%
  select(Period,transactions, Revenue, DB1,DB2) %>%
  mutate(Revenue = Revenue / transactions,
         DB1 = DB1 / transactions,
         DB2 = DB2 / transactions) %>%
  select(-Period, -transactions) %>%
  rownames_to_column() %>% 
  gather(var, value, -rowname) %>% 
  spread(rowname, value) %>%
  rename(Period1 = `1`, Period2 = `2`) %>%
  mutate(order = c(3,1,2)) %>%
  arrange(order) %>%
  select(-order)

# each book 

df_isbn <- df %>%
  group_by(isbn13,Period) %>%
  summarise(transactions = n(),
            revenue = sum(ProductSales),
            DB1 = sum(`4_DB1`),
            DB2 = sum(`5_DB2`))

# members
new_members_book <- df %>%
  filter(SubscriptionInit_Key == 1) %>%
  group_by(isbn13, Period) %>%
  count() %>%
  rename(new_members = n)

# book conv rate
conversion_book <- df %>%
  group_by(isbn13,Period,SubscriptionInit_Key) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = SubscriptionInit_Key,values_from = n) %>%
  mutate(Premium_conv_rate = `1` / `0` * 100) %>%
  select(isbn13,Period, Premium_conv_rate) %>%
  mutate(Premium_conv_rate = replace_na(Premium_conv_rate,0))


df_isbn <- df_isbn %>%
  left_join(new_members_book, by = c("isbn13","Period")) %>%
  left_join(conversion_book, by = c("isbn13","Period")) %>%
  mutate(new_members = replace_na(new_members,0)) %>%
  ungroup()

# Grafer

df_isbn_long <- df_isbn %>%
  select(-isbn13) %>%
  pivot_longer(-Period) 

df_isbn_long$name = factor(df_isbn_long$name, levels=c('transactions','revenue','DB1','DB2','new_members','Premium_conv_rate'))

df_isbn_long %>%
  ggplot(aes(Period,value,colour = Period)) +
  geom_jitter(colour = "darkgrey") +
  geom_boxplot(alpha=.4,outlier.shape = NA) +
  theme_minimal() +
  facet_wrap(~name,scales="free") +
  ggthemes::scale_colour_tableau() +
  theme(strip.text = element_text(size = 14),
        axis.text = element_text(size=12),
        strip.background = element_rect(fill="lightgrey",colour="lightgrey"),
        panel.spacing.y = unit(1,"cm"))



