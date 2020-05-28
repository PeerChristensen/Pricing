# ordreniveau

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
      ,orders.OrderID
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

df <- df %>%
  group_by(OrderID) %>%
  mutate(revenue = sum(ProductSales),
         DB1 = sum(`4_DB1`),
         DB2 = sum(`5_DB2`),
         n_products = n())

df2 <- read_excel("Top50 24-4.xlsx")

isbns <- df2 %>% pull(isbn13)

df <- df %>%
  filter(isbn13 %in% isbns)
