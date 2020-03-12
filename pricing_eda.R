# Pricing script 1


library(tidyverse)
library(RODBC)
library(readxl)
library(broom)

# 559 bøger
# 3 prisgrupper med hver deres prissætning. 
# 
# foretage en prissætning
# kigge på en anden opdeling end de nuværende 3 prisgrupper, 
# anvende ”conversion rate”, ”bogkategori (themakode), ”bogserie” (ja/nej)?
# andre måder at kategorisere bøgerne?
  
df <- read_excel("Price_recommendations1.xlsx")

#####
## query

query <- "
Select 
	tbl1.Product_Key,
	isbn13,
	n,
	category,
	case when Series_Key is null then 0 else 1 end as inSeries

from (
	SELECT TOP 1000
      [Product_Key]
	  ,count(Product_Key) as n

  FROM [EDW].[edw].[OrderFact] 
  where DateOrdered_Key > 20200301

  group by Product_Key

  order by n desc) tbl1

inner join (
	select Product_Key, 
		   isbn13 

	from [EDW].[edw].[Product]) tbl2 on tbl2.Product_Key = tbl1.Product_Key

inner join (
	select Product_Key, 
		   ThemaCategory_Key
	
	from [EDW].[edw].[ProductThemaCategoryFact]) tbl3 on tbl3.Product_Key = tbl2.Product_Key

inner join (
	select ThemaCategory_Key,Level1_Danish as dkCategory,
			CustomerAura_ProductCategoryGroup as category
			
	from [EDW].[edw].[ThemaCategory]) tbl4 on tbl4.ThemaCategory_Key = tbl3.ThemaCategory_Key

left join (
	select Product_Key, 
		   Series_Key
	
	from  [EDW].[edw].[ProductSeriesFact]) tbl5 on tbl5.Product_Key = tbl1.Product_Key

	order by n desc"

#####
#####
credentials <- read_rds("credentials.rds")

channel <- odbcConnect(credentials[1],credentials[2],credentials[3])

df2 <- sqlQuery(channel,query) %>% as_tibble() %>%
  select(isbn13,inSeries,LitGroup = category) %>%
  mutate(isbn13 = as.character(isbn13))

df <- df %>%
  left_join(df2,by="isbn13")

# markedposition
df %>%
  select(Group, pricing_position = `Pricing Position`, market_position) %>%
  mutate_at(vars(pricing_position,market_position),as.numeric) %>%
  pivot_longer(cols = ends_with("position")) %>%
  group_by(Group,name) %>%
  count(value) %>%
  #filter(name == "market_position") %>%
  ggplot(aes(value,n)) +
    geom_col() +
    facet_grid(Group ~name,scales = "free")

# markedposition ved conversionrate >= 10
df %>%
  mutate(conversionrate = as.numeric(conversionrate)) %>% 
  mutate(conversionLevel = if_else(conversionrate>=10,">= 10","< 10")) %>%
  select(conversionLevel,pricing_position = `Pricing Position`, market_position) %>%
  mutate_at(vars(pricing_position,market_position),as.numeric) %>%
  pivot_longer(cols = ends_with("position")) %>%
  group_by(conversionLevel,name) %>%
  count(value) %>%
  ggplot(aes(value,n)) +
  geom_col() +
  facet_grid(name~conversionLevel)

# conversion rate by LitGroup category
df %>%
  select(isbn13,conversionrate,LitGroup) %>%
  distinct() %>%
  mutate(conversionrate = as.numeric(conversionrate)) %>%
  ggplot(aes(conversionrate,fill=factor(LitGroup))) +
  geom_density(alpha=.4) +
  facet_wrap(~LitGroup,ncol=1)

df %>%
  select(isbn13,conversionrate,LitGroup) %>%
  mutate(conversionrate = as.numeric(conversionrate)) %>%
  group_by(LitGroup) %>%
  summarise(n = n(),
            avg = mean(conversionrate),
            med = median(conversionrate))

# conversion rate by series
df %>%
  select(isbn13,conversionrate,inSeries) %>%
  distinct() %>%
  mutate(conversionrate = as.numeric(conversionrate)) %>%
  ggplot(aes(conversionrate,fill=factor(inSeries))) +
  geom_density(alpha=.4) +
  facet_wrap(~inSeries,ncol=1)

df %>%
  select(isbn13,conversionrate,inSeries) %>%
  mutate(conversionrate = as.numeric(conversionrate)) %>%
  group_by(inSeries) %>%
  summarise(n = n(),
            avg = mean(conversionrate),
            med = median(conversionrate))

# conversion rate by Group
df %>%
  select(isbn13,conversionrate,Group) %>%
  mutate(conversionrate = as.numeric(conversionrate)) %>%
  ggplot(aes(conversionrate,fill=factor(Group))) +
  geom_density(alpha=.4) +
  facet_wrap(~Group,ncol=1)
  
df %>%
  select(isbn13,conversionrate,Group) %>%
  mutate(conversionrate = as.numeric(conversionrate)) %>%
  group_by(Group) %>%
  summarise(avg = mean(conversionrate),
            med = median(conversionrate))

df_lm <- df %>%
  select(Group,inSeries,LitGroup,conversionrate,pricing_position = `Pricing Position`,market_position) %>%
  mutate(Group = as.factor(Group),
         inSeries = as.factor(inSeries),
         LitGroup = fct_relevel(LitGroup,levels = c("Skønlitteratur", "Faglitteratur")),
         pricing_position = as.numeric(pricing_position),
         market_position = as.numeric(market_position),
         conversionrate = log(as.numeric(conversionrate)))

mod <- lm(conversionrate ~ .,data = df_lm)
summary(mod)
mod %>% tidy() %>% select(term,estimate) %>% mutate(estimate = (exp(estimate)-1)*100)
# fx conversion rate siger med 35% for faglitteratur
