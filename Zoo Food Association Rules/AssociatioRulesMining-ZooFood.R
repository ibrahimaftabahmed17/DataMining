setwd('D:/UC/Sem 2/Flex 2/Data Mining 2')
install.packages('arules')
library(readxl)

data.food <- read_excel("food_4_association.csv",col_names = TRUE)
data.food
df = as.data.frame(data.food)
typeof(data.food)
library(arules)


df$TransID <- as.factor(df$TransID)

df$`Add CheeseFood`<- as.logical(df$`Add CheeseFood`)
class(df$BeerFood)

df_2 <- data.frame(lapply(df[,-c(1)], function(x) if(is.numeric(x)) { 
  return(as.logical(x))
} else {  
  return(x) 
}
), stringsAsFactors=FALSE)

df_2['TransID'] = df['TransID']
head(df_2)
class(df_2$TransID)
df_2[(is.na(df_2))] <- FALSE
basket <- as(df_2, "transactions")
head(basket)
View(basket)

summary(basket)
itemFrequencyPlot(basket, support = 0.1, cex.names=0.8)
basket_rules <- apriori(basket,parameter = list(sup = 0.003, conf = 0.9,target="rules"))
summary(basket_rules)
inspect((basket_rules))

inspect(subset(basket_rules, lift>5))

install.packages('arulesViz')
library('arulesViz')
plot(basket_rules)
#plot(basket_rules, interactive=TRUE)

plot(head(sort(basket_rules, by="lift"), 10), method = "graph")
plot(basket_rules, method="grouped")
