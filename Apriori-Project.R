library(readr)
library(dplyr)
library(tidyr)
library(arules)
library(arulesViz)
library(methods)

#Loading datasets prior products and products
ordr_pr <- read_csv("C:/Users/namra/Documents/737 Data/order_products__prior.csv")
prods <- read_csv("C:/Users/namra/Documents/737 Data/products.csv")

#Join between prior products and products dataset by product ID to create a common data for analysis
order_baskets <- ordr_pr %>% 
  left_join(prods, by="product_id") %>% 
  group_by(order_id) %>%
  summarise(basket = as.vector(list(product_name)))

# Converting the dataset into transactions for performing association mining.
transactions <- as(order_baskets$basket, "transactions")

#Number of items per basket visualization 
hist(size(transactions), breaks = 0:150, xaxt="n", ylim=c(0,250000), 
     main = "Number of Items per basket", xlab = "Number of Items")
axis(1, at=seq(0,160,by=10), cex.axis=0.8)
mtext(paste("Total:", length(transactions), "baskets,", sum(size(transactions)), "items"))

#Understanding basket size
basketSizes<-size(transactions)
summary(basketSizes)

#Finding which items in dataset are most frequent
item_frequencies <- itemFrequency(transactions)
sum(item_frequencies)

#Getting the frequency of products or count of products because sum of frequencies does not give a clear picture
productCount <- (item_frequencies/sum(item_frequencies))*sum(basketSizes)
summary(productCount)

#Finding the products with maximum count
orderedProducts <- sort(productCount, decreasing=T)
orderedProducts[1:10]

#Performing association mining
support<-0.01
itemsets <- apriori(transactions, parameter = list(target = "frequent itemsets", supp=support, minlen=2), control = list(verbose = FALSE))

#Plot of most frequent itemsets
par(mar=c(5,18,2,2)+.1)
sets_order_supp <- DATAFRAME(sort(itemsets, by="support", decreasing = F))
barplot(sets_order_supp$support, names.arg=sets_order_supp$items, xlim=c(0,0.02), horiz = T, las = 2, cex.names = .8, main = "Frequent Itemsets")
mtext(paste("support:",support), padj = .8)

product_baskets <- transactions[basketSizes > 1]

#Changing value of support and confidence
rules <- apriori(product_baskets, parameter = list(supp =  0.003269976, conf = 0.01, maxlen=3), control = list(verbose = FALSE)) 
#pot of rules
plot(rules)

# Rules as per lift value
inspect(sort(rules, by="lift")[1:5])

#Rules as per confidence to find rules which are most significant
top_rules = inspect(sort(rules, by="confidence")[1:10])

#Visualization of top rules
library(arulesViz)
plot(top_rules,method="paracoord")
plot(top_rules, method="graph", control=list(type="items"))
plot(top_rules, method="graph", control=list(layout=igraph::in_circle()))

#Before purchasing banana the most significant rules ordered by confidence are
rules2<-apriori(data=product_baskets, parameter=list(supp=0.0032,conf = 0.01), 
               appearance = list(default="lhs",rhs="Banana"),
               control = list(verbose=F))

rules2<-sort(rules2, decreasing=TRUE,by="confidence")
inspect(rules2[1:15])


#After purchasing Banana the most significant rules depicted by confidence are
rules3<-apriori(data=product_baskets, parameter=list(supp=0.0032,conf = 0.01), 
               appearance = list(default="rhs",lhs="Banana"),
               control = list(verbose=F))
rules3<-sort(rules3, decreasing=TRUE,by="confidence")
inspect(rules3[1:15])

datanew = read.csv(file.choose(), header=TRUE)
f = na.omit(datanew)
write.csv(f,"ndata.csv",  row.names = TRUE)

