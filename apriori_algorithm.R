library('arules')
library('arulesViz')
data(Groceries)
summary(Groceries)     
Groceries@itemInfo[1:10,]
apply(Groceries@data[,10:20],2,function(r) paste(Groceries@itemInfo[r,"labels"],collapse=", "))

itemsets<-apriori(Groceries,parameter=list(minlen=1,maxlen=1,support=0.02,target="frequent itemsets"))
summary(itemsets)                               # found 59 itemsets
inspect(head(sort(itemsets,by="support"),10))   # lists top 10

itemsets<-apriori(Groceries,parameter=list(minlen=2,maxlen=2,support=0.02,target="frequent itemsets"))
summary(itemsets)                               # found 61 itemsets
inspect(head(sort(itemsets,by="support"),10))   # lists top 10

itemsets<-apriori(Groceries,parameter=list(minlen=3,maxlen=3,support=0.02,target="frequent itemsets"))
summary(itemsets)                               # found 2 itemsets
inspect(head(sort(itemsets,by="support"),10))   # lists top 10

rules <-     apriori(Groceries,parameter=list(support=0.001,confidence=0.6,target="rules"))
summary(rules)            # finds 2918 rules
plot(rules)               # displays scatterplot
plot(rules@quality)        # displays scatterplot matrix
#Compute the 1/Support(Y) which is the slope
slope<-sort(round(rules@quality$lift/rules@quality$confidence,2))
#Display the number of times each slope appears in dataset
unlist(lapply(split(slope,f=slope),length))
#Display the top 10 rules sorted by lift
inspect(head(sort(rules,by="lift"),10))
#Find the rules with confidence above 0.9
confidentRules<-rules[quality(rules)$confidence>0.9] 
confidentRules       # set of 127 rules
#Plot a matrix-based visualization of the LHS v RHS of rules
#changed reorder from True to none since ture is not available, but the plot is same with different order
plot(confidentRules,method="matrix",measure=c("lift","confidence"),control=list(reorder='none'))
#Visualize the top 5 rules with the highest lift and plot them
highLiftRules<-head(sort(rules,by="lift"),5) 
plot(highLiftRules,method="graph",control=list(type="items"))
     