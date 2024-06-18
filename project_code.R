install.packages("readxl")
library(readxl)


#install.packages("shiny")
library(shiny)
#install.packages("data.table")
library(data.table)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("dplyr")
library(dplyr)
#install.packages("arules")
library(arules)
#install.packages("arulesViz")
library(arulesViz)

Orignal_Data=read.csv("C:/Users/ZBOOK STUDIO G5/Desktop/CS/Semester 2/Introduction to Data sincse/project DS/grc.csv")

data<- data.frame(Orignal_Data)
data

str(data)

#---------------------------- DATA CLEANING ------------------------

#Duplicates -> 2 duplicates:

duplicated(data)

sum(duplicated(data))

df= unique(data)

sum(duplicated(df))


#Missing value -> 0:

sum(is.na(df))


#Data structure ( total is converted from integer into numeric for more accuracy):

is.numeric(df$count)

is.integer(df$total)

df$total= as.numeric(df$total)

is.numeric(df$rnd)

df$rnd= as.numeric(df$rnd)

is.numeric(df$age)

df$age = as.numeric(df$age)

str(df)




summary(df)

#---------------------------------------------(Comapre the payments type)----------------------------------------
cash_t = 0
credit_t = 0
df = Orignal_Data
for(i in 1:9835){
  if(df$paymentType[i] == "Cash"){
    cash_t  = cash_t + df$total[i]
  } else if (df$paymentType[i] == "Credit") {
    credit_t = credit_t + df$total[i]
  }
}


Slices = c(cash_t , credit_t)
names = c("Cash" , "Credit")
pie(Slices, labels = names,main ="Type of Payemnt" , col = c("green" , "black") )
legend("bottomright", legend =c("Cash" =paste0("Cash = " ,cash_t),"Credit" =paste0("Credit = " ,credit_t)), fill = c("green","black"))


#-----------------------------------------------------------------------------

install.packages("dplyr")
library("dplyr")


#-------------------------------------------(Compare each age and sum of total spending. )----------------------------

df<- data.frame(items=Orignal_Data$items ,
                num_items=as.numeric(Orignal_Data$count),
                money_spend = as.numeric(Orignal_Data$total),
                rnd = as.integer(Orignal_Data$rnd),
                names = Orignal_Data$customer,
                age = as.numeric(Orignal_Data$age),
                city = Orignal_Data$city,
                type  = Orignal_Data$paymentType)

total_spending <- df %>%
  group_by(age) %>%
  summarize(total_money_spend = sum(money_spend))


barplot(total_spending$total_money_spend~total_spending$age,col="darkred",border = "cyan",main = "Sum of total spending by Age",
        xlab="Age" ,
        ylab="total spending",
        ylim= c(0,2000000))


#--------------------------------------------(compeer between Sum of Total Spending by City)--------------


df<- data.frame(items=Orignal_Data$items ,
                num_items=as.numeric(Orignal_Data$count),
                money_spend = as.numeric(Orignal_Data$total),
                rnd = as.integer(Orignal_Data$rnd),
                names = Orignal_Data$customer,
                age = as.numeric(Orignal_Data$age),
                city = Orignal_Data$city,
                type  = Orignal_Data$paymentType)

city_total <- df %>%
  group_by(city) %>%
  summarise(total_spending_city = sum(money_spend)) 

# Extract total spending as a vector
total_spending_vector <- city_total$total_spending_city


total_spending_sorted <- sort(total_spending_vector, decreasing = FALSE)


city_v =c("Alexandria","Cairo","Hurghada","Sohag","Dakahlia","Port Said","Giza","Gharbia","Fayoum","Aswan")

city_vs<- sort(city_v, decreasing = TRUE)



barplot(total_spending_sorted~city_vs,
        col = "darkred",
        border = "black",
        main = "Sum of Total Spending by City",
        xlab = "City",
        ylab = "Total Spending",
        ylim = c(0, 3000000),
        name = c("Alexandria","Cairo","Hurghada","Sohag","Dakahlia","Port Said","Giza","Gharbia","Fayoum","Aswan"))
#--------------------------------------------------------------------------------------------------------------------------








#-------------------------------------------------------( distribution of total spending )----------


h <- c(df$money_spend)

plot(density(h),
     main = "Density Plot of Total Spending",
     xlab = "Total Spending",
     ylab = "Density",
     col = "skyblue" )

#or
boxplot(h , xlab = "total spend" , main = "Distribution of total spending" )
#-----------------------------------------------------------------------------------------







#----------------------------------------------------------K means-------------------------------------------------------------------------------------------------------------------

total_spend = c(x22 , x23, x25,x29, x30, x35, x36,x37,x39,x50,x55,x60)


dfk5 <- data.frame(  names = Orignal_Data$customer, 
                     age = as.numeric(Orignal_Data$age),
                     money_spend = as.numeric(Orignal_Data$total), 
                     cluster = 0)

n = as.numeric(readline("Enter number of Clusters"))

Kmean_clustring1 <- kmeans(total_spend, centers = n)

for(i in 1:9835){
  if(dfk5$age[i] == 22){
    dfk5$cluster[i] =  Kmean_clustring1$cluster[1]
  }else if(dfk5$age[i] == 23){
    dfk5$cluster[i] =  Kmean_clustring1$cluster[2]
  }else if(dfk5$age[i] == 25){
    dfk5$cluster[i] =  Kmean_clustring1$cluster[3]
  }else if(dfk5$age[i] == 29){
    dfk5$cluster[i] =  Kmean_clustring1$cluster[4]
  }else if(dfk5$age[i] == 30){
    dfk5$cluster[i] =  Kmean_clustring1$cluster[5]
  }else if(dfk5$age[i] == 35){
    dfk5$cluster[i] =  Kmean_clustring1$cluster[6]
  }else if(dfk5$age[i] == 36){
    dfk5$cluster[i] =  Kmean_clustring1$cluster[7]
  }else if(dfk5$age[i] == 37){
    dfk5$cluster[i] =  Kmean_clustring1$cluster[8]
  }else if(dfk5$age[i] == 39){
    dfk5$cluster[i] =  Kmean_clustring1$cluster[9]
  }else if(dfk5$age[i] == 50){
    dfk5$cluster[i] =  Kmean_clustring1$cluster[10]
  }else if(dfk5$age[i] == 55){
    dfk5$cluster[i] =  Kmean_clustring1$cluster[11]
  }else if(dfk5$age[i] == 60){
    dfk5$cluster[i] =  Kmean_clustring1$cluster[12]
  }} 



opop = data.frame(  names = Orignal_Data$customer, 
                    age = as.numeric(Orignal_Data$age),
                    money_spend = as.numeric(Orignal_Data$total))

print(opop$money_spend)
x <- opop[, c("age" , "money_spend")]
#print(x)
kmean_t1 <- kmeans(x , centers = n)
kmean_t1

opop$cluster = kmean_t1$cluster




selected_data <- df[,c( "age", "money_spend")]
Kmean_result <- kmeans(selected_data,4)
df$cluster <- Kmean_result$cluster


#-------------------------------------------------------------------------


#--------------------------------------------------( association rules)-------------------------
install.packages("arules")
library(arules)


tdata=as(df,"transactions")
list_of_items=strsplit(as.character(df$items),split = ",")
transactions=as(list_of_items,"transactions")
print(transactions)

inspect(transactions)


min_supp = as.numeric(readline("enter your minimum Support"))
min_con = as.numeric(readline("Enter your minimum Confidence"))

apriori_items <- apriori(transactions, parameter = list(supp = min_supp, conf = min_conf))
print(apriori_items)
inspect(apriori_items)
#-----------------------------------------------------------------------







#bar plot
#barplot(data,xlab = ,ylab = ,main = ,col = ,names= c(,,,,,,))

#scater plot
#plot(x,y,xlab=,ylab=,main=)

#pie chart
#pie(x=c(numeric values),labels =c() ,main = ,col = )

#histogram
#hist(x=c(),col = ,border = , main =,xlab =,ylab =)

#box plot
#boxplot(x=c(),xlab,ylab,)

#sec6
#k=kmeans(data frame, centers=2)
#print(k)



#sec7
#Apriori Algorithm
# Install packages
install.packages("arules")
# Using arules package
library(arules)
tdata <- read.transactions("F:/Job/ANU/DS/Sections/apriori.txt", sep=",")
inspect(tdata)
apriori_rules <- apriori(tdata, parameter = list(supp = 0.4, conf = 0.7, minlen=2))


#sec8

barplot( 
  height =data$salary, 
  name =data$employeeID, 
  col = "skyblue", 
  main = "Compare employees salaries", 
  xlab = "emploee ID", 
  ylab = "Salary")


boxplot( x = data$salary, 
         main = "Distribution employees salaries", 
         xlab = "Salary")

                          #حواف العواميد
hist(data$age, col="red", border="blue", main= "Age frequency", xlab="Age" , 
     ylab="frequency")



x = table(data$gender)
percentage=paste0(round(100*x/sum(x)),"%")
pie(x, labels = percentage, main = "Compare genders by count", col=c("pink","lightblue"))
# add more information by using legend
legend("bottomright", legend = c("female", "male"), fill = c("pink", "lightblue"))


#scatter plot
plot(x = data$age,
     y = data$salary, 
     main = "salary vs. age", 
     xlab = "age", 
     ylab = "salary", 
     col="blue" )

#sec9

# Install packages
install.packages("rpart")
install.packages("rpart.plot")
# Load to create the tree
library(rpart)
#used to plot the tree
library(rpart.plot)

# read data
training_data <- read.csv('decisionTree.csv')
#print data and summary as dataframe
head(training_data)
class(training_data)

tree<- rpart(Computer ~., data = Data ,minsplit = 2)

rpart.plot(tree)

# create new dataframe with the input columns
Data_to_predict <- data.Frame(age = “from 31 to 40",income ="high“
                              , student = "not student",
                              credit_range = "fair")
# to predict the output 
predict(tree, newdata = data_to_predict)




