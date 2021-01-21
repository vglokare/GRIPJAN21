#load the downloaded Libraries
library(readr)
library(ggplot2)

library(dplyr)

print(getwd())

# Now read in the data and explore the dataset

x <- read.csv("C:\\Users\\USER\\Desktop\\SPARK\\store.csv")

#print x

x

#Lets explore variable align

str(x)

#dimension of x

dim(x)

#view the full csv file

View(x)


#extracting city from store file

str(x$City)

#extracting postal.code from store file

str(x$Postal.Code)

#extracting ship.mode from store file

str(x$Ship.Mode)

#extracting segment from store file

str(x$Segment)

#city from store file

unique(x$Ship.Mode)

#extracting city from store file

unique(x$Region)

unique(x$Category)

summary(x$State)

summary(x$City)

#check the levels of the factor ship.mode

levels(x$Ship.Mode)

#Lets checks the frequency table for factor values

table(x$Ship.Mode)

table(x$Country)

# contingency table for state vs city)

table(x$City,x$Country)

# contingency table for Ship.Mode vs State)

table(x$Ship.Mode,x$State)

x = x %>%
  filter(Ship.Mode!= "First Class")

dim(x)

table(x$Ship.Mode)

"To Drop a level you have to first make the count Zero for that factor value
and then drop the level"

#filter out the rows with ship.mode not euqal to first class

x = x %>%
  filter(Ship.Mode!= "First Class")%>%droplevels()

#filter out the rows with city not euqal to first class

x
x = x %>%
  filter(City!= "First Class")%>%droplevels()

#filter out the rows with city not euqal to los angeles

x = x %>%
  filter(City!= "Los Angeles")

table(x$City)

#create contingency table for ship.mode vs city

table(x$Ship.Mode, x$City)

View(select(x, Ship.Mode,
            Profit,
            City,
            Segment))

View(select(x, Postal.Code,
            Discount,
            Country))

#create a side-by-side bar chart for city

ggplot(x, aes(x = City, col="red")) +
  geom_bar()

#create a side-by-side bar chart for ship.mode

ggplot(x, aes(x = Ship.Mode, col="green",fill="yellow")) +
  geom_bar()

#create a side-by-side bar chart for profit

ggplot(x, aes(x = Profit, col="green",fill="yellow")) +
  geom_bar()

#create a side-by-side bar chart for quantity

ggplot(x, aes(x = Quantity, col="green",fill="yellow")) +
  geom_bar()

#create a side-by-side bar chart for sales

ggplot(x, aes(x = Sales, col="green",fill="yellow")) +
  geom_bar()

#create a side-by-side bar chart for category

ggplot(x, aes(x = Category, col="green",fill="yellow")) +
  geom_bar()

#create a side-by-side bar chart for country

ggplot(x, aes(x = Country, col="yellow")) +
  geom_bar()

#create a side-by-side bar chart for state

ggplot(x, aes(x = State, col="pink")) +
  geom_bar(position="dodge")

#plot alignment for each state

ggplot(x, aes(x = Region, fill =State)) +
  geom_bar() +
  facet_wrap(~ State)

#plot alignment for each profit

ggplot(x, aes(x = Postal.Code, fill=Profit)) +
  geom_bar() +
  facet_wrap(~ Profit)

#plot alignment for each sales

ggplot(x, aes(x = Quantity, fill=Sales)) +
  geom_bar() +
  facet_wrap(~ Sales)

#plot alignment for each discount

ggplot(x, aes(x = Category, fill=Discount)) +
  geom_bar() +
  facet_wrap(~ Discount)

#plot alignment for each country

ggplot(x, aes(x = City, fill=Country)) +
  geom_bar() +
  facet_wrap(~ Country)

#create dotplot for segment

ggplot(x, aes(x = Segment)) +
  geom_dotplot()

#create dotplot for  sub.category

ggplot(x, aes(x = Sub.Category)) +
  geom_dotplot(dotsize = 0.4)

#create dotplot for  profit

ggplot(x, aes(x = Profit)) +
  geom_dotplot()

#create dotplot for  shipmode

ggplot(x, aes(x = Ship.Mode,fill="yellow")) +
  geom_dotplot()

#create dotplot for  postal.code

ggplot(x, aes(x = Postal.Code,col="red",fill="green")) +
  geom_dotplot()

#create dotplot for  sales

ggplot(x, aes(x = Sales,fill="blue")) +
  geom_dotplot()

#create dotplot for  category

ggplot(x, aes(x = Category)) +
  geom_dotplot()

#create histogram for  profit

ggplot(x, aes(x = Profit)) +
  geom_histogram()

#create histogram  for  sales

ggplot(x, aes(x = Sales)) +
  geom_histogram()

#create histogram  for  category

ggplot(x, aes(x = Category)) +
  geom_histogram()

#create histogram  for  segment

ggplot(x, aes(x = Segment)) +
  geom_histogram()

#create histogram  for  discount

ggplot( x, aes(x = Discount)) +
  geom_histogram()

#create histogram  for  quantity

ggplot(x, aes(x = Quantity)) +
  geom_histogram()

#create density  for  ship.mode

ggplot(x, aes(x = Ship.Mode)) +
  geom_density()

#create density  fo rcountry

ggplot(x, aes(x = Country)) +
  geom_density()

#create density  for  city

ggplot(x, aes(x = City)) +
  geom_density()

#create density  for  profit

ggplot(x, aes(x = Profit)) +
  geom_density()

#create boxplot  for  sales

ggplot(x, aes(x = 1, y = Sales)) +
  geom_boxplot()

#create boxplot for profit

ggplot(x, aes(x = 1, y = Profit)) +
  geom_boxplot()

#create boxplot for quantity

ggplot(x, aes(x = 1, y = Quantity)) +
  geom_boxplot()

