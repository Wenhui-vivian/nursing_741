## ------------------------------------------------------------------------
names  <- c("P1","P2","P3","P4","P5")
temp   <- c(98.2,101.3,97.2,100.2,98.5)
pulse  <- c(66,72,83,85,90)
gender <- c("M","F","M","M","F")


## ------------------------------------------------------------------------
# Get the first element of temp
temp[1]

# Get elements 3,4, and 5 from pulse
pulse[3:5]

# series of numbers and composite functions

seq(1,16,4)

# Looks like f(x)
rnorm(5)

# Looks like f(g(x))
hist(rnorm(1000))
hist(rnorm(1000000))

# Whoa ! What is this ? We'll get to this
rnorm(1000000) %>% hist()

## ------------------------------------------------------------------------
temp < 98


## ------------------------------------------------------------------------
temp[temp < 98]


## ------------------------------------------------------------------------
(my_df <- data.frame(names,temp,pulse,gender)) 


## ------------------------------------------------------------------------
# Get the temp column
my_df$temp

# What's the mean of the temp column
mean(my_df$temp)


## ----eval=FALSE----------------------------------------------------------
## AirPassengers           Monthly Airline Passenger Numbers 1949-1960
## BJsales                 Sales Data with Leading Indicator
## BOD                     Biochemical Oxygen Demand
## CO2                     Carbon Dioxide Uptake in Grass Plants
## ChickWeight             Weight versus age of chicks on different diets
## DNase                   Elisa assay of DNase
## Formaldehyde            Determination of Formaldehyde
## HairEyeColor            Hair and Eye Color of Statistics Students
## Harman23.cor            Harman Example 2.3
## Harman74.cor            Harman Example 7.4
## Indometh                Pharmacokinetics of Indomethacin
## InsectSprays            Effectiveness of Insect Sprays
## JohnsonJohnson          Quarterly Earnings per Johnson & Johnson Share
## LakeHuron               Level of Lake Huron 1875-1972
## LifeCycleSavings        Intercountry Life-Cycle Savings Data
## Loblolly                Growth of Loblolly pine trees
## Nile                    Flow of the River Nile
## Orange                  Growth of Orange Trees
## OrchardSprays           Potency of Orchard Sprays
## PlantGrowth             Results from an Experiment on Plant Growth
## Puromycin               Reaction Velocity of an Enzymatic Reaction
## Theoph                  Pharmacokinetics of Theophylline


## ----eval=FALSE----------------------------------------------------------
## The data was extracted from the 1974 Motor Trend US
## magazine, and comprises fuel consumption and 10 aspects
## of automobile design and performance for 32 automobiles
## (1973–74 models).
## 
## A data frame with 32 observations on 11 (numeric)
## variables.
## 
## [, 1]	mpg	Miles/(US) gallon
## [, 2]	cyl	Number of cylinders
## [, 3]	disp	Displacement (cu.in.)
## [, 4]	hp	Gross horsepower
## [, 5]	drat	Rear axle ratio
## [, 6]	wt	Weight (1000 lbs)
## [, 7]	qsec	1/4 mile time
## [, 8]	vs	Engine (0 = V-shaped, 1 = straight)
## [, 9]	am	Transmission (0 = automatic, 1 = manual)
## [,10]	gear	Number of forward gears
## [,11]	carb	Number of carburetors


## ------------------------------------------------------------------------
data(mtcars)
mtcars


## ----cars----------------------------------------------------------------
plot(mpg ~ wt, data=mtcars)


## ------------------------------------------------------------------------
(mylm <- lm(mpg ~ ., data = mtcars))


## ------------------------------------------------------------------------
str(mtcars)


## ------------------------------------------------------------------------
# how many rows
nrow(mtcars) 

# how many columns
ncol(mtcars) 

# Column names
names(mtcars)



## ------------------------------------------------------------------------
head(mtcars,5) # First 5 rows
tail(mtcars,3) # Last 3 rows


## ------------------------------------------------------------------------
mtcars[1,]     # First row, all columns

## ------------------------------------------------------------------------
mtcars[1:3,]   # First three rows, all columns



## ------------------------------------------------------------------------
# All rows, and first 4 columns
mtcars[,1:4]   



## ------------------------------------------------------------------------
# Rows 1-5 and columns 1,2 and 8-10
mtcars[1:4,c(1:2,8:10)]


## ------------------------------------------------------------------------
# Rows 1-5 and columns 1,2 and 8-10
mtcars[1:4,c(1:2,8:10)]


## ------------------------------------------------------------------------
# Rows 1-5 and columns by name
mtcars[1:4,c("mpg","wt","drat")]


## ------------------------------------------------------------------------
mtcars$mpg > 11 & mtcars$wt < 2.0


## ------------------------------------------------------------------------
mtcars[mtcars$mpg > 11 & mtcars$wt < 2.0,]


## ------------------------------------------------------------------------
nrow(mtcars[mtcars$mpg > 11 & mtcars$wt < 2.0,])


## ------------------------------------------------------------------------
mtcars[mtcars$cyl == 4,]


## ------------------------------------------------------------------------
mtcars[mtcars$mpg > mean(mtcars$mpg),]


## ------------------------------------------------------------------------
mtcars[mtcars$mpg > quantile(mtcars$mpg)[4],]


## ----eval=F--------------------------------------------------------------
## url <- "https://raw.githubusercontent.com/steviep42/utilities/master/data/mtcars_na.csv"
## (mtcars_na <- read.csv(url, stringsAsFactors = FALSE))


## ------------------------------------------------------------------------
mean(mtcars_na$wt)


## ------------------------------------------------------------------------
mean(mtcars$wt, na.rm=TRUE)


## ------------------------------------------------------------------------
complete.cases(mtcars_na)


## ------------------------------------------------------------------------
# How many rows in the df do not contain any NAs ?
sum(complete.cases(mtcars_na))

# How many rows in the df do contain at least one NA ?
sum(!complete.cases(mtcars_na))


## ------------------------------------------------------------------------
mtcars_na[complete.cases(mtcars_na),]


## ------------------------------------------------------------------------
mtcars_na[!complete.cases(mtcars_na),]


## ------------------------------------------------------------------------
na.omit(mtcars_na)


## ------------------------------------------------------------------------
sapply(mtcars, function(x) length(unique(x)))


## ------------------------------------------------------------------------
summary(mtcars$am)


## ------------------------------------------------------------------------
summary(mtcars$am)


## ------------------------------------------------------------------------
mtcars$am <- factor(mtcars$am, 
                    levels = c(0,1), 
                    labels = c("Auto","Man") )


## ------------------------------------------------------------------------
summary(mtcars$am)


## ----echo=FALSE----------------------------------------------------------
suppressMessages(library(tidyverse))


## ------------------------------------------------------------------------
ggplot(mtcars,aes(x=wt,y=mpg)) + 
  geom_point() + 
  facet_wrap(~am)


## ------------------------------------------------------------------------
order(mtcars$mpg)


## ------------------------------------------------------------------------
mtcars[order(mtcars$mpg),]


## ------------------------------------------------------------------------
head(mtcars[rev(order(mtcars$mpg)),])


## ------------------------------------------------------------------------
url <- "https://raw.githubusercontent.com/pittardsp/bios545r_spring_2018/master/SUPPORT/hsb2.csv"

data1 <- read.csv(url,header=T,sep=",")

head(data1)


## ----eval=FALSE----------------------------------------------------------
## install.packages("tidyverse")


## ------------------------------------------------------------------------
suppressMessages(library(tidyverse))


## ------------------------------------------------------------------------
df <- data.frame(id = 1:5,
                 gender = c("MALE","MALE","FEMALE","MALE","FEMALE"),
                 age = c(70,76,60,64,68))


## ------------------------------------------------------------------------
filter(df,gender == "FEMALE")

# Given this data frame, the following is equivalent 

filter(df, gender != "MALE")


## ------------------------------------------------------------------------
filter(df, id %in% c(1,3,5))


## ------------------------------------------------------------------------
mutate(df,meanage = mean(age))


## ------------------------------------------------------------------------
mutate(df,old_young=ifelse(df$age>=mean(df$age),"Y","N"))


## ------------------------------------------------------------------------
tmp <- mutate(df, color = ifelse(age > mean(age),"red","blue")) 

plot(tmp$age,col=tmp$color, type="p",
     pch=19,main="Ages",ylab="Age") 

grid()

abline(h=mean(tmp$age),lty=2)

legend("topright",
       c("Above Avg","Below Avg"),col=c("red","blue"),pch=19)


## ------------------------------------------------------------------------
df[rev(order(df$age)),]


## ------------------------------------------------------------------------
arrange(df,desc(age))


## ------------------------------------------------------------------------
arrange(df, gender,desc(age))


## ------------------------------------------------------------------------
df[order(df$gender,-df$age),]


## ------------------------------------------------------------------------
# Reorder the columns
select(df,gender,id,age)  


## ------------------------------------------------------------------------
# Select all but the age column
select(df,-age)


## ------------------------------------------------------------------------
# Can use the ":" operator to select a range
select(df,id:age) 


## ------------------------------------------------------------------------
# Select all columns that start with an "a"
select(df,starts_with("a"))


## ------------------------------------------------------------------------
names(mtcars)

# Get only columns that start with "c"

select(mtcars,starts_with("c"))


## ------------------------------------------------------------------------
testdf <- expand.grid(m_1=seq(60,70,10),
                      age=c(25,32),
                      m_2=seq(50,60,10),
                      m_3=seq(60,70,10))


## ------------------------------------------------------------------------
testdf


## ------------------------------------------------------------------------
select(testdf,matches("_"))


## ------------------------------------------------------------------------
select(testdf,num_range("m_",1:2))


## ------------------------------------------------------------------------
df

# Hmm. the following doesn't do anything - or so it seems

group_by(df)


## ------------------------------------------------------------------------
summarize(group_by(df,gender),total=n())


## ------------------------------------------------------------------------
summarize(group_by(df,gender),av_age=mean(age))


## ------------------------------------------------------------------------
summarize(group_by(df,gender),av_age=mean(age),total=n())


## ------------------------------------------------------------------------
summarize(group_by(df,gender),av_age=mean(age))


## ------------------------------------------------------------------------
head(select(mtcars, mpg, am))


## ------------------------------------------------------------------------
mtcars %>% select(mpg, am) %>% head


## ------------------------------------------------------------------------
mtcars %>% select(mpg, am)


## ------------------------------------------------------------------------
df %>% group_by(gender) %>% summarize(avg=mean(age))

# Same as the following but the pipes don't require you to "commit"
# With the following, you have to know in advance what you want to do

summarize(group_by(df,gender), avg=mean(age))


## ------------------------------------------------------------------------
df %>% 
  group_by(gender) %>% 
  summarize(avg=mean(age),total=n())


## ------------------------------------------------------------------------
df %>% 
  filter(gender == "MALE") %>% 
  summarize(med_age=median(age))


## ------------------------------------------------------------------------
results <- df %>% 
              filter(gender == "MALE") %>% 
              summarize(med_age=median(age))


## ------------------------------------------------------------------------
mtcars %>% filter(wt > 3.3)  %>%
           mutate(ab_be=ifelse(mpg > mean(mpg),"Y","N")) %>%
           group_by(ab_be) %>% 
           summarize(mean_mpg=mean(mpg))


## ------------------------------------------------------------------------
mtcars %>% filter(wt > 3.3)  %>%
           mutate(ab_be=ifelse(mpg > mean(mpg),"Y","N")  ) %>%
           group_by(ab_be) %>% summarize(mean_mpg=mean(mpg)) %>%
           ggplot(aes(x=ab_be,y=mean_mpg)) +
           geom_bar(stat="identity") +
           ggtitle("Mean MPG") + labs(x = "ab_be", y = "Mean MPG")


## ------------------------------------------------------------------------
data(iris)


## ------------------------------------------------------------------------
str(iris)


## ------------------------------------------------------------------------
glimpse(iris)


## ------------------------------------------------------------------------
iris %>% 
  filter(Sepal.Length > 4.7 & Sepal.Length < 5.0 & Species=="setosa" )


## ------------------------------------------------------------------------
iris %>% select(c(Sepal.Length,Sepal.Width)) %>% head(5)

# Or use a helper function to process strings

iris %>% select(starts_with("Sepal")) %>% head(5)

# Or if you know which column numbers you want

iris %>% select(c(1:2)) %>% head(5)


## ------------------------------------------------------------------------
iris %>% arrange(desc(Sepal.Width)) %>% head(5)


## ------------------------------------------------------------------------
iris %>% group_by(Species) %>% count()

# Or

iris %>% group_by(Species) %>% summarize(total=n())


## ------------------------------------------------------------------------
iris %>% select(-ends_with("Length")) %>% head()


## ------------------------------------------------------------------------
iris %>% select(-c(ends_with("Length"),"Species")) %>% head()

# Or

iris %>% select(-ends_with("Length")) %>% select(-"Species") %>% head()


## ------------------------------------------------------------------------
iris %>% filter(Sepal.Width > 3.9) %>% 
  select(-ends_with("Length")) %>% 
  select(-"Species")

# Or

iris %>% select(-ends_with("Length")) %>% 
  select(-"Species") %>%
  filter(Sepal.Width > 3.9) 


## ------------------------------------------------------------------------
iris %>% summarize(mean=mean(Sepal.Length),
                   sd=sd(Sepal.Length),
                   max=max(Sepal.Length),
                   min=min(Sepal.Length))


## ------------------------------------------------------------------------
iris %>% group_by(Species) %>% summarize(mean=mean(Sepal.Length),
                                         sd=sd(Sepal.Length),
                                         max=max(Sepal.Length),
                                         min=min(Sepal.Length))


## ------------------------------------------------------------------------
inventory <- data.frame(part_num=c("001","002","003"),
                    description=c("Indispensable Widget",
                                  "Flux Capacitor",
                                  "Radiator"),
                    price=c(20,25,15),stringsAsFactors = FALSE)
                    
sales <- data.frame(part_num=c("001","001","001","003","110"),
                    quantity_sold=c(23,100,44,98,98),
               sales_regions=c("east","west","north","north","south"), stringsAsFactors = FALSE)


## ------------------------------------------------------------------------
inventory


## ------------------------------------------------------------------------
sales


## ------------------------------------------------------------------------
full_join(inventory,sales)


## ------------------------------------------------------------------------
inner_join(inventory,sales)


## ------------------------------------------------------------------------
left_join(inventory,sales)


## ------------------------------------------------------------------------
right_join(inventory,sales)


## ------------------------------------------------------------------------
data(msleep)


## ------------------------------------------------------------------------
names(msleep)

#

str(msleep)


## ----echo=FALSE----------------------------------------------------------
msleep %>% filter(vore=="omni") %>% summarize(mean=mean(sleep_total))

# Or

msleep %>% group_by(vore) %>% 
  summarize(mean=mean(sleep_total)) %>% 
  filter(vore=="omni")


## ----echo=FALSE----------------------------------------------------------
msleep %>% 
  group_by(order) %>% 
  summarize(avg=mean(sleep_total)) %>%
  arrange(desc(avg))


## ----echo=FALSE----------------------------------------------------------
msleep %>% group_by(vore) %>% 
  summarize(mean=mean(sleep_total))


## ----echo=FALSE----------------------------------------------------------
msleep %>% 
  drop_na() %>% 
  group_by(vore) %>% 
  summarize(mean=mean(sleep_total))


## ----echo=FALSE----------------------------------------------------------
msleep_na <- msleep %>% drop_na()
msleep_na %>% nrow()


## ----echo=FALSE----------------------------------------------------------
msleep_na %>% 
  group_by(order,vore) %>% 
  summarize(mean=mean(brainwt))


## ----eval=FALSE----------------------------------------------------------
## url <- "https://raw.githubusercontent.com/steviep42/nursing_741/master/chi_crimes.csv"
## 
## download.file(url,"chi_crimes.csv")


## ------------------------------------------------------------------------
chi <- read_csv("chi_crimes.csv")


## ------------------------------------------------------------------------
glimpse(chi)


## ------------------------------------------------------------------------
chi %>% summarize_all(n_distinct)


## ------------------------------------------------------------------------
suppressMessages(library(lubridate))
# Right now the dates and times are just a character string

str(chi$Date)  

# Note that the dates have a PM or AM string. Let's exploit 
# that information to create a category that captures this information.

chi <- chi %>% mutate(am_pm=ifelse(grepl("PM",Date),"PM","AM")) 

# Let's turn them into actual dates and times using the lubridate package

chi$Date <- parse_date_time(chi$Date,'%m/%d/%Y %I:%M:%S %p')

range(chi$Date)


## ------------------------------------------------------------------------
# List the first 5 records from the chi data table
chi$Date %>% head

# This will tell us what day of the week the given date and time represents
chi$Date %>% weekdays %>% head

# This will tell us what month the date and time falls into
chi$Date %>% months %>% head

# Let's add a factor to the data table/data frame that gives the month
chi <- chi %>% mutate(Month=months(Date)) 

unique(chi$Month)


## ------------------------------------------------------------------------
chi <- chi %>% mutate(quarter=quarter(Date)) 

chi <- chi %>% mutate(weekdays=weekdays(Date))

chi %>% select(Month, am_pm, weekdays, quarter) %>% head()


## ------------------------------------------------------------------------
# This gives us a table of Arrests vs non-Arrests

chi %>% count(Arrest)

# And we could see the count of reported crimes according to FBI code

chi %>% count(`FBI Code`) %>% arrange(desc(n))


## ------------------------------------------------------------------------
chi %>% 
  count(Arrest,am_pm) %>% 
  filter(Arrest==TRUE) %>% 
  arrange(desc(n))


## ------------------------------------------------------------------------
chi %>% count(Arrest,Month) %>% 
  ggplot(aes(x=Month,y=n,fill=Arrest)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=45)) + 
  ylab("Calls To Police") +
  ggtitle("Chicago Arrests per Month in 2013")


## ------------------------------------------------------------------------
chi %>% group_by(Arrest,Month) %>% 
  summarize(total=n()) %>% 
  ggplot(aes(x=Month,y=total,fill=Arrest)) + 
  geom_bar(stat="identity",position=position_dodge()) + 
  theme(axis.text.x=element_text(angle=45)) + 
  ggtitle("Chicago Arrests per Month in 2013")



## ------------------------------------------------------------------------
chi %>% group_by(am_pm,Month) %>% 
  summarize(total=n()) %>% 
  ggplot(aes(x=Month,y=total,fill=am_pm)) + 
  geom_bar(stat="identity",position=position_dodge()) + 
  theme(axis.text.x=element_text(angle=45)) + 
  ggtitle("Calls to Police per Month in 2013")



## ------------------------------------------------------------------------
chi %>% mutate(ydays = yday(Date)) %>% 
  group_by(ydays) %>% summarize(total_calls=n()) %>% 
  ggplot(aes(x=ydays,y=total_calls)) + geom_line() +
  ggtitle("Calls per Each Day of 2012") + 
  xlab("Day of the year 1 - 365") +
  ylab("Total Calls to the Police")



## ------------------------------------------------------------------------
chi %>% 
  mutate(ydays = yday(Date)) %>% 
  group_by(am_pm,ydays) %>% 
  summarize(total_calls=n()) %>%
  ggplot(aes(x=ydays,y=total_calls)) + geom_line() + facet_grid(am_pm~.)


## ------------------------------------------------------------------------
chi %>% 
  mutate(ydays = yday(Date)) %>% 
  group_by(am_pm,ydays) %>% 
  summarize(total_calls=n()) %>%
  ggplot(aes(x=ydays,y=total_calls)) + geom_line() + facet_grid(.~am_pm)



## ------------------------------------------------------------------------

chi %>% mutate(hod=hour(Date)) %>% 
  ggplot(aes(x=hod)) + 
  geom_histogram(bins=24,color="black",fill="white") + 
  ggtitle("Call Count by Hour of the Day") +
  xlab("Hour of the day 0 - 23") + theme_bw()



## ----eval=FALSE----------------------------------------------------------
## calend <- chi %>%
##   mutate(ydays = ymd(format(chi$Date,"%Y-%m-%d"))) %>%
##   group_by(ydays) %>% summarize(total_calls=n()) %>%
##   gvisCalendar(.,datevar="ydays",numvar="total_calls",
##                options=list(width="1000px", height="300px"))
## plot(calend)


## ------------------------------------------------------------------------
# Sort the Crime Types From Highest Count to Lowest
chi %>% count(`Primary Type`) %>% arrange(desc(n))

# Equivalent to 

chi %>% 
  group_by(`Primary Type`) %>% 
  summarize(n=n()) %>%
  arrange(desc(n))


## ------------------------------------------------------------------------
chi %>% count(`Primary Type`) %>% 
  arrange(desc(n)) %>% 
  ggplot(aes(x=reorder(`Primary Type`,-n),y=n)) + 
  geom_bar(stat="identity",fill="#f68060", alpha=.6, width=.8) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


## ------------------------------------------------------------------------
chi %>% group_by(`Primary Type`) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n)) %>% 
  slice(1:20) %>% 
  ggplot(aes(x=reorder(`Primary Type`,-n),y=n)) + 
  geom_bar(stat="identity",fill="#f68060", alpha=.6, width=.8) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


## ------------------------------------------------------------------------
chi %>% group_by(Arrest,`Primary Type`) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n)) %>% 
  slice(1:20) %>% 
  ggplot(aes(x=reorder(`Primary Type`,-n),y=n,fill=Arrest)) + 
  geom_bar(stat="identity", width=.8) +
  xlab("Crime Type") +
  ylab("Total Calls") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## ------------------------------------------------------------------------
chi %>% group_by(Arrest,`Primary Type`) %>% 
  summarize(count=n()) %>% 
  arrange(desc(count)) %>% 
  slice(10:20) %>% 
  ggplot(aes(x=reorder(`Primary Type`,-count),
             y=count,fill=Arrest)) +     
  geom_bar(stat="identity",width=.8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## ------------------------------------------------------------------------
chi %>% filter(grepl("PROST",Description)) %>% 
  group_by(Arrest) %>% 
  summarize(cnt=n())

#

chi %>% filter(grepl("BURGLARY",`Primary Type`)) %>% 
  group_by(Arrest) %>% 
  summarize(cnt=n())



## ------------------------------------------------------------------------
chi %>% filter(grepl("NARCOTICS",`Primary Type`)) %>% 
  group_by(Description) %>%
  summarize(count=n()) %>%
  arrange(desc(count)) %>% 
  slice(1:10) %>% 
  ggplot(aes(x=reorder(Description,-count),
             y=count)) +      
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## ------------------------------------------------------------------------

# Most of the > 30GMS cannabis possession happens in the STREET

chi %>% filter(grepl("CANNABIS MORE THAN 30GMS",Description)) %>%
  group_by(`Location Description`) %>% 
  summarize(count=n()) %>%
  arrange(desc(count))




## ----echo=FALSE----------------------------------------------------------
chi_map <- readRDS("chi_map.rds")
chi_map_13 <- readRDS("chi_map_13.rds")


## ----etmap13,eval=FALSE--------------------------------------------------
## library(ggmap)
## 
## chi_map <- get_map("Chicago, Illinois",zoom=11,extent="device")


## ------------------------------------------------------------------------

# The following reveals that there are 284 violations. 
street <- chi %>% 
  filter(grepl("CANNABIS MORE THAN 30GMS",Description)) %>%
  filter(grepl("STREET",`Location Description`)) 
  
nrow(street)

# Let's map these using R googleVis
ggmap(chi_map) + geom_point(aes(x=Longitude,y=Latitude, 
                         color=am_pm),alpha=1,data=street)




## ----cache=TRUE----------------------------------------------------------
narcotics<- chi %>% filter(grepl("NARCOTICS",`Primary Type`))

ggmap(chi_map) + geom_point(aes(x=Longitude,y=Latitude, 
                         color=am_pm),alpha=.9,data=narcotics) 
 


## ----cache=TRUE----------------------------------------------------------
ggmap(chi_map) + 
  stat_bin2d(
    aes(x=Longitude, y=Latitude, color=am_pm, fill=am_pm),
    size= .5,bins=30, alpha=1/2,
    data = narcotics
  )


## ----echo=FALSE----------------------------------------------------------
register_google(key="AIzaSyAl4fVkTWpnEFanFZaKegkkRNC-WOFZjBk")


## ----getmap13,eval=FALSE,cache=TRUE--------------------------------------
## chi_map_13 <- get_map("Chicago,Illinois",zoom=13,extent="device")


## ----statd13,eval=FALSE--------------------------------------------------
## ggmap(chi_map_13) +
##   stat_density2d(
##     aes(x = Longitude, y =Latitude, fill = ..level..,
##         alpha = ..level..),
##     size = 2, bins = 4, data = narcotics, geom = "polygon")
## 


## ----cache=TRUE,eval=FALSE-----------------------------------------------
## ChiMap <- ggmap(chi_map_13,
##                 base_layer = ggplot(aes(x = Longitude,
##                                         y = Latitude),
##                                         data = narcotics))
## 
## # Now we overlay the base with a density based approach
## # The more crime a location experienvces, the darker the color
## 
## ChiMap +
##  stat_density2d(aes(x = Longitude, y =Latitude,
##                     fill = ..level.., alpha = ..level..),
##                 size = 2, bins = 4,
##                 data = narcotics,
##                 geom = "polygon") + facet_wrap(~weekdays)


## ----leaflet-------------------------------------------------------------
library(leaflet)

m <- leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng=narcotics$Longitude,
             lat=narcotics$Latitude,
             popup=narcotics$Description,
             clusterOptions = markerClusterOptions())

m

