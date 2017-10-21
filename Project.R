#loading libraries
library(plyr)
library(dplyr)
library(ggplot2)
#---------------------------------------------------------------------------------------------------------------------
#Reading data set
data <- read.csv("data/DOHMH_NYC.csv", na.strings = c("NA","Not Applicable"))
#---------------------------------------------------------------------------------------------------------------------
#Filtering out restaurants with No violations, as they do not have complete information
dataNOV <- data %>% filter(ACTION =="No violations")

#Selecting the other restaurants
dataNew <- data %>% filter(!ACTION =="No violations")

#Omitting NA values which are no considerable
dataNA <- na.omit(dataNew)
#---------------------------------------------------------------------------------------------------------------------
#Formating date types
dataNewD <- dataNA
dataNewD$GRADE.DATE <- as.Date(dataNewD$GRADE.DATE, format="%y/%m/%d")
dataNewD$INSPECTION.DATE <- as.Date(dataNewD$INSPECTION.DATE, format="%y/%m/%d")
dataNewD$RECORD.DATE <- as.Date(dataNewD$RECORD.DATE, format="%y/%m/%d")
#---------------------------------------------------------------------------------------------------------------------
# Data set with unque observations for each restaurant
dataUniqueR<- dataNewD %>% group_by(BORO, DBA, BUILDING, STREET) %>% arrange(desc(GRADE.DATE)) %>% slice(1:1)
#---------------------------------------------------------------------------------------------------------------------
#Number of restaurants in each BORO
A <- dataNewD %>% select(BORO, DBA, BUILDING, STREET)
dataResCountB <- unique(A[,]) %>% group_by(BORO) %>% tally()
myPlot1 <- ggplot(data = dataResCountB, aes(x=BORO,y=n))+ ggtitle("Number of Restaurants in Each Boro")+labs(x="Boro",y="Number of Restaurants")
myPlot1 + geom_bar(stat="identity", aes(fill = BORO))+ theme(plot.title = element_text(hjust = 0.5))
#---------------------------------------------------------------------------------------------------------------------
# Distribution of score variable
plot10 <- dataUniqueR %>% select(SCORE) %>% filter(SCORE != "NA") %>% group_by(SCORE) %>% tally()
dataBoroCount10 <- plot10
myPlot3 <- ggplot(data = dataBoroCount10, aes(x=SCORE,y=n))+ggtitle("Distribution of Scores")+ labs(x="Score",y="Frequency of restaurants")
myPlot3 + geom_bar(stat="identity", fill = "blue")+ xlim(-2,160) + theme(plot.title = element_text(hjust = 0.5))
#---------------------------------------------------------------------------------------------------------------------
# Number of Restaurants vs Cuisine Type 
dataResCountC <- dataUniqueR %>% group_by(CUISINE.DESCRIPTION) %>% tally() %>% arrange(desc(n))
dataResCountCTop10 <- dataResCountC %>% head(10)
myPlot1 <- ggplot(data = dataResCountCTop10, aes(x=CUISINE.DESCRIPTION,y=n)) + ggtitle("Number of Restaurants by Cuisine type")+ labs(x="Cuisine Type",y="Number of Restaurants")
myPlot1 + geom_bar(stat="identity", aes(fill = CUISINE.DESCRIPTION)) + theme(axis.text.x = element_text(angle = 90)) + theme(plot.title = element_text(hjust = 0.5))
#---------------------------------------------------------------------------------------------------------------------
# Number of critical violations vs Cuisine Type 
dataResCountCRI <- dataUniqueR %>% filter(CRITICAL.FLAG=="Critical") %>% group_by(CUISINE.DESCRIPTION)  %>% tally() %>% arrange(desc(n))
dataResCountCRITop10 <- dataResCountCRI %>% head(10)
myPlot1 <- ggplot(data = dataResCountCRITop10, aes(x=CUISINE.DESCRIPTION,y=n)) + ggtitle("Number of Restaurants with critical violations")+ labs(x="Cuisine Type",y="Number of Restaurants")
myPlot1 + geom_bar(stat="identity", aes(fill = CUISINE.DESCRIPTION)) + theme(axis.text.x = element_text(angle = 90)) + theme(plot.title = element_text(hjust = 0.5))

#---------------------------------------------------------------------------------------------------------------------
# Distribution of Restaurants graded A for each boro
dataBoroCount2<- dataUniqueR %>% select(BORO, DBA, GRADE) %>% filter(GRADE %in% c("A","B","C")) %>% group_by(BORO,GRADE) %>% tally() %>% mutate(percentage = (n*100)/sum(n))
dataBoroCount2_A <- dataBoroCount2 %>% filter(GRADE=="A")
myPlot2 <- ggplot(data = dataBoroCount2, aes(x=BORO,y=n,fill=GRADE)) +ggtitle("Distribution of Grades of Restaurants area wise")+ labs(x="Borough",y="Number of Restaurants")
myPlot2 + geom_bar(stat="identity") + theme(plot.title = element_text(hjust = 0.5))
#---------------------------------------------------------------------------------------------------------------------
# Area vs No of restaurants with critical violations
dataBoroCount7 <- dataUniqueR %>% select(BORO, DBA, CRITICAL.FLAG) %>% filter(CRITICAL.FLAG == "Critical") %>% group_by(BORO) %>% tally() %>% mutate(percentage = (n*100)/sum(n))
myPlot3 <- ggplot(data = dataBoroCount7, aes(x=BORO,y=percentage))+ggtitle("% of Restaurants with Critical Violations")+ labs(x="Boro",y="% of restaurants")
myPlot3 + geom_bar(stat="identity", aes(fill = BORO)) + theme(plot.title = element_text(hjust = 0.5))
#---------------------------------------------------------------------------------------------------------------------
# Area vs No of restaurants with critical violations
dataBoroCount7 <- dataUniqueR %>% select(BORO, DBA, CRITICAL.FLAG) %>% filter(CRITICAL.FLAG == "Critical") %>% group_by(BORO) %>% tally() %>% mutate(percentage = (n*100)/sum(n))
myPlot3 <- ggplot(data = dataBoroCount7, aes(x=BORO,y=percentage))+ggtitle("% of Restaurants with Critical Violations")+ labs(x="Boro",y="% of restaurants")
myPlot3 + geom_bar(stat="identity", aes(fill = BORO)) + theme(plot.title = element_text(hjust = 0.5))

#---------------------------------------------------------------------------------------------------------------------
# Area vs percent of A, B..
dataBoroCount8<- dataUniqueR %>% select(BORO, GRADE) %>% filter(GRADE != "Not Yet Graded") %>% group_by(BORO, GRADE) %>% tally()
myPlot6 <- ggplot(data = dataBoroCount8, aes(x=BORO,y=n, fill=GRADE))+ggtitle("Distribution of grades in each borough")+ labs(x="Borough",y="Number of grades")
myPlot6 + geom_bar(stat="identity") + theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 30))
#---------------------------------------------------------------------------------------------------------------------
#average scores of each borough for the different cuisine types available
x <- dataNewD %>% group_by(CUISINE.DESCRIPTION,BORO) %>% summarise(count=n(),mean_score=mean(SCORE))
x <- x %>% filter(CUISINE.DESCRIPTION=="Caribbean" | CUISINE.DESCRIPTION=="Chinese" | CUISINE.DESCRIPTION=="Indian"| CUISINE.DESCRIPTION=="Latin")
ggplot(x,aes(x=BORO,y=mean_score))+facet_wrap(~CUISINE.DESCRIPTION,ncol = 2)+theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90))+geom_bar(stat="identity", aes(fill = BORO)) +ggtitle("Average score vs BORO")+ labs(x="Borough",y="Average score")
#---------------------------------------------------------------------------------------------------------------------
#distribution of grades for Domino’s over the period 2001 to 2012
y <- dataNewD %>% select(DBA,GRADE.DATE,GRADE)
y <- na.omit(y) %>% filter(DBA=="DOMINO'S" & GRADE %in% c("A","B","C")) 
y <- y %>% mutate(year = format(y$GRADE.DATE, "%Y"))
z <- y %>% group_by(year,GRADE) %>% tally() %>% mutate(percentage=n*100/sum(n))
ggplot(z,aes(x=GRADE,y=percentage))+facet_wrap(~year,ncol = 4)+theme(plot.title = element_text(hjust = 0.5))+geom_bar(stat="identity", aes(fill = GRADE)) +ggtitle("DOMINO'S: Percentage of each grade year wise")+ labs(x="Grade",y="Percentage of each grade received")
#---------------------------------------------------------------------------------------------------------------------
#distribution of grades for Papa John’s over the period 2001 to 2012
y <- dataNewD %>% select(DBA,GRADE.DATE,GRADE)
y <- na.omit(y) %>% filter(DBA=="PAPA JOHN'S" & GRADE %in% c("A","B","C")) 
y <- y %>% mutate(year = format(y$GRADE.DATE, "%Y"))
z <- y %>% group_by(year,GRADE) %>% tally() %>% mutate(percentage=n*100/sum(n))
ggplot(z,aes(x=GRADE,y=percentage))+facet_wrap(~year,ncol = 4)+theme(plot.title = element_text(hjust = 0.5))+geom_bar(stat="identity", aes(fill = GRADE)) +ggtitle("PAPA JOHN'S: Percentage of each grade year wise")+ labs(x="Grade",y="Percentage of each grade received")

#---------------------------------------------------------------------------------------------------------------------
#Dstribution of mean scores on a time scale for pizza restaurants
ys <- dataNewD %>% select(DBA,GRADE.DATE,SCORE)
ys <- na.omit(ys) %>% filter(DBA=="DOMINO'S"|DBA=="PAPA JOHN'S"|DBA=="PIZZA HUT" | DBA=="LITTLE CAESARS") 
ys <- ys %>% mutate(year = format(ys$GRADE.DATE, "%Y"))
zs <- ys %>% group_by(DBA,year) %>% summarise(mean_score=mean(SCORE))
ggplot(zs,aes(x=year,y=mean_score,group=DBA))+theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 90))+geom_line(aes(color=DBA)) +ggtitle("Time series analysis of mean score")+ labs(x="Year",y="Mean score")

