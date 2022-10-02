rm(list = ls())
set.seed(32156944)
webforum <- read.csv("webforum.csv") # Read the data
webforum <- webforum [sample(nrow(webforum), 20000), ] # 20000 rows
# Library used,there's another igraph library will be attached in question(c) later,if I attach it here,
# there will be some errors in Question(a)(1).
library(dplyr)
library(ggplot2)
library(tidyverse)
library(correlation)
library(corrplot)
#Question (a)(1)
#Need to detach the igraph package or it may have errors.
#Convert the dates in Date column to date objects with correct format.
webforum$Date <- as.Date(webforum$Date, format = "%Y-%m-%d")
#Sort the data by date in ascending order.
webforum <- webforum[order(as.Date(webforum$Date, format="%Y-%m-%d")),]
#To get the frequency of posts posted in every month of each year,I have to get the frequency of every month of each year.
tab <- table(cut(webforum$Date, 'month'))
tab2 <- table(cut(webforum$Date, 'year'))
#Then,I make it into a data frame recording the date and its corresponding frequency.
date_frequency <- data.frame(Date=format(as.Date(names(tab)), '%m-%Y'),Frequency=as.vector(tab))
time.series2 <- data.frame(Date=format(as.Date(names(tab2)), '%Y'),Frequency=as.vector(tab2))
#Create the time_series object
monthly_webforum_ts <- ts(date_frequency$Frequency, start =c(2002,1), freq = 12)
#Decompose the time series into seasonal, trend and irregular components
monthly_webforum_decomposed <- decompose(monthly_webforum_ts)
#Plot the decomposed time series
plot(monthly_webforum_decomposed)
#Question(a)(2), to observe the levels of these linguistic variables over the duration of the forum
#I split the webforum into webforum 2 and webforum 3,each of those contain Date and different linguistic variables.
webforum2 <- webforum[,c(3,5:14)]
webforum3 <- webforum[,c(3,15:23)]
#Convert the date to year-month format and add the day of 01 to each date so that it can be converted to Date object in R
webforum2$Date <- format(as.Date(webforum2$Date), "%Y-%m")
webforum2$Date <- as.Date(paste(webforum2$Date,"-01",sep=""))
#Convert the date to year-month format and add the day of 01 to each date so that it can be converted to Date object in R
webforum3$Date <- format(as.Date(webforum3$Date), "%Y-%m")
webforum3$Date <- as.Date(paste(webforum3$Date,"-01",sep=""))
#Then,for all the posts posted on each year-month,I group them up by using the group_by(Date) function and used
#summarise_all(.funs = mean) to get the mean value of all their linguistic variables on that particular year-month.
average_webforum2 <- webforum2 %>% group_by(Date) %>% summarise_all(.funs = mean)
average_webforum3 <- webforum3 %>% group_by(Date) %>% summarise_all(.funs = mean)
#Then,I reformat the data from "wide" to "long" format and plot each of the linguistic variable over years with facet_wrap.
formatted <- average_webforum2 %>% pivot_longer(2:11, names_to = "linguistic_variable", values_to = "values")
ggplot(formatted, aes(Date, values)) + geom_line() + facet_wrap(~linguistic_variable, scales = "free_y", ncol = 1)
#Then,I reformat the data from "wide" to "long" format and plot each of the linguistic variable over years with facet_wrap.
formatted2 <- average_webforum3 %>% pivot_longer(2:10, names_to = "linguistic_variable", values_to = "values")
ggplot(formatted2, aes(Date, values)) + geom_line() + facet_wrap(~linguistic_variable, scales = "free_y", ncol = 1)
clout_analytic <- formatted[formatted$linguistic_variable %in% c("Clout","Analytic"),]
t.test(values ~ linguistic_variable,data = clout_analytic)
#Question(a)(2) Part 2
#To filter out all the linguistic variables
linguistic_variables <- webforum[,5:23]
#correlation is the function from correlation package
#Performs a correlation analysis on linguistic variables,it will also output the p-values for each correlation coefficient .
corr <- as.data.frame(correlation::correlation(linguistic_variables,include_factors = TRUE, method = "auto"))
#Filter out the relevant columns
corr <- corr[,c(1,2,3,9)]
#Filter out the correlation coefficient values with p-value < 0.05
corr <- filter(corr,p < 0.05)
#Find the strongly correlated linguistic variables
strong_relationship <- filter(corr, r >= 0.6 | r <= -0.6)
#Find the moderately correlated linguistic variables
moderate_relationship <- filter(corr, (r >= 0.4 & r < 0.6) | (r <= -0.4 & r > -0.6))
#Find the weakly correlated linguistic variables
weak_relationship <- filter(corr, (r >= 0.2 & r < 0.4) | (r <= -0.2 & r > -0.4))
#Find the very weakly correlated linguistic variables
very_weak_relationship <- filter(corr, (r >= 0.0 & r < 0.2) | (r <= -0.0 & r > -0.2))
#corrplot is the function from corrplot package which is used to plot correlogram.
corr_plot <- corrplot(cor(linguistic_variables),method = "number",type = "upper",sig.level = 0.05,insig = "blank")
#Question(b)
#Filter out the posts with WC >= 100
webforum_filtered_wc <- filter(webforum, WC >= 100)
#Filter out the columns with only relevant linguistic variables
emo <- webforum_filtered_wc[,c(1,16,17,19,20)]
#Aggregate by ThreadID and get the mean of each linguistic variable.
emo <- aggregate(emo,list(emo$ThreadID),mean)
emo <- emo[,2:6]
#Do the summary
summary(emo[,2:5])
#Filter out the threads with their negomo == 0 & anger == 0 & sad == 0
optimistic <- filter(emo,negemo == 0 & anger == 0 & sad == 0)
#Get the top 10 threads with greatest posemo
optimistic <- optimistic[order(-optimistic$posemo),]
optimistic <- optimistic[1:10,]
#Plot the boxplot of top 20 most posts posted threads with their posemo values from year 2002-2004
year02_04 <- filter(webforum, Date < as.Date("2004-01-01"))
summary(year02_04[,c("posemo","negemo","anger","sad")])
table(year02_04$ThreadID)
tab_threadID <- table(year02_04$ThreadID)
threadID_freq <- data.frame(ThreadID = names(tab_threadID),Frequency=as.vector(tab_threadID))
attach(threadID_freq)
threadID_freq <- threadID_freq[order(-Frequency),]
#Filter out top 20 most posts posted threads
filtered_0204 <- filter(year02_04,year02_04$ThreadID %in% threadID_freq[1:20,1])
boxplot_0204 <- filtered_0204[,c(1,16)]
boxplot(boxplot_0204$posemo ~ boxplot_0204$ThreadID, las = 2, xlab = "ThreadID", ylab = "posemo",
        main = "Boxplot top 20 most posts threads from 2002 - 2004" )
#Plot the boxplot of top 20 most posts posted threads with their posemo values from year 2004-2006
year04_06 <- filter(webforum, Date > as.Date("2003-12-31") & Date < as.Date("2006-01-01"))
summary(year04_06[,c("posemo","negemo","anger","sad")])
table(year04_06$ThreadID)
tab_threadID <- table(year04_06$ThreadID)
threadID_freq <- data.frame(ThreadID = names(tab_threadID),Frequency=as.vector(tab_threadID))
attach(threadID_freq)
threadID_freq <- threadID_freq[order(-Frequency),]
filtered_year04_06 <- filter(year04_06,year04_06$ThreadID %in% threadID_freq[1:20,1])
boxplot_0406 <- filtered_year04_06[,c(1,16)]
boxplot(boxplot_0406$posemo ~ boxplot_0406$ThreadID, las = 2, xlab = "ThreadID", ylab = "posemo", main = "Boxplot top 20 most posts threads from 2004 - 2006")
#Plot the boxplot of top 20 most posts posted threads with their posemo values from year 2006-2008
year06_08 <- filter(webforum, Date > as.Date("2005-12-31") & Date < as.Date("2008-01-01"))
summary(year06_08[,c("posemo","negemo","anger","sad")])
table(year06_08$ThreadID)
tab_threadID <- table(year06_08$ThreadID)
threadID_freq <- data.frame(ThreadID = names(tab_threadID),Frequency=as.vector(tab_threadID))
attach(threadID_freq)
threadID_freq <- threadID_freq[order(-Frequency),]
filtered_year06_08 <- filter(year06_08,year06_08$ThreadID %in% threadID_freq[1:20,1])
boxplot_0608 <- filtered_year06_08[,c(1,16)]
boxplot(boxplot_0608$posemo ~ boxplot_0608$ThreadID, las = 2, xlab = "ThreadID", ylab = "posemo", main = "Boxplot top 20 most posts threads from 2006 - 2008")
#Plot the boxplot of top 20 most posts posted threads with their posemo values from year 2008-2010
year08_10 <- filter(webforum, Date > as.Date("2007-12-31") & Date < as.Date("2010-01-01"))
summary(year08_10[,c("posemo","negemo","anger","sad")])
table(year08_10$ThreadID)
tab_threadID <- table(year08_10$ThreadID)
threadID_freq <- data.frame(ThreadID = names(tab_threadID),Frequency=as.vector(tab_threadID))
attach(threadID_freq)
threadID_freq <- threadID_freq[order(-Frequency),]
filtered_year08_10 <- filter(year08_10,year08_10$ThreadID %in% threadID_freq[1:20,1])
boxplot_0810<- filtered_year08_10[,c(1,16)]
boxplot(boxplot_0810$posemo ~ boxplot_0810$ThreadID, las = 2, xlab = "ThreadID", ylab = "posemo", main = "Boxplot top 20 most posts threads from 2008 - 2010")
#Plot the boxplot of top 20 most posts posted threads with their posemo values from year 2010-2012
year10_12 <- filter(webforum, Date > as.Date("2009-12-31"))
summary(year10_12[,c("posemo","negemo","anger","sad")])
table(year10_12$ThreadID)
tab_threadID <- table(year10_12$ThreadID)
threadID_freq <- data.frame(ThreadID = names(tab_threadID),Frequency=as.vector(tab_threadID))
attach(threadID_freq)
threadID_freq <- threadID_freq[order(-Frequency),]
filtered_year10_12 <- filter(year10_12,year10_12$ThreadID %in% threadID_freq[1:20,1])
boxplot_1012 <- filtered_year10_12[,c(1,16)]
boxplot(boxplot_1012$posemo ~ boxplot_1012$ThreadID, las = 2, xlab = "ThreadID", ylab = "posemo", main =
          "Boxplot top 20 most posts threads from 2010 - 2012")
#Question(c)(1)
#Library of igraph imported here or the graph in (a)(1) cannot be plotted
library(igraph)
webforum_month_year <- webforum
#Get all the data from march 2002 only.
webforum_month_year$Date <- format(as.Date(webforum_month_year$Date), "%Y-%m")
webforum_month_year$Date <- as.Date(paste(webforum_month_year$Date,"-01",sep=""))
webforum_march_2002 <- filter(webforum_month_year,Date == as.Date("2002-03-01"))
#Get all the unique AuthorID in march 2002 so that can be used as nodes.
Author <- unique(webforum_march_2002$AuthorID)
Author <- as.data.frame(Author)
#Make an empty graph
social_network <- make_empty_graph(directed = FALSE)
#Add all the vertices with AuthorID as the name into the graph.
for(i in 1:nrow(Author)){
  social_network <- add_vertices(social_network,1,name = as.character(Author$Author[i]))
}
#Get only ThreadID and AuthorID
webforum_threads_author <- webforum_march_2002[,c(1,2)]
# loop through each thread
for(thread in unique(webforum_threads_author$ThreadID)){
  temp = webforum_threads_author[(webforum_threads_author$ThreadID == thread),]
  if(nrow(temp)>1){
    # Combine each pair of authors to make an edge list
    Edgelist = as.data.frame(t(combn(temp$AuthorID,2)))
    # loop through pairs of edges and add
    for(i in 1:nrow(Edgelist)){
      colnames(Edgelist) = c("Author1","Author2")
      social_network <- add_edges(social_network,c(as.character(Edgelist$Author1[i]),as.character(Edgelist$Author2[i])))
    }
  }
}
#Simplify to remove duplicated edge
social_network = simplify(social_network)
layout <- layout.fruchterman.reingold(social_network)
#Plot the graph
plot(social_network,layout = layout, vertex.size = 9)
layout <- layout.circle(social_network)
plot(social_network,layout = layout, vertex.size = 9)
#Question (c)(2)
#get the degree centrality
degree = as.table(degree(social_network))
#get the betweenness centrality
betweenness = as.table(betweenness(social_network))
#get the closeness centrality
closeness = as.table(closeness(social_network))
#get the eigenvector centrality
eig = as.table(evcent(social_network)$vector)
#Combine the values and make them into a dataframe
vertex_analysis <- as.data.frame(rbind(degree, betweenness, closeness, eig))
vertex_analysis <- t(vertex_analysis)
#Sort the vertex by betweenness centrality in descending order
vertex_analysis <- vertex_analysis[order(-betweenness),]
top_author_others <- webforum_march_2002
top_author_others$AuthorID[top_author_others$AuthorID != 5697] <- "OTHERS"
top_author_others_avrg <- as.data.frame(aggregate(top_author_others,list(top_author_others$AuthorID),mean))
top_author_others_avrg <- top_author_others_avrg[,c(1,6,7,8,9,10,17,18,19,20,21,22,23,24)]
#Hypothesis testing
#Test if True mean of WC(5697) – True mean of WC(OTHERS) > 0
t.test(WC ~ AuthorID,data = top_author_others,alternative = "greater")
#Test if True mean of Analytic(5697) – True mean of Analytic(OTHERS) > 0
t.test(Analytic ~ AuthorID,data = top_author_others,alternative = "greater")
#Test if True mean of anger(5697) – True mean of anger(OTHERS) < 0
t.test(anger ~ AuthorID,data = top_author_others,alternative = "less")
#Test if True mean of focuspresent(5697) – True mean of focuspresent (OTHERS) < 0
t.test(focuspresent ~ AuthorID,data = top_author_others,alternative = "less")