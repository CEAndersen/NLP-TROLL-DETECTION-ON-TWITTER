setwd("~/Desktop/NLP/LDA")

dTroll=read.csv("fulldata.csv", sep = ",")
dNonTroll=read.csv("NotTrolls.csv", sep = ",")

#Add labels to the troll and non troll data 
dTroll["label"] <- 1
dNonTroll["label"] <- 0

#Rename colum to text
names(dNonTroll)[names(dNonTroll) == "X0"] <- "text"

#Choose only text and label columns 
dNonTroll1 <- subset(dNonTroll, select = c("text", "label"))
dTroll1 <- subset(dTroll, select = c("text", "label"))

#Bind the two datasets togeather
data = rbind(dTroll1,dNonTroll1)

#Save data 
write.csv(data,"/Users/Pia/Desktop/NLP/LDA//dataTest", row.names = FALSE)

#Save as CSV file
AllData=read.csv("AllDataCleaned.csv", sep = ",")

#Split data into the two groups
AllDataTroll=subset(AllData, label == 1)
AllDataNotTroll=subset(AllData, label == 0)

#t.test on number of words per tweet 
Model= t.test(AllDataTroll$tweet_length, AllDataNotTroll$tweet_length, paried=TRUE)
Model

t = Model$statistic[[1]]
t
df = Model$parameter[[1]]
df
r = sqrt(t^2/(t^2+df))
r


#t.test on number of charecters per tweet
Model1= t.test(AllDataTroll$tweet_word_count, AllDataNotTroll$tweet_word_count, paried=TRUE)
Model1


t1 = Model1$statistic[[1]]
t1
df1 = Model1$parameter[[1]]
df1
r1 = sqrt(t1^2/(t1^2+df1))
r1

#Some colors to use
palette(c("red", "#4682B4", "#00008B", "darkgreen"))

#Plots: 
plot1=boxplot(tweet_word_count ~ label,
        data = AllData,
        names=c("NotTrolls","Trolls"),
        ylab="Average number of words per tweet",
        col=c(2,4))


plot2=boxplot(tweet_length ~ label,
        data = AllData,
        names=c("NotTrolls","Trolls"),
        ylab="Average number of characters per tweet",
        col=c(2,4))




