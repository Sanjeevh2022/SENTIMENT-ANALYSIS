library(dplyr)
library(ggplot2)
library(forcats)
library(tidyverse)
library(tm)
library(SnowballC)
library(tidytext)
library(lubridate)
library(scales)

#reading the file
tekhubdata <- readxl::read_excel("TekHub Visit Form (Responses) (5).xlsx")

#structure of the dataset
head(tekhubdata)
str(tekhubdata)
summary(tekhubdata)

#changing column names
colnames(tekhubdata)<-c("timestamp","whichtech","goal","challenge","resolved","note","satisfaction")
summary(tekhubdata)

#checking for missing values
sapply(tekhubdata, function(x) sum(is.na(x)))

#deleting missing values
tekhubdata<- na.omit(tekhubdata)

#checking again to make sure missing values are deleted
sapply(tekhubdata, function(x) sum(is.na(x)))

#timestamp
#plotting visits/calls over the months
tekhubdata$month <- as.Date(tekhubdata$timestamp, format="%m")
visits_by_month <- aggregate(tekhubdata$timestamp, by=list(tekhubdata$month), FUN=length)
colnames(visits_by_month) <- c("Month","Visits")
print(visits_by_month)

tekhubdata$date <- as.Date(tekhubdata$timestamp)

ggplot(tekhubdata, aes(x=date)) +
  geom_freqpoly(binwidth = 3, color = "lightblue", size = 1) +
  scale_x_date(date_breaks = "month", date_labels = "%b-%y") +
  xlab("Month-Year") +
  ylab("Number of Visits") +
  ggtitle("Number of Visits/calls By Month") +
  theme(plot.title = element_text(hjust = 0.5))

#plotting Frequency by Days of the Week
# Extract the 'timestamp' column from the dataset
timestamp <- tekhubdata$timestamp

# Convert the 'timestamp' column to date format
timestamp <- as.Date(timestamp)

# Extract the 'Day' and 'Month' of the 'timestamp'
Day <- wday(timestamp)
Month <- month(timestamp)

# Create a dataframe of 'Day' and 'Month'
data <- data.frame(Day, Month)


day <- as.Date(timestamp, "%Y-%m-%d")
tekhubdata$day <- as.factor(weekdays(day)) 
ordered_day <- factor(tekhubdata$day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
ggplot(data = tekhubdata)+
  geom_bar(aes(x = ordered_day, fill = ordered_day))+
  labs(title = "Frequency by Days of the Week", x = "Day of the Week", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

#plotting the Frequency By Month

# Extract the month from the timestamp variable
tekhubdata$month <- format(as.POSIXct(tekhubdata$timestamp,format='%Y-%m-%d %H:%M:%S'),'%m')

# Calculate the frequency of months
month_freq <- tekhubdata %>%
  group_by(month) %>%
  summarise(count=n())

# Plot the result using ggplot2
ggplot(month_freq, aes(x=month, y=count)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x="Month", y="Frequency") +
  ggtitle("Frequency of Visits/Calls by Month") +
  theme(plot.title = element_text(hjust = 0.5))

#whichtech 
str(tekhubdata$whichtech)
unique(tekhubdata$whichtech)

#binning categories
tekhubdata$whichtech <- gsub("Smartphone: iOS|Smartphone: Android|Phones|Captioned Phone|Flip Phone|Flip phone|Smart Device|OOMA|Flip-phone|Voice Mail|Landline", "Phone", tekhubdata$whichtech)
tekhubdata$whichtech <- gsub("Laptop|Desktop|Desktop Windows|Desktop Mac|DVD Player Laptop|Computer/Speakers", "Computer", tekhubdata$whichtech)
tekhubdata$whichtech <- gsub("Linux Telikin|Tablet:iOS|Tablet: Android|iPad and iPhone|Nook|Telikin|Telekin|Stylus", "Tablet", tekhubdata$whichtech)
tekhubdata$whichtech <- gsub("Modem + Router|Wireless Printer|Printer|Router|Hardware|Wireless Keyboard|Keyboard|Monitor|Scanner|Power|Electronics|Modem|Camera|HardwarePoint|HardwarePoint|Wireless Mouse|Answering Machine", "Hardware", tekhubdata$whichtech)
tekhubdata$whichtech <- gsub("Cloud|Google Sheets|PowerPoint|Word|Presentation|Installation|YouTube|Accountant Calculator|Photo|Document|Chromebook|Digital Software Frame|Photo Printer|Drive|Digital Signature|Powerpoint|Email|Communication", "Software", tekhubdata$whichtech)
tekhubdata$whichtech <- gsub("Flashdrive|Digital Picture Frame|Accessories|Medical Devices|Digital Photo Frame|Hearing Aids|Scale|Digital Frame", "Misc_Hardware", tekhubdata$whichtech)
tekhubdata$whichtech <- gsub("Online Shopping|Amazon|Internet|Amazon Self Publishing|Research|YouTube|Location|Portal|Facebook Portal|Zoom", "Dotcom", tekhubdata$whichtech)
tekhubdata$whichtech <- gsub("Account|Account_Mgmt_Mgmt_Mgmt_Mgmt|Account_Mgmt_Mgmt_Mgmt|Password Manager|Password Reset|Security|Scam|Accounts|Password|PayPal|Antivirus|Hack", "Account_Mgmt", tekhubdata$whichtech)
tekhubdata$whichtech <- gsub("Bluetooth|WiFi|Game|Fitbit|Apple Watch|Voice command device (e.g. Echo)|Literally everything|Infrastructure|Recycle|icense Renewal|Various|Data Transfer|Latitude", "Other", tekhubdata$whichtech)
tekhubdata$whichtech <- gsub("Smart TV|Computer TV Phone Hardware|Roku|Remote|Video|Apple TV|Recording|Play|CD|DVD|TV|Home Theater|Sound System|Television|DVD Player", "TV_Video_audio", tekhubdata$whichtech)


#explore whichtech
tekhubdata$whichtech1 <-as.factor(tekhubdata$whichtech1)
tekhubdata %>% mutate(whichtech1 =  fct_lump(whichtech,prop=0.025))%>% count(whichtech1,sort=TRUE)
ggplot(data = subset(tekhubdata, !is.na(whichtech)),aes(y=fct_lump(whichtech, n = 5))) + 
  geom_bar(fill = c("yellow", "blue", "lightgreen", "orange","purple","pink")) + coord_flip() +
  xlab("Frequency") +
  ylab("Technology/Device") +
  ggtitle("Technology") +
  theme(plot.title = element_text(hjust = 0.5))


#GOALS 
# 'goals' into factor
#lumping 10% or less categories into Other
tekhubdata$goal <-as.factor(tekhubdata$goal)
levels(factor(tekhubdata$goal))
tekhubdata %>% mutate(newgoal =  fct_lump(goal,prop=0.1)) %>% count(newgoal,sort=TRUE)
ggplot(data = subset(tekhubdata, !is.na(goal)), aes(x=fct_lump(goal, n = 3))) +
  geom_bar(fill = c("lightyellow", "lightblue", "lightgreen", "orange")) +
  xlab("Goals") +
  ylab("Frequency") +
  ggtitle("Goals") +
  theme(plot.title = element_text(hjust = 0.5))

#CHALLENGES
#replacing the longer strings with shorter strings
tekhubdata$challenge <-  gsub("Awareness- knowledge of available solutions to address the task","Awareness",tekhubdata$challenge,perl = FALSE)
tekhubdata$challenge <-  gsub("Confidence- belief in ability to handle the task","Confidence",tekhubdata$challenge,perl = FALSE)
tekhubdata$challenge <-  gsub("Cognitive- ability to understand the requirements for use","Cognitive",tekhubdata$challenge,perl = FALSE)
tekhubdata$challenge <-  gsub("Memory ability to remember key requirements (e.g. password)","Memory",tekhubdata$challenge,perl = FALSE)
tekhubdata$challenge <-  gsub("PhysicaL ability to manipulate the device (e.g. advanced swiping required)","Physical",tekhubdata$challenge,perl = FALSE)
tekhubdata$challenge <-  gsub("Sensory ability to detect the state of the device (e.g. text too small)","Sensory",tekhubdata$challenge,perl = FALSE)

#removing punctuations
tekhubdata$challenge <-  gsub("-","",tekhubdata$challenge,perl = FALSE)
tekhubdata$challenge <- gsub("-.*","", tekhubdata$challenge)
tekhubdata$challenge<- gsub("ability.*","", tekhubdata$challenge)
tekhubdata$challenge<- gsub("(.*)e\\.g\\..*","", tekhubdata$challenge)



# Split values separated by ',' in the 'challenge' column
challenge_split <- separate(tekhubdata,challenge,into = c("Awareness", "Confidence", "Cognitive","Memory","Physical","Sensory"), sep = ",")

# Bind (combine) the two dataframes together
tekhubdata <- bind_cols(tekhubdata, challenge_split[,4:9])



#encoding
tekhubdata$Awareness <- ifelse(grepl('Awareness', tekhubdata$challenge, ignore.case = TRUE), 1, 0)
tekhubdata$Confidence <- ifelse(grepl('Confidence', tekhubdata$challenge, ignore.case = TRUE), 1, 0)
tekhubdata$Cognitive <- ifelse(grepl('Cognitive', tekhubdata$challenge, ignore.case = TRUE), 1, 0)
tekhubdata$Physical <- ifelse(grepl('Physical', tekhubdata$challenge, ignore.case = TRUE), 1, 0)
tekhubdata$Sensory <- ifelse(grepl('Sensory', tekhubdata$challenge, ignore.case = TRUE), 1, 0)
tekhubdata$Memory <- ifelse(grepl('Memory', tekhubdata$challenge, ignore.case = TRUE), 1, 0)

# Calculating the column totals
tekhubdata$challenge_score <- rowSums(tekhubdata[,c("Awareness","Confidence","Cognitive","Memory","Physical","Sensory")])

tekhubdata$challenge_score <- as.factor(tekhubdata$challenge_score, levels = c("0","1","2","3","4"))

#sum of columns
sums <- apply(tekhubdata[ , 8:13], 2, sum)
sums <-as.data.frame(sums)

# Visualize the sums
colnames(sums) <- c("sums1")
ggplot(sums,aes(rownames(sums),sums$sums1)) + 
  geom_bar(stat="identity", fill = c("red", "lightblue", "green", "orange","purple","pink")) +
  xlab("Challenge") +
  ylab("Frequency") +
  ggtitle("Challenges") +
  theme(plot.title = element_text(hjust = 0.5))


#challenge_score
ggplot(data = tekhubdata,aes(x = challenge_score)) + 
  geom_bar(fill = c("lightblue","darkred","orange","purple","pink")) +
  xlab("Challenge_Score") +
  ylab("Frequency") +
  ggtitle("Challenge Score") +
  theme(plot.title = element_text(hjust = 0.5))


#note

#unnesting the words
tekhubdata %>% select(note) %>% unnest_tokens(word,note) %>% count(word,sort=TRUE)

#creating stop_words
data(stop_words)

tekhubdata %>% select(note) %>% unnest_tokens(word,note) %>% 
  filter(!word %in% stop_words$word)  %>%
  count(word,sort=TRUE) %>% View()

tekhubdata %>% select(note) %>% unnest_tokens(ngrams,note,token="ngrams",n=2) %>% count(ngrams,sort=TRUE)


bigrams_separated <-tekhubdata %>% select(note) %>% 
  unnest_tokens(ngrams,note,token="ngrams",n=2) %>%
  separate(ngrams, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigrams <- paste(bigram_counts$word1,bigram_counts$word2,sep = " ")
bigrams <- as.data.frame(bigrams)

bigrams$bigrams <- gsub("resident|Resident|resident's|Resident's|residents|Residents", "", bigrams$bigrams)
bigrams$bigrams <- gsub("zoom meeting|zoom call|zoom calls|zoom account|zoom app|zoom installed|zoom invite", "zoom", bigrams$bigrams)
bigrams$bigrams <- gsub("email address|email account", "email", bigrams$bigrams)
bigrams$bigrams <- gsub("multiple times", "", bigrams$bigrams)





bigrams$bigrams$combined_words <- paste(bigram_counts$word1, bigram_counts$word2, sep = " ")
bigram_counts <- na.omit(bigram_counts)

bigram_counts$combined_words <- gsub("[0-9]", "", bigram_counts$combined_words)
ggplot(data = bigram_counts[bigram_counts$n > 9,],aes(x = n, y = combined_words,sort = TRUE)) +
  geom_bar(stat = "identity") +
  theme(plot.title = element_text(hjust = 0.5))

#WORDCLOUDS
# Natural Language Processing

docs <- Corpus(VectorSource(tekhubdata$note))

#text transformation
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
#text cleaning
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)
#Build a Term Document Matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#CLOUD#1
docs <- Corpus(VectorSource(bigram_counts$combined_words))





#text cleaning
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("resident", "Resident")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)




#Build a Term Document Matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


#Generate a WordCloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq,scale=c(2.5,0.25), min.freq = 4,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
#CLOUD#2
docs <- Corpus(VectorSource(bigram_counts$combined_words))





#text cleaning
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("resident", "Resident")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)




#Build a Term Document Matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


#Generate a WordCloud
set.seed(123)
wordcloud(words = d$word, freq = d$freq,scale=c(2.5,0.25), min.freq = 4,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


str(tekhubdata)




