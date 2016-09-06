library(twitteR)
library(ROAuth)
library(httr)
library(dplyr)
library(cognizer)
library(tm)
library(stringr)

setwd("~/Analyses/NFL/KapTweets")

consumer_key <- 'myKey'
consumer_secret <- "mySecret"
access_token <- "myAccess"
access_secret <- "mySecret2"

# Setup Twitter Authorization
setup_twitter_oauth(consumer_key=consumer_key, 
                    consumer_secret=consumer_secret, 
                    access_token=access_token, 
                    access_secret=access_secret)


# Grab latest tweets
kaptweets6 <- searchTwitter('Kaepernick', n=50000, since='2016-08-25', retryOnRateLimit = 150)

# (Had to grab in a few batches)
kap_tweets_df6 <- twListToDF(kaptweets6)
saveRDS(kap_tweets_df6, "kaptweets6.RDS")

kap_tweets_df1 <- readRDS("kaptweets1.RDS")
kap_tweets_df2 <- readRDS("kaptweets2.RDS")
kap_tweets_df3 <- readRDS("kaptweets3.RDS")
kap_tweets_df4 <- readRDS("kaptweets4.RDS")
kap_tweets_df5 <- readRDS("kaptweets5.RDS")
kap_tweets_df6 <- readRDS("kaptweets6.RDS")


full.df <- bind_rows(kap_tweets_df1,kap_tweets_df2)
full.df <- bind_rows(full.df,kap_tweets_df5)
saveRDS(full.df, "kaptweetsFULL.RDS")

full.df <- readRDS("kaptweetsFULL.RDS")

# Remove Retweers
dat <- filter(full.df, isRetweet==FALSE)


# Clean Up Tweet Text with TM
tweets <- str_replace_all(dat$text,"@[a-z,A-Z]*","")  
kapTweets <- Corpus(VectorSource(dat$text))
removeUsers <- function(x) gsub("@[a-z,A-Z]*","",x)
kapTweets <- tm_map(kapTweets, content_transformer(removeUsers))
removeHashtags <- function(x) gsub("#[a-z,A-Z]*","",x)
kapTweets <- tm_map(kapTweets, content_transformer(removeHashtags))
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
kapTweets <- tm_map(kapTweets, content_transformer(removeURL))
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
kapTweets <- tm_map(kapTweets, content_transformer(removeNumPunct))
kapTweets <- tm_map(kapTweets, content_transformer(tolower))

# Reattach Tweets to Data Frame
cleanTweets <-data.frame(clean_text=unlist(sapply(kapTweets, `[`, "content")), 
                      stringsAsFactors=F)

dat <- bind_cols(dat, cleanTweets)

# Save DF with Cleaned Tweets
saveRDS(dat, "kapTweetsClean.RDS")

# Limit to 300 Tweets so as to not go over API limits
dat <- sample_n(dat,300)


#Logging into Cognizer
SERVICE_API_KEY = "apiKey"

# Get Emotion
emotion <- text_emotion(dat3$clean_text, SERVICE_API_KEY)
saveRDS(emotion,"emotion.RDS")

# Get Sentiment
sentiment <- text_sentiment(dat3$clean_text, SERVICE_API_KEY)
saveRDS(sentiment, "sentiment3.RDS")


#Extract Emotion
## -- Quick Note: Emotions aren't able to be pulled for each tweet. 
## -- For example, if I just pulled emotions, i'd get a vector of 275
## -- but not know which tweets those mapped to. To fix this, I first pull
## -- the element 'status' which will tell whether or not the tweet was 
## -- scored, and use those vector locations to map back to dat3

# Get Status
status <- ldply(1:length(emotion), function(x) emotion[[x]]$status)
status$valid <- row.names(status)
a <- as.numeric(status$valid[status$V1=="OK"])

# Get Emotions for Valid Tweets
anger <- ldply(a, function(x) as.numeric(emotion[[x]]$docEmotions$anger))
disgust <- ldply(a, function(x) as.numeric(emotion[[x]]$docEmotions$disgust))
fear <- ldply(a, function(x) as.numeric(emotion[[x]]$docEmotions$fear))
sadness <- ldply(a, function(x) as.numeric(emotion[[x]]$docEmotions$sadness))
joy <- ldply(a, function(x) as.numeric(emotion[[x]]$docEmotions$joy))

# Merge into Data Frame
emodat <- data.frame(a,anger,disgust,fear,sadness,joy)
names(emodat) <- c("row","anger","disgust","fear","sadness","joy")



#Extract Sentiment
# Get overall sentiment classification (Scored for all)
type <- ldply(1:length(sentiment3), function(x) sentiment3[[x]]$docSentiment$type)

# Get tweets with sentiment score (not neutral)
type$valid <- row.names(status)
a <- as.numeric(type$valid[type$V1!="neutral"])

# Get sentiment score for valid tweets
score <- ldply(a, function(x) as.numeric(sentiment3[[x]]$docSentiment$score))
sent <- data.frame(a,score)
names(sent) <- c("row","score")

type <- type$V1


#Merge back to original data frame
dat3 <- left_join(dat3,emodat, by="row")
dat3 <- left_join(dat3,sent, by="row")
dat3 <- cbind(dat3,type)

saveRDS(dat3,"dat_emotion.RDS")


## Plotzz
stmt_tweets <- dat3 %>%
    group_by(sentiment) %>%
    summarize(tweets=n_distinct(id))

ggplot(stmt_tweets, aes(x=sentiment, y=tweets)) +
    geom_bar(stat="identity") +
    xlab("Sentiment") +
    ylab("Tweets") +
    ggtitle("Tweets about Colin Kaepernick by Sentiment")

ggplot(dat3, aes(x=score)) +
    geom_histogram() +
    xlab("Sentiment Score (Higher Values are More Positive)") +
    ylab("Count") +
    scale_x_continuous(limits=c(-1,1)) +
    ggtitle("Sentiment Distribution of Tweets about Colin Kaepernick")



emo <- dat3 %>%
    filter(type != "neutral") %>%
    group_by(type) %>%
    summarize(anger=median(anger,na.rm=TRUE), disgust=median(disgust, na.rm=TRUE), 
                 fear=median(fear, na.rm=TRUE), sadness=median(sadness, na.rm=TRUE), 
                 joy=median(joy, na.rm=TRUE)) %>%
    melt() %>%
    ggplot(aes(x=variable, y=value, fill=type)) +
    geom_bar(position="dodge", stat="identity") +
    ggtitle("Median Tweet Emotions by Sentiment Type") +
    scale_y_continuous(labels = percent) +
    ylab("Percent Emotion Score") +
    xlab("Emotion")

emo_hist <- dat3 %>%
    filter(type != "neutral") %>%
    select(type,anger:joy) %>%
    melt() %>%
    ggplot(aes(x=value)) +
    ggtitle("Emotion Distribution by Sentiment Type") +
    geom_histogram() +
    facet_grid(variable~type, scales = "free_y") +
    scale_x_continuous("Percent Emotion Score")

emo_hist <- dat3 %>%
    filter(type != "neutral") %>%
    select(type,anger:joy) %>%
    melt() %>%
    ggplot(aes(x=value, fill=variable)) +
    ggtitle("Emotion Distribution") +
    geom_density(alpha=.4) +
    scale_x_continuous("Percent Emotion Score")



