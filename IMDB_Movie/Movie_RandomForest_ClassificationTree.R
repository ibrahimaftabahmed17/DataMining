library(ggplot2) # visualization
library(ggrepel)
library(ggthemes) # visualization
library(scales) # visualization
library(dplyr) # data manipulation
install.packages('GGally')
library(VIM)

library(data.table)
library(formattable)
library(plotly)
library(corrplot)
library(GGally)
library(caret)
library(car)

IMDB <- read.csv('C:/Users/ibrah/Downloads/movie_metadata.csv')
IMDB <- subset(IMDB, select = -c(genres))
# duplicate rows
sum(duplicated(IMDB))
# delete duplicate rows
IMDB <- IMDB[!duplicated(IMDB), ]

library(stringr)
IMDB$movie_title <- gsub("Â", "", as.character(factor(IMDB$movie_title)))
str_trim(IMDB$movie_title, side = "right")

colSums(sapply(IMDB, is.na))

missing.values <- aggr(IMDB, sortVars = T, prop = T, sortCombs = T, cex.lab = 1.5, cex.axis = .6, cex.numbers = 5, combined = F, gap = -.2)

IMDB <- IMDB[!is.na(IMDB$gross), ]
IMDB <- IMDB[!is.na(IMDB$budget), ]
dim(IMDB)

sum(complete.cases(IMDB))

colSums(sapply(IMDB, is.na))

table(IMDB$aspect_ratio)
class(IMDB$aspect_ratio)

IMDB$aspect_ratio[is.na(IMDB$aspect_ratio)] <- 0
mean(IMDB$movie_score[IMDB$aspect_ratio == 1.85])
mean(IMDB$movie_score[IMDB$aspect_ratio == 2.35])
mean(IMDB$movie_score[IMDB$aspect_ratio != 1.85 & IMDB$aspect_ratio != 2.35])

IMDB <- subset(IMDB, select = -c(aspect_ratio))

# replace NA with column average for facenumber_in_poster
IMDB$facenumber_in_poster[is.na(IMDB$facenumber_in_poster)] <- round(mean(IMDB$facenumber_in_poster, na.rm = TRUE))
# convert 0s into NAs for other predictors
IMDB[,c(5,6,8,13,24,26)][IMDB[,c(5,6,8,13,24,26)] == 0] <- NA
# impute missing value with column mean
IMDB$num_critic_for_reviews[is.na(IMDB$num_critic_for_reviews)] <- round(mean(IMDB$num_critic_for_reviews, na.rm = TRUE))
IMDB$duration[is.na(IMDB$duration)] <- round(mean(IMDB$duration, na.rm = TRUE))
IMDB$director_facebook_likes[is.na(IMDB$director_facebook_likes)] <- round(mean(IMDB$director_facebook_likes, na.rm = TRUE))
IMDB$actor_3_facebook_likes[is.na(IMDB$actor_3_facebook_likes)] <- round(mean(IMDB$actor_3_facebook_likes, na.rm = TRUE))
IMDB$actor_1_facebook_likes[is.na(IMDB$actor_1_facebook_likes)] <- round(mean(IMDB$actor_1_facebook_likes, na.rm = TRUE))
IMDB$cast_total_facebook_likes[is.na(IMDB$cast_total_facebook_likes)] <- round(mean(IMDB$cast_total_facebook_likes, na.rm = TRUE))
IMDB$actor_2_facebook_likes[is.na(IMDB$actor_2_facebook_likes)] <- round(mean(IMDB$actor_2_facebook_likes, na.rm = TRUE))
IMDB$movie_facebook_likes[is.na(IMDB$movie_facebook_likes)] <- round(mean(IMDB$movie_facebook_likes, na.rm = TRUE))


table(IMDB$content_rating)
IMDB <- IMDB[!(IMDB$content_rating %in% ""),]
IMDB$content_rating[IMDB$content_rating == 'M']   <- 'PG' 
IMDB$content_rating[IMDB$content_rating == 'GP']  <- 'PG' 
IMDB$content_rating[IMDB$content_rating == 'X']   <- 'NC-17'

IMDB$content_rating[IMDB$content_rating == 'Approved']  <- 'R' 
IMDB$content_rating[IMDB$content_rating == 'Not Rated'] <- 'R' 
IMDB$content_rating[IMDB$content_rating == 'Passed']    <- 'R' 
IMDB$content_rating[IMDB$content_rating == 'Unrated']   <- 'R' 
IMDB$content_rating <- factor(IMDB$content_rating)
table(IMDB$content_rating)

IMDB <- IMDB %>% 
  mutate(profit = gross - budget,
         return_on_investment_perc = (profit/budget)*100)

table(IMDB$color)

# delete predictor color
IMDB <- subset(IMDB, select = -c(color))

IMDB <- subset(IMDB, select = -c(language))

table(IMDB$country)

levels(IMDB$country) <- c(levels(IMDB$country), "Others")
IMDB$country[(IMDB$country != 'USA')&(IMDB$country != 'UK')] <- 'Others' 
IMDB$country <- factor(IMDB$country)
table(IMDB$country)

ggplot(IMDB, aes(title_year)) +
  geom_bar() +
  labs(x = "Year movie was released", y = "Movie Count", title = "Histogram of Movie released") +
  theme(plot.title = element_text(hjust = 0.5))

IMDB <- IMDB[IMDB$title_year >= 1980,]

IMDB %>%
  filter(title_year %in% c(2000:2016)) %>%
  arrange(desc(profit)) %>%
  top_n(20, profit) %>%
  ggplot(aes(x=budget/1000000, y=profit/1000000)) +
  geom_point() +
  geom_smooth() + 
  geom_text_repel(aes(label=movie_title)) +
  labs(x = "Budget $million", y = "Profit $million", title = "Top 10 Profitable Movies") +
  theme(plot.title = element_text(hjust = 0.5))

IMDB %>%
  filter(budget > 100000) %>%
  mutate(profit = gross - budget,
         return_on_investment_perc = (profit/budget)*100) %>%
  arrange(desc(profit)) %>%
  top_n(20, profit) %>%
  ggplot(aes(x=budget/1000000, y = return_on_investment_perc)) + 
  geom_point(size = 2) + 
  geom_smooth(size = 1) + 
  geom_text_repel(aes(label = movie_title), size = 3) + 
  xlab("Budget $million") + 
  ylab("Percent Return on Investment") + 
  ggtitle("20 Most Profitable Movies based on its Return on Investment")

IMDB %>%
  group_by(director_name) %>%
  summarise(avg_imdb = mean(movie_score)) %>%
  arrange(desc(avg_imdb)) %>%
  top_n(20, avg_imdb) %>%
  formattable(list(avg_imdb = color_bar("orange")), align = 'l')

IMDB %>%
  top_n(20, profit) %>%
  ggplot(aes(x = movie_score, y = gross/10^6, size = profit/10^6, color = content_rating)) + 
  geom_point() + 
  geom_hline(aes(yintercept = 600)) + 
  geom_vline(aes(xintercept = 7.75)) + 
  geom_text_repel(aes(label = movie_title), size = 4) +
  xlab("Imdb score") + 
  ylab("Gross money earned in million dollars") + 
  ggtitle("Commercial success Vs Critical acclaim") +
  annotate("text", x = 8.5, y = 700, label = "High ratings \n & High gross") +
  theme(plot.title = element_text(hjust = 0.5))

IMDB %>%
  plot_ly(x = ~movie_facebook_likes, y = ~movie_score, color = ~content_rating , mode = "markers", text = ~content_rating, alpha = 0.7, type = "scatter")

# number of directors
sum(uniqueN(IMDB$director_name))
# number of actors
sum(uniqueN(IMDB[, c("actor_1_name", "actor_2_name", "actor_3_name")]))

IMDB <- subset(IMDB, select = -c(director_name, actor_2_name, actor_1_name,
                                 movie_title, actor_3_name, plot_keywords, 
                                 movie_movie_link))

IMDB <- subset(IMDB, select = -c(profit, return_on_investment_perc))

ggcorr(IMDB, label = TRUE, label_round = 2, label_size = 3.5, size = 2, hjust = .85) +
  ggtitle("Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))

# add up actor 2 and 3 facebook likes into other actors facebook likes
IMDB$other_actors_facebook_likes <- IMDB$actor_2_facebook_likes + IMDB$actor_3_facebook_likes
# use the ratio of critical reviews amount to total reviews amount
IMDB$critic_review_ratio <- IMDB$num_critic_for_reviews / IMDB$num_user_for_reviews
# delete columns
IMDB <- subset(IMDB, select = -c(cast_total_facebook_likes, actor_2_facebook_likes, actor_3_facebook_likes, num_critic_for_reviews, num_user_for_reviews))

ggcorr(IMDB, label = TRUE, label_round = 2, label_size = 4, size = 3, hjust = .85) +
  ggtitle("Correlation Heatmap") +
  theme(plot.title = element_text(hjust = 0.5))

IMDB$binned_score <- cut(IMDB$movie_score, breaks = c(0,4,6,8,10))

IMDB <- IMDB[,c(9,4,5,14,12,2,3,13,1,6,10,7,8,11,15)]
colnames(IMDB) <- c("budget", "gross", "user_vote", "critic_review_ratio",
                    "movie_fb", "director_fb", "actor1_fb", "other_actors_fb",
                    "duration", "face_number", "year", "country", "content",
                    "imdb_score", "binned_score")
View(IMDB)


set.seed(45)
train.index <- sample(row.names(IMDB), dim(IMDB)[1]*0.6)
valid.index <- sample(setdiff(row.names(IMDB), train.index), dim(IMDB)[1]*0.2)
test.index <- setdiff(row.names(IMDB), union(train.index, valid.index))
train <- IMDB[train.index, ]
valid <- IMDB[valid.index, ]
test <- IMDB[test.index, ]

library(randomForest)

rf <- randomForest(binned_score ~ . -movie_score, data = train, mtry = 5)
# Show model error
plot(rf)
legend('topright', colnames(rf$err.rate), col=1:5, fill=1:5)

# Get importance
importance <- importance(rf)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

# apply model on validation set
rf.pred.valid <- predict(rf, valid)
# generate confusion matrix for validation data
confusionMatrix(rf.pred.valid, valid$binned_score)

# apply model on test set
rf.pred.test <- predict(rf, test)
# generate confusion matrix for test data
confusionMatrix(rf.pred.test, test$binned_score)




library(rpart)
library(rpart.plot)
# Full grown tree
class.tree <- rpart(binned_score ~ . -movie_score, data = train, method = "class")
## plot tree
prp(class.tree, type = 1, extra = 1, under = TRUE, split.font = 2, varlen = 0)


cv.ct <- rpart(binned_score ~ . -movie_score, data = train, method = "class", 
               cp = 0.00001, minsplit = 5, xval = 5)
printcp(cv.ct)


pruned.ct <- prune(cv.ct, 
                   cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10)


# apply model on training set
tree.pred.train <- predict(pruned.ct, train, type = "class")
# generate confusion matrix for training data
confusionMatrix(tree.pred.train, train$binned_score)

# apply model on validation set
tree.pred.valid <- predict(pruned.ct, valid, type = "class")
# generate confusion matrix for validation data
confusionMatrix(tree.pred.valid, valid$binned_score)

# apply model on test set
tree.pred.test <- predict(pruned.ct, test, type = "class")
# generate confusion matrix for test data
confusionMatrix(tree.pred.test, test$binned_score)
