#### BIO_ANALYSIS ####
# Desc: File just for the Analysis of the Profiles with Bios

library(tidyverse)
library(readr)
library(glmnet)
library(ROCR)
library(lda)
library(randomForest)
library(tm)
library(topicmodels)
library(tidytext)
set.seed(42)
final_data <- read_csv("~/Documents/MSD/github_project/data/final_data.csv")
final_data <- final_data %>% mutate(followers = ifelse(is.na(followers),0,followers),
                                    following = ifelse(is.na(following),0,following),
                                    public_repos = ifelse(is.na(public_repos),0,public_repos),
                                    public_gists = ifelse(is.na(public_gists),0,public_gists),
                                    has_bio=ifelse(!is.na(bio)>0, 1, 0)) %>% filter(has_bio==1) ## Key Difference
### List of Features
# Custom Domain
# Custom email domain
# Topic Modeling


## Let's Trying Building a Model?

# Building the dataframe
# Defining the Threshold as the Median
# To avoid Skew, just Doing binary for now for above_pop
predictive <- final_data %>% mutate(has_location=ifelse(!is.na(location)>0, 1, 0),
                                    has_company=ifelse(!is.na(company)>0, 1, 0),
                                    has_blog=ifelse(!is.na(blog)>0, 1, 0),
                                    has_name=ifelse(!is.na(name)>0, 1, 0),
                                    hireable=ifelse(is.na(hireable),0,1),
                                    above_pop=ifelse(followers>median(followers, na.rm=T), 1, 0),
                                    fol_scaled=(followers-mean(followers))/sd(followers),
                                    bio_length=ifelse(is.na(nchar(bio)),0,nchar(bio)), # no effect on the AUC of Model
                                    bio_scaled=(bio_length-mean(bio_length))/sd(bio_length),
                                    following_scaled=(following-mean(following))/sd(following),
                                    repos_scaled=(public_repos-mean(public_repos))/sd(public_repos),
                                    age=(as.numeric(Sys.time()-created_at)),
                                    age_scaled=(age-mean(age))/sd(age),
                                    gists_scaled=(public_gists-mean(public_gists))/sd(public_gists)) %>% 
  select(userId, has_bio, has_location, has_company, has_blog, has_name, following_scaled, repos_scaled, gists_scaled,
         above_pop, fol_scaled, bio_length, bio_scaled, hireable, age_scaled, closed_merge_frac)

# Statistical Significance Aside:
significance <- final_data %>% mutate(has_location=ifelse(!is.na(location)>0, 1, 0),
                                    has_company=ifelse(!is.na(company)>0, 1, 0),
                                    has_blog=ifelse(!is.na(blog)>0, 1, 0),
                                    has_name=ifelse(!is.na(name)>0, 1, 0),
                                    hireable=ifelse(is.na(hireable),0,1),
                                    above_pop=ifelse(followers>median(followers, na.rm=T), 1, 0),
                                    fol_scaled=(followers-mean(followers))/sd(followers),
                                    bio_length=ifelse(is.na(nchar(bio)),0,nchar(bio)), # no effect on the AUC of Model
                                    bio_scaled=(bio_length-mean(bio_length))/sd(bio_length),
                                    following_scaled=(following-mean(following))/sd(following),
                                    repos_scaled=(public_repos-mean(public_repos))/sd(public_repos),
                                    gists_scaled=(public_gists-mean(public_gists))/sd(public_gists))
                                    

## Linear Regression Time
###### WE GONNA GIVE A TED TALK
fit <- lm(closed_merge_frac ~ has_location + has_company +
            has_blog + has_name + hireable + above_pop + following_scaled + fol_scaled +
            bio_scaled + repos_scaled + gists_scaled, age_scaled, significance)
summary(fit)

## Significant Values: hireable, above_pop, fol_scaled, gists_scaled

## Let's try Doing Logistic Regression Here
## First: Plotting Over Thresholds for All Features

models <- matrix(NA, nrow=length(seq(0.01,.99,0.01)), ncol=3)
count <- 1
for (i in seq(.01,.99, 0.01)) {
  pred_fixed <- predictive %>% mutate(good_coder=ifelse(closed_merge_frac>i, 1, 0)) %>% 
    select(-closed_merge_frac)
  data <- pred_fixed %>% select(-good_coder, -userId)
  #data <- pred_fixed %>% select(above_pop, fol_scaled, bio_scaled, bio_length, hireable,
  #                              repos_scaled,)
  
  # 90% Train-Test Split
  ndx <- sample(nrow(data), floor(nrow(data) * 0.9))
  train <- as.matrix(data[ndx,])
  test <- as.matrix(data[-ndx,])
  
  ## Let's get the Good Coder Values
  trainy <- pred_fixed[ndx,]$good_coder
  test_y <- pred_fixed[-ndx,]$good_coder
  
  cvfit <- cv.glmnet(train, trainy, family = "binomial", type.measure = "auc")
  
  ## Getting the Train Accuracy
  tmp <- data.frame(pred=(predict(cvfit,newx=train,s="lambda.min", type="class")), real=trainy)
  acc <- tmp %>% mutate(right=ifelse(X1==real,1,0)) %>% summarize(acc=sum(right)/nrow(tmp))
  
  # Build the DataFrame
  models[count,] <-c(i, max(cvfit$cvm), acc[1,1])
  count <- count + 1
}
models <- data.frame(models)
colnames(models) <- c("threshold","auc","acc")

## Plot the Thresholds:
# Over AUC
models %>% ggplot(aes(x=threshold,y=auc)) + geom_line() +
  labs(title="AUC over Thresholds (Just Bio)") +
  geom_vline(aes(xintercept = median(final_data$closed_merge_frac)), color="red")

# Over Accuracy
models %>% ggplot(aes(x=threshold,y=acc)) + geom_line() +
  labs(title="Accuracy over Thresholds (Just Bio)") +
  geom_vline(aes(xintercept = median(final_data$closed_merge_frac)), color="red")

## Second: Plotting Over Thresholds for Sig. Features

models_sub <- matrix(NA, nrow=length(seq(0.01,.99,0.01)), ncol=3)
count <- 1
for (i in seq(.01,.99, 0.01)) {
  pred_fixed <- predictive %>% mutate(good_coder=ifelse(closed_merge_frac>i, 1, 0)) %>% 
    select(hireable, above_pop, fol_scaled, gists_scaled, age_scaled, good_coder)
  data <- pred_fixed %>% select(-good_coder)
  #data <- pred_fixed %>% select(above_pop, fol_scaled, bio_scaled, bio_length, hireable,
  #                              repos_scaled)
  
  # 90% Train-Test Split
  ndx <- sample(nrow(data), floor(nrow(data) * 0.9))
  train <- as.matrix(data[ndx,])
  test <- as.matrix(data[-ndx,])
  
  ## Let's get the Good Coder Values
  trainy <- pred_fixed[ndx,]$good_coder
  test_y <- pred_fixed[-ndx,]$good_coder
  
  cvfit <- cv.glmnet(train, trainy, family = "binomial", type.measure = "auc")
  
  ## Getting the Train Accuracy
  tmp <- data.frame(pred=(predict(cvfit,newx=train,s="lambda.min", type="class")), real=trainy)
  acc <- tmp %>% mutate(right=ifelse(X1==real,1,0)) %>% summarize(acc=sum(right)/nrow(tmp))
  
  # Build the DataFrame
  models_sub[count,] <-c(i, max(cvfit$cvm), acc[1,1])
  count <- count + 1
}
models_sub <- data.frame(models_sub)
colnames(models_sub) <- c("threshold","auc","acc")

## Plot the Thresholds:
# Over AUC
models_sub %>% ggplot(aes(x=threshold,y=auc)) + geom_line() +
  labs(title="AUC over Thresholds (Just Bio, Significant Features)") +
  geom_vline(aes(xintercept = median(final_data$closed_merge_frac)), color="red")

# Over Accuracy
models_sub %>% ggplot(aes(x=threshold,y=acc)) + geom_line() +
  labs(title="Accuracy over Thresholds (Just Bio, Significant Features)") +
  geom_vline(aes(xintercept = median(final_data$closed_merge_frac)), color="red")

## Let's Do Random Forest one more Time
## Haven't had any luck with logistic, let's try Random Forests?

pred_fixed <- predictive %>% mutate(good_coder=ifelse(closed_merge_frac>median(closed_merge_frac), 1, 0)) %>% 
  select(hireable, above_pop, fol_scaled, gists_scaled, good_coder)
data <- pred_fixed %>% select(-good_coder)
#data <- pred_fixed %>% select(above_pop, fol_scaled, bio_scaled, bio_length, hireable,
#                              repos_scaled)

# 90% Train-Test Split
ndx <- sample(nrow(data), floor(nrow(data) * 0.9))
train <- as.matrix(data[ndx,])
test <- as.matrix(data[-ndx,])

## Let's get the Good Coder Values
trainy <- pred_fixed[ndx,]$good_coder
test_y <- pred_fixed[-ndx,]$good_coder

forrest <- randomForest(train, as.factor(trainy))
forrest ## As you can see, it doesn't do much better

## I look around my data analysis and I see desolation
### Let's do some Topic Modeling

# create a Corpus from the user bios
corpus <- VCorpus(VectorSource(final_data$bio))

# remove punctuation and numbers, lowercase as well
corpus <- corpus %>% tm_map(removeNumbers) %>% tm_map(removePunctuation)
corpus <- tm_map(corpus , content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords('english'))
# create a DocumentTermMatrix from the Bios Corpus
dtm <- DocumentTermMatrix(corpus)

# Removing 0 entry lines from dtm, from stackoverflow: 
# http://stackoverflow.com/questions/13944252/remove-empty-documents-from-documenttermmatrix-in-r-topicmodels
rowTotals <- apply(dtm , 1, sum)   #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]           #remove all docs without words

bio_lda <- LDA(dtm.new, k = 10, control = list(seed = 1234))

bio_topics <- tidy(bio_lda, matrix = "beta")

bio_top_terms <- bio_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

bio_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
