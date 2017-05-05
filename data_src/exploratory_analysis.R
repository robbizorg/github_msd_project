library(tidyverse)
library(readr)
library(glmnet)
library(ROCR)
library(randomForest)
set.seed(42)
final_data <- read_csv("~/Documents/MSD/github_project/data/final_data.csv")
final_data <- final_data %>% mutate(followers = ifelse(is.na(followers),0,followers),
                                    following = ifelse(is.na(following),0,following),
                                    public_repos = ifelse(is.na(public_repos),0,public_repos),
                                    public_gists = ifelse(is.na(public_gists),0,public_gists)) %>% filter(count>=15)

# Plotting the Histogram of the Distribution of the Fraction
final_data %>% ggplot(aes(x=closed_merge_frac)) + geom_histogram()

# Shwoing the Fraction of Various Different Features
final_data %>% mutate(has_bio=ifelse(!is.na(bio)>0, 1, 0)) %>% select(userId, bio, has_bio) %>% 
	group_by(has_bio) %>% summarize(count=n()) %>% ungroup() %>% 
	mutate(frac=count/sum(count))

final_data %>% mutate(has_company=ifelse(!is.na(company)>0, 1, 0)) %>% 
	select(userId, company, has_company) %>% group_by(has_company) %>% 
	summarize(count=n()) %>% ungroup() %>% mutate(frac=count/sum(count))

final_data %>% mutate(has_blog=ifelse(!is.na(blog)>0, 1, 0)) %>% select(userId, blog, has_blog) %>% group_by(has_blog) %>% 
	summarize(count=n()) %>% ungroup() %>% mutate(frac=count/sum(count))

final_data %>% mutate(has_name=ifelse(!is.na(name)>0, 1, 0)) %>% 
	group_by(has_name) %>% summarize(count=n()) %>% ungroup() %>% 
	mutate(frac=count/sum(count))

final_data %>% mutate(has_location=ifelse(!is.na(location)>0, 1, 0)) %>% 
	group_by(has_location) %>% summarize(count=n()) %>% ungroup() %>% 
	mutate(frac=count/sum(count))

final_data %>% mutate(hireable=ifelse(!is.na(hireable)>0, 1, 0)) %>% 
  group_by(hireable) %>% summarize(count=n()) %>% ungroup() %>% 
  mutate(frac=count/sum(count))

## Plotting Distribution of Merge Frac based on these Features
final_data %>% mutate(has_bio=ifelse(!is.na(bio)>0, 1, 0)) %>% 
  ggplot(aes(x=closed_merge_frac)) + facet_wrap(~has_bio) + geom_histogram() + 
  labs(title="Distribution of closed_merge_frac on Bio") + 
  geom_vline(aes(xintercept = median(closed_merge_frac)), color="red")

final_data %>% mutate(has_company=ifelse(!is.na(company)>0, 1, 0)) %>% 
  ggplot(aes(x=closed_merge_frac)) + facet_wrap(~has_company) + geom_histogram() + 
  labs(title="Distribution of closed_merge_frac on Company") + 
  geom_vline(aes(xintercept = median(closed_merge_frac)), color="red")

final_data %>% mutate(has_blog=ifelse(!is.na(blog)>0, 1, 0)) %>% 
  ggplot(aes(x=closed_merge_frac)) + facet_wrap(~has_blog) + geom_histogram() + 
  labs(title="Distribution of closed_merge_frac on Blog") + 
  geom_vline(aes(xintercept = median(closed_merge_frac)), color="red")

final_data %>% mutate(has_name=ifelse(!is.na(name)>0, 1, 0)) %>% 
  ggplot(aes(x=closed_merge_frac)) + facet_wrap(~has_name) + geom_histogram() + 
  labs(title="Distribution of closed_merge_frac on Name") + 
  geom_vline(aes(xintercept = median(closed_merge_frac)), color="red")

final_data %>% mutate(has_location=ifelse(!is.na(location)>0, 1, 0)) %>% 
  ggplot(aes(x=closed_merge_frac)) + facet_wrap(~has_location) + geom_histogram() + 
  labs(title="Distribution of closed_merge_frac on Location") + 
  geom_vline(aes(xintercept = median(closed_merge_frac)), color="red")

final_data %>% mutate(above_pop=ifelse(followers>median(followers, na.rm=T), 1, 0)) %>% 
  ggplot(aes(x=closed_merge_frac)) + facet_wrap(~above_pop) + geom_histogram() + 
  labs(title="Distribution of closed_merge_frac on Followers") + 
  geom_vline(aes(xintercept = median(closed_merge_frac)), color="red")

final_data %>% mutate(hireable=ifelse(!is.na(hireable)>0, 1, 0)) %>% 
  ggplot(aes(x=closed_merge_frac)) + facet_wrap(~hireable) + geom_histogram() + 
  labs(title="Distribution of closed_merge_frac on Followers") + 
  geom_vline(aes(xintercept = median(closed_merge_frac)), color="red")

## closed_merge_frac as a Function of Followers
final_data %>%
  ggplot(aes(x=followers, y=closed_merge_frac)) + geom_point() + 
  labs(title="Merge Frac as Function of Followers")

final_data %>% mutate(has_bio=ifelse(!is.na(bio), 1, 0)) %>% 
  ggplot(aes(x=followers, y=closed_merge_frac)) + geom_point() + facet_wrap(~has_bio) +
  labs(title="Merge Frac as Function of Followers (per Bio)")

# closed_merge_frac as a Function of Bio_Length
final_data %>% mutate(bio_length=ifelse(is.na(nchar(bio)),0,nchar(bio))) %>%
  ggplot(aes(x=bio_length, y=closed_merge_frac)) + geom_point() + 
  labs(title="Merge Frac as Function of Bio Length")

# Closed_Merge_Frac as a Function of Age of User Account
final_data %>% mutate(age=(Sys.time()-created_at)) %>%
  ggplot(aes(x=age, y=closed_merge_frac)) + geom_point() + 
  labs(title="Merge Frac as Function of Age")

## Let's Trying Building a Model?

# Building the dataframe
# Defining the Threshold as the Median
# To avoid Skew, just Doing binary for now for above_pop
predictive <- final_data %>% mutate(has_bio=ifelse(!is.na(bio)>0, 1, 0),
                                    has_location=ifelse(!is.na(location)>0, 1, 0),
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
                                    email_domain=ifelse(grepl("^.*@gmail.com", email), 0, 1),
                                    gists_scaled=(public_gists-mean(public_gists))/sd(public_gists)) %>% 
  select(userId, has_bio, has_location, has_company, has_blog, has_name, following_scaled, repos_scaled, gists_scaled,
         above_pop, fol_scaled, bio_length, bio_scaled, hireable, age_scaled, email_domain, closed_merge_frac)

# Statistical Significance Aside:
significance <- final_data %>% mutate(has_bio=ifelse(!is.na(bio)>0, 1, 0),
                                    has_location=ifelse(!is.na(location)>0, 1, 0),
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
                                    gists_scaled=(public_gists-mean(public_gists))/sd(public_gists),
                                    age=(as.numeric(Sys.time()-created_at)),
                                    age_scaled=(age-mean(age))/sd(age),
                                    email_domain=ifelse(grepl("^.*@gmail.com", email), 0, 1),
                                    good_coder=ifelse(closed_merge_frac>quantile(closed_merge_frac, 0.25), 1, 0))

neg <- significance %>% filter(good_coder==0)
neg <- data.frame(neg)
pos <- significance %>% filter(good_coder==1)
pos <- data.frame(pos)
iter <- length(names(predictive)) - 2
count <- 1
results <- matrix(NA, nrow=iter, ncol=3)
for (i in names(predictive)) {
  if (identical(i, "good_coder") | identical(i, "userId")) {
    next
  }
  res <- t.test(neg[i], pos[i], conf.level = 0.95)
  results[count,] <-c(i, as.numeric(res$statistic), as.numeric(res$p.value))
  count <- count + 1
}
results <- data.frame(results)
colnames(results) <- c("feature","t_statistic","p_value")

## Linear Regression Time
###### WE GONNA GIVE A TED TALK
fit <- lm(closed_merge_frac ~ has_bio + has_location + has_company +
            has_blog + has_name + hireable + above_pop + following_scaled + fol_scaled +
            bio_scaled + repos_scaled + gists_scaled + age_scaled + email_domain, significance)
summary(fit)

## Try Logistic Regression over different Thresholds
# Possibly only try to predict on the first and last quartiles

models <- matrix(NA, nrow=length(seq(0.01,.99,0.01)), ncol=3)
count <- 1
for (i in seq(.01,.99, 0.01)) {
  pred_fixed <- predictive %>% mutate(good_coder=ifelse(closed_merge_frac>i, 1, 0)) %>% 
    select(-closed_merge_frac)
  data <- pred_fixed %>% select(-good_coder, -userId)
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
  models[count,] <-c(i, max(cvfit$cvm), acc[1,1])
  count <- count + 1
}
models <- data.frame(models)
colnames(models) <- c("threshold","auc","acc")

## Plot the Thresholds:
# Over AUC
models %>% ggplot(aes(x=threshold,y=auc)) + geom_line() +
  labs(title="AUC over Thresholds") +
  geom_vline(aes(xintercept = median(final_data$closed_merge_frac)), color="red")

# Over Accuracy
models %>% ggplot(aes(x=threshold,y=acc)) + geom_line() +
  labs(title="Accuracy over Thresholds") +
  geom_vline(aes(xintercept = median(final_data$closed_merge_frac)), color="red")
#data <- predictive %>% select(-good_coder, -userId)

## Haven't had any luck with logistic, let's try Random Forests?

pred_fixed <- predictive %>% mutate(good_coder=ifelse(closed_merge_frac>median(closed_merge_frac), 1, 0)) %>% 
  select(-closed_merge_frac)
data <- pred_fixed %>% select(-good_coder, -userId)
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
