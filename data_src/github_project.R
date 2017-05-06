library(tidyverse)
library(readr)
library(glmnet)
library(ROCR)
library(randomForest)
library(modelr)
library(combinat)
library(plotrix)
library(reshape)
set.seed(42)
final_data <- read_csv("./data/final_data.csv")
final_data <- final_data %>% mutate(followers = ifelse(is.na(followers),0,followers),
                                    following = ifelse(is.na(following),0,following),
                                    public_repos = ifelse(is.na(public_repos),0,public_repos),
                                    public_gists = ifelse(is.na(public_gists),0,public_gists)) %>% filter(count>=15)

## Let's Graph All the Data
data <- final_data %>% mutate(has_bio=ifelse(!is.na(bio)>0, 1, 0),
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
                              email_domain=ifelse(grepl("^.*@gmail.com|^.*@yahoo.com|^.*@outlook.com|^.*@aol.com|^.*@zoho.com|^.*@mail.com|^.*@yandex|^.*@protonmail|^.*@gmx|^.*@icloud",
                                                        email)|is.na(email), 0, 1),
                              good_coder=ifelse(closed_merge_frac>quantile(closed_merge_frac, 0.25), 1, 0)) %>%
  select(userId, has_bio, has_location, has_company, has_blog, has_name, following_scaled, repos_scaled, gists_scaled,
         above_pop, fol_scaled, bio_length, bio_scaled, hireable, age, age_scaled, email_domain, closed_merge_frac)
remove_cols <- c("userId", "following_scaled", "repos_scaled", "gists_scaled", "fol_scaled", "bio_scaled", "age_scaled", "closed_merge_frac", "bio_length")
data2 <- data[, !(names(data) %in% remove_cols)]

hist(final_data$closed_merge_frac, xlab="Pull Request Merge Rate", ylab="Frequency (Number of Users)", main="Distribution of Pull Request Merge Rate")

###################### Graph 3
# Compute closed_merge_frac for each boolean variable
indicator_cols <- c("has_bio", "has_company", "has_blog", "has_name", "hireable", "email_domain")

summary <- data %>% group_by(has_location) %>% summarise(mean_frac=mean(closed_merge_frac))
summary_sds <- data %>% group_by(has_location) %>% summarise(sd_frac=std.error(closed_merge_frac))
colnames(summary)[2] <- colnames(summary)[1]
colnames(summary)[1] <- "indicator"
colnames(summary_sds)[2] <- colnames(summary_sds)[1]
colnames(summary_sds)[1] <- "indicator"

for (i in 1:length(indicator_cols)) {
  summary2 <- data %>% group_by_(indicator_cols[i]) %>% summarise(mean_frac=mean(closed_merge_frac))
  sds <- data %>% group_by_(indicator_cols[i]) %>% summarise(sd_frac=std.error(closed_merge_frac))
  colnames(summary2)[2] <- colnames(summary2)[1]
  colnames(summary2)[1] <- "indicator"
  colnames(sds)[2] <- colnames(sds)[1]
  colnames(sds)[1] <- "indicator"
  summary <- merge(summary, summary2, by="indicator")
  summary_sds <- merge(summary_sds, sds, by="indicator")
}

colnames(summary) <- c("indicator", "location", "bio", "company", "blog", "name", "hireable", "domain")
colnames(summary_sds) <- c("indicator", "location", "bio", "company", "blog", "name", "hireable", "domain")

graph_data_meanfrac <- melt(summary,id.vars="indicator")
graph_data_meanfrac_sds <- melt(summary_sds, id.vars="indicator")
graph_data_meanfrac <- graph_data_meanfrac %>% mutate(mergeby = paste(indicator, variable, sep=""))
graph_data_meanfrac_sds <- graph_data_meanfrac_sds %>% mutate(mergeby = paste(indicator, variable, sep=""))

graph_data_meanfrac <- merge(graph_data_meanfrac, graph_data_meanfrac_sds, by="mergeby")
graph_data <- graph_data_meanfrac[,c(2,3,4,7)]
colnames(graph_data) <- c("indicator", "variable", "value", "se")
ggplot(graph_data ,aes(x=variable,y=value,fill=factor(indicator)))+
  geom_bar(stat="identity", position="dodge", width=0.9)+
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.1, position=position_dodge(0.9)) +
  coord_cartesian(ylim = c(0.625, 0.725)) +
  scale_fill_discrete(name="Value",
                      breaks=c(0, 1),
                      labels=c("False", "True")) +
  xlab("Binary Variable")+ylab("Mean Pull Request Merge Rate") + labs(title="Mean Pull Request Merge Rate by Binary Variable")

###########################
### Generate Table 1 ######

for (i in colnames(data2)) {
  column_data <- data2 %>% group_by_(i) %>% summarise(count=n()/nrow(data2))
  colnames(column_data)[2] <- sprintf("%s_reported", colnames(column_data)[1])
  colnames(column_data)[1] <- "indicator"
  summary <- merge(summary, column_data, by="indicator")
}

########################### Graphs 2
#### generate histograms for continuous variables
cont_vars <- c("followers", "following", "public_gists", "public_repos", "age", "bio_length")
titles <- c("Number of Followers", "Number of Users Following", "Public Gists Created", "Public Repos Owned", "Days Since Profile Creation", "Length of Bio")
limits <- c(c(0, 1500), c(0, 400), c(0, 300), c(0, 750), c(0, 3500), c(0, 175))
x = 1
c = 1
for (i in cont_vars) {
  if (!identical(i, "age") && !identical(i, "bio_length")) {
    print(ggplot(final_data, aes_string(i)) +
            geom_density() + 
            coord_cartesian(xlim = c(limits[x], limits[x+1])) +
            labs(title=sprintf("Distribution of %s", titles[c])) + 
            xlab(titles[c]))
  } else {
    data_biolength <- data %>% filter(bio_length > 0)
    if(identical(i, "age")) {
      data_to_use <- data
    } else {
      data_to_use <- data_biolength 
    }
    print(ggplot(data_to_use, aes_string(i)) +
            geom_density() +
            coord_cartesian(xlim = c(limits[x], limits[x+1])) + 
            labs(title=sprintf("Distribution of %s", titles[c])) + 
            xlab(titles[c]))
  }
  x = x + 2
  c = c+1
}

########################### Graphs 4
##### Generate plot of mean_merge_frac vs number of 0s and 1s
x <- 1
for (i in cont_vars) {
  if (!identical(i,"age") && !identical(i, "bio_length")) {
    arranged_data <- final_data %>% arrange_(i) %>% mutate(number=row_number())
    arranged_data <- arranged_data %>% mutate(percentile=floor(number/nrow(final_data)*10))
    arranged_data[nrow(arranged_data),]$percentile <- 9
    arranged_summary <- arranged_data %>% group_by(percentile) %>% summarise(mean=mean(closed_merge_frac))
    #arranged_summary <- arranged_summary[1:10,]
    print(ggplot(arranged_summary ,aes(x=percentile,y=mean,fill=factor(percentile)))+
            geom_bar(stat="identity", position="dodge") +
            coord_cartesian(ylim = c(0.625, 0.75)) +
            theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
            scale_fill_discrete(breaks=0:9,
                                labels=c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90-100%")) +
            xlab(sprintf("%s Percentile", titles[x]))+ylab("Pull Request Merge Rate") + labs(title=sprintf("Pull Request Merge Rate vs. %s", titles[x]), fill="Percentile"))
  } else {
    arranged_data <- data %>% arrange_(i) %>% mutate(number=row_number())
    arranged_data <- arranged_data %>% mutate(percentile=floor(number/nrow(data)*10))
    arranged_data[nrow(arranged_data),]$percentile <- 9
    arranged_summary <- arranged_data %>% group_by(percentile) %>% summarise(mean=mean(closed_merge_frac))
    print(ggplot(arranged_summary ,aes(x=percentile,y=mean,fill=factor(percentile)))+
            geom_bar(stat="identity", position="dodge") +
            coord_cartesian(ylim = c(0.6, 0.75)) +
            theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
            scale_fill_discrete(breaks=0:9,
                                labels=c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90-100%")) +
            xlab(sprintf("%s Percentile", titles[x]))+ylab("Pull Request Merge Rate") + labs(title=sprintf("Pull Request Merge Rate vs. %s", titles[x]), fill="Percentile"))
  }
  x = x+1
}


## Let's Trying Building a Model

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
                                    email_domain=ifelse(grepl("^.*@gmail.com|^.*@yahoo.com|^.*@outlook.com|^.*@aol.com|^.*@zoho.com|^.*@mail.com|^.*@yandex|^.*@protonmail|^.*@gmx|^.*@icloud",
                                                              email)|is.na(email), 0, 1),
                                    gists_scaled=(public_gists-mean(public_gists))/sd(public_gists)) %>% 
  select(userId, has_bio, has_location, has_company, has_blog, has_name, following_scaled, repos_scaled, gists_scaled,
         above_pop, fol_scaled, bio_length, bio_scaled, hireable, age_scaled, email_domain, closed_merge_frac)

## Holdout Dataset:
# 90% Train-Test Split
ndx <- sample(nrow(predictive), floor(nrow(predictive) * 0.9))
holdout <- predictive[-ndx,]
predictive <- predictive[ndx,]

## Results Final Holder Variable
total_results <- data.frame()

# Helper Function
compDegree <- function(df, n, var) {
  for (i in 1:(n+1)){
    #print(i)
    varname <- paste(var, i , sep=".")
    df[[varname]] <- with(df, df[[var]] ^ i)
  }
  df
}

## Folds
folds <- 5

## The Disclosed Indicator Values

kfold_predictive <- predictive 
kfold_predictive <- kfold_predictive [sample(1:nrow(kfold_predictive)),]
kfold_predictive  <- kfold_predictive %>% mutate(fold=row_number() %% 5 + 1)
results <- data.frame()
pred_fixed <- kfold_predictive %>% mutate(good_coder=closed_merge_frac) %>% 
  select(-closed_merge_frac)

pred_result <- kfold_predictive %>% select(userId)

for (name in c("has_name", "has_bio", "has_blog", "has_location", "email_domain", "has_company", "hireable")) {
  if (identical(name, "good_coder") | identical(name, "userId")) {
    next
  }
  
  tmp <- compDegree(df=pred_fixed, n=1, var=name) %>% select(-good_coder, -fold, -has_bio, -has_location, -has_company,
                                                             -has_blog, -has_name, -hireable, -above_pop, -following_scaled, -fol_scaled,
                                                             -bio_scaled, -repos_scaled, -gists_scaled, -age_scaled, -email_domain, -bio_length)
  pred_result <- left_join(pred_result, tmp, by="userId")
}

pred_result <- left_join(pred_result, select(pred_fixed, userId, fold, good_coder))
my_data <- pred_result %>% select(-userId)

#data <- pred_fixed %>% select(above_pop, fol_scaled, bio_scaled, bio_length, hireable,
#                              repos_scaled)

total_rsme_train <- data.frame()
total_rsme_test <- data.frame()
avg_rsme_test <- c()
avg_rsme_train <- c()

for (i in 1:folds) {
  test <- my_data %>% filter(fold==i)
  train <- my_data %>% filter(fold!=i)
  model <- lm(good_coder ~ ., data = train)
  
  train <- train %>% add_predictions(model)
  test <- test %>% add_predictions(model)
  
  rsme_train <- train %>% summarize(rsme_train=sqrt(mean((pred - good_coder)^2)))
  rsme_test <- test %>% summarize(rsme_test=sqrt(mean((pred - good_coder)^2)))
  total_rsme_train <- rbind(total_rsme_train, rsme_train)
  total_rsme_test <- rbind(total_rsme_test, rsme_test)
}

avg_rsme_train <- total_rsme_train %>% summarize(avg_rsme_train=mean(rsme_train))
avg_rsme_test <- total_rsme_test %>% summarize(avg_rsme_test=mean(rsme_test))
se_rsme_train <- total_rsme_train %>% summarize(se_rsme_train=sd(rsme_train)/5)
se_rsme_test <- total_rsme_test %>% summarize(se_rsme_test=sd(rsme_test)/5)
results <- rbind(results, 
                 data.frame(
                   degree=1,
                   avg_rsme_test=avg_rsme_test[1],
                   avg_rsme_train=avg_rsme_train[1],
                   se_validate_err= se_rsme_train[1],
                   se_rsme_test=se_rsme_test[1]
                 ))
  ## Getting the Train Accuracy
  #tmp <- data.frame(pred=(predict(cvfit,newx=train,s="lambda.min", type="class")), real=trainy)
  #acc <- tmp %>% mutate(right=ifelse(X1==real,1,0)) %>% summarize(acc=sum(right)/nrow(tmp))
  
  # Build the DataFrame
  #degree_models[count,] <-c(k, summary(cvfit)$r.squared, as.numeric(rsme_train[1,1]), as.numeric(rsme_test[1,1]))

## Report the Results
print("Results for Indicator Variables")
results

total_results <- rbind(total_results, results)


## The Repositories and Gists
K <- 8
kfold_predictive <- predictive 
kfold_predictive <- kfold_predictive [sample(1:nrow(kfold_predictive)),]
kfold_predictive  <- kfold_predictive %>% mutate(fold=row_number() %% 5 + 1)
results <- data.frame()
for (k in 1:K) {
  pred_fixed <- kfold_predictive %>% mutate(good_coder=closed_merge_frac) %>% 
    select(-closed_merge_frac)
  

  
  pred_result <- kfold_predictive %>% select(userId)
  
  for (name in c("repos_scaled", "gists_scaled")) {
    if (identical(name, "good_coder") | identical(name, "userId")) {
      next
    }
    
    tmp <- compDegree(df=pred_fixed, n=k, var=name) %>% select(-good_coder, -fold, -has_bio, -has_location, -has_company,
                                                               -has_blog, -has_name, -hireable, -above_pop, -following_scaled, -fol_scaled,
                                                               -bio_scaled, -repos_scaled, -gists_scaled, -age_scaled, -email_domain, -bio_length)
    pred_result <- left_join(pred_result, tmp, by="userId")
  }
  
  pred_result <- left_join(pred_result, select(pred_fixed, userId, fold, good_coder))
  my_data <- pred_result %>% select(-userId)

  #data <- pred_fixed %>% select(above_pop, fol_scaled, bio_scaled, bio_length, hireable,
  #                              repos_scaled)
  
  total_rsme_train <- data.frame()
  total_rsme_test <- data.frame()
  avg_rsme_test <- c()
  avg_rsme_train <- c()
  
  for (i in 1:folds) {
    test <- my_data %>% filter(fold==i)
    train <- my_data %>% filter(fold!=i)
    model <- lm(good_coder ~ ., data = train)
    
    train <- train %>% add_predictions(model)
    test <- test %>% add_predictions(model)
    
    rsme_train <- train %>% summarize(rsme_train=sqrt(mean((pred - good_coder)^2)))
    rsme_test <- test %>% summarize(rsme_test=sqrt(mean((pred - good_coder)^2)))
    total_rsme_train <- rbind(total_rsme_train, rsme_train)
    total_rsme_test <- rbind(total_rsme_test, rsme_test)
  }
  
  avg_rsme_train <- total_rsme_train %>% summarize(avg_rsme_train=mean(rsme_train))
  avg_rsme_test <- total_rsme_test %>% summarize(avg_rsme_test=mean(rsme_test))
  se_rsme_train <- total_rsme_train %>% summarize(se_rsme_train=sd(rsme_train)/5)
  se_rsme_test <- total_rsme_test %>% summarize(se_rsme_test=sd(rsme_test)/5)

  results <- rbind(results, 
                   data.frame(
                     degree=k,
                     avg_rsme_test=avg_rsme_test[1],
                     avg_rsme_train=avg_rsme_train[1],
                     se_validate_err= se_rsme_train[1],
                     se_rsme_test=se_rsme_test[1]
                   ))
  ## Getting the Train Accuracy
  #tmp <- data.frame(pred=(predict(cvfit,newx=train,s="lambda.min", type="class")), real=trainy)
  #acc <- tmp %>% mutate(right=ifelse(X1==real,1,0)) %>% summarize(acc=sum(right)/nrow(tmp))
  
  # Build the DataFrame
  #degree_models[count,] <-c(k, summary(cvfit)$r.squared, as.numeric(rsme_train[1,1]), as.numeric(rsme_test[1,1]))
}

## Report the Results
results

total_results <- rbind(total_results, filter(results, avg_rsme_test==min(avg_rsme_test)))

ggplot(results,aes(x = degree, y = avg_rsme_test)) +
  geom_line(aes(y = avg_rsme_test, color="avg_rsme_test")) +
  geom_line(aes(y = avg_rsme_train, color="avg_rsme_train")) +
  geom_point(aes(color=ifelse(avg_rsme_test == min(avg_rsme_test), "best_degree", "avg_rsme_test"))) +
  scale_size_area(guide = F) +
  labs(title="Test and Train Average Error (Repos and Gists)") +
  ylab("Average Error") +
  xlab("Degree")

## Followers, Following, Profile Age

kfold_predictive <- predictive 
kfold_predictive <- kfold_predictive [sample(1:nrow(kfold_predictive)),]
kfold_predictive  <- kfold_predictive %>% mutate(fold=row_number() %% 5 + 1)
results <- data.frame()
for (k in 1:K) {
  pred_fixed <- kfold_predictive %>% mutate(good_coder=closed_merge_frac) %>% 
    select(-closed_merge_frac)
  

  
  pred_result <- kfold_predictive %>% select(userId)
  
  for (name in c("fol_scaled", "following_scaled", "age_scaled")) {
    if (identical(name, "good_coder") | identical(name, "userId")) {
      next
    }
    
    tmp <- compDegree(df=pred_fixed, n=k, var=name) %>% select(-good_coder, -fold, -has_bio, -has_location, -has_company,
                                                               -has_blog, -has_name, -hireable, -above_pop, -following_scaled, -fol_scaled,
                                                               -bio_scaled, -repos_scaled, -gists_scaled, -age_scaled, -email_domain, -bio_length)
    pred_result <- left_join(pred_result, tmp, by="userId")
  }
  
  pred_result <- left_join(pred_result, select(pred_fixed, userId, fold, good_coder))
  my_data <- pred_result %>% select(-userId)

  #data <- pred_fixed %>% select(above_pop, fol_scaled, bio_scaled, bio_length, hireable,
  #                              repos_scaled)
  
  total_rsme_train <- data.frame()
  total_rsme_test <- data.frame()
  avg_rsme_test <- c()
  avg_rsme_train <- c()
  
  for (i in 1:folds) {
    test <- my_data %>% filter(fold==i)
    train <- my_data %>% filter(fold!=i)
    model <- lm(good_coder ~ ., data = train)
    
    train <- train %>% add_predictions(model)
    test <- test %>% add_predictions(model)
    
    rsme_train <- train %>% summarize(rsme_train=sqrt(mean((pred - good_coder)^2)))
    rsme_test <- test %>% summarize(rsme_test=sqrt(mean((pred - good_coder)^2)))
    total_rsme_train <- rbind(total_rsme_train, rsme_train)
    total_rsme_test <- rbind(total_rsme_test, rsme_test)
  }
  
  avg_rsme_train <- total_rsme_train %>% summarize(avg_rsme_train=mean(rsme_train))
  avg_rsme_test <- total_rsme_test %>% summarize(avg_rsme_test=mean(rsme_test))
  se_rsme_train <- total_rsme_train %>% summarize(se_rsme_train=sd(rsme_train)/5)
  se_rsme_test <- total_rsme_test %>% summarize(se_rsme_test=sd(rsme_test)/5)
  results <- rbind(results, 
                   data.frame(
                     degree=k,
                     avg_rsme_test=avg_rsme_test[1],
                     avg_rsme_train=avg_rsme_train[1],
                     se_validate_err= se_rsme_train[1],
                     se_rsme_test=se_rsme_test[1]
                   ))
  ## Getting the Train Accuracy
  #tmp <- data.frame(pred=(predict(cvfit,newx=train,s="lambda.min", type="class")), real=trainy)
  #acc <- tmp %>% mutate(right=ifelse(X1==real,1,0)) %>% summarize(acc=sum(right)/nrow(tmp))
  
  # Build the DataFrame
  #degree_models[count,] <-c(k, summary(cvfit)$r.squared, as.numeric(rsme_train[1,1]), as.numeric(rsme_test[1,1]))
}

## Report the Results
results
total_results <- rbind(total_results, filter(results, avg_rsme_test==min(avg_rsme_test)))

ggplot(results,aes(x = degree, y = avg_rsme_test)) +
  geom_line(aes(y = avg_rsme_test, color="avg_rsme_test")) +
  geom_line(aes(y = avg_rsme_train, color="avg_rsme_train")) +
  geom_point(aes(color=ifelse(avg_rsme_test == min(avg_rsme_test), "best_degree", "avg_rsme_test"))) +
  scale_size_area(guide = F) +
  labs(title="Test and Train Average Error (Following/ers, Profile Age)") +
  ylab("Average Error") +
  xlab("Degree")


## The Repositories and Gists, and Disclosed

kfold_predictive <- predictive 
kfold_predictive <- kfold_predictive [sample(1:nrow(kfold_predictive)),]
kfold_predictive  <- kfold_predictive %>% mutate(fold=row_number() %% 5 + 1)
results <- data.frame()
for (k in 1:K) {
  pred_fixed <- kfold_predictive %>% mutate(good_coder=closed_merge_frac) %>% 
    select(-closed_merge_frac)
  

  
  pred_result <- kfold_predictive %>% select(userId)
  
  for (name in c("repos_scaled", "gists_scaled", "has_name", "has_bio", "has_blog", "has_location", "email_domain", "has_company", "hireable")) {
    if (identical(name, "good_coder") | identical(name, "userId")) {
      next
    }
    
    tmp <- compDegree(df=pred_fixed, n=k, var=name) %>% select(-good_coder, -fold, -has_bio, -has_location, -has_company,
                                                               -has_blog, -has_name, -hireable, -above_pop, -following_scaled, -fol_scaled,
                                                               -bio_scaled, -repos_scaled, -gists_scaled, -age_scaled, -email_domain, -bio_length)
    pred_result <- left_join(pred_result, tmp, by="userId")
  }
  
  pred_result <- left_join(pred_result, select(pred_fixed, userId, fold, good_coder))
  my_data <- pred_result %>% select(-userId)

  #data <- pred_fixed %>% select(above_pop, fol_scaled, bio_scaled, bio_length, hireable,
  #                              repos_scaled)
  
  total_rsme_train <- data.frame()
  total_rsme_test <- data.frame()
  avg_rsme_test <- c()
  avg_rsme_train <- c()
  
  for (i in 1:folds) {
    test <- my_data %>% filter(fold==i)
    train <- my_data %>% filter(fold!=i)
    model <- lm(good_coder ~ ., data = train)
    
    train <- train %>% add_predictions(model)
    test <- test %>% add_predictions(model)
    
    rsme_train <- train %>% summarize(rsme_train=sqrt(mean((pred - good_coder)^2)))
    rsme_test <- test %>% summarize(rsme_test=sqrt(mean((pred - good_coder)^2)))
    total_rsme_train <- rbind(total_rsme_train, rsme_train)
    total_rsme_test <- rbind(total_rsme_test, rsme_test)
  }
  
  avg_rsme_train <- total_rsme_train %>% summarize(avg_rsme_train=mean(rsme_train))
  avg_rsme_test <- total_rsme_test %>% summarize(avg_rsme_test=mean(rsme_test))
  se_rsme_train <- total_rsme_train %>% summarize(se_rsme_train=sd(rsme_train)/5)
  se_rsme_test <- total_rsme_test %>% summarize(se_rsme_test=sd(rsme_test)/5)
  results <- rbind(results, 
                   data.frame(
                     degree=k,
                     avg_rsme_test=avg_rsme_test[1],
                     avg_rsme_train=avg_rsme_train[1],
                     se_validate_err= se_rsme_train[1],
                     se_rsme_test=se_rsme_test[1]
                   ))
  ## Getting the Train Accuracy
  #tmp <- data.frame(pred=(predict(cvfit,newx=train,s="lambda.min", type="class")), real=trainy)
  #acc <- tmp %>% mutate(right=ifelse(X1==real,1,0)) %>% summarize(acc=sum(right)/nrow(tmp))
  
  # Build the DataFrame
  #degree_models[count,] <-c(k, summary(cvfit)$r.squared, as.numeric(rsme_train[1,1]), as.numeric(rsme_test[1,1]))
}

## Report the Results
results

total_results <- rbind(total_results, filter(results, avg_rsme_test==min(avg_rsme_test)))

ggplot(results,aes(x = degree, y = avg_rsme_test)) +
  geom_line(aes(y = avg_rsme_test, color="avg_rsme_test")) +
  geom_line(aes(y = avg_rsme_train, color="avg_rsme_train")) +
  geom_point(aes(color=ifelse(avg_rsme_test == min(avg_rsme_test), "best_degree", "avg_rsme_test"))) +
  scale_size_area(guide = F) +
  labs(title="Test and Train Average Error (Disclosed + Repos/Gists)") +
  ylab("Average Error") +
  xlab("Degree")


## The Disclosed and the Follower/ing and Age

kfold_predictive <- predictive 
kfold_predictive <- kfold_predictive [sample(1:nrow(kfold_predictive)),]
kfold_predictive  <- kfold_predictive %>% mutate(fold=row_number() %% 5 + 1)
results <- data.frame()
for (k in 1:K) {
  pred_fixed <- kfold_predictive %>% mutate(good_coder=closed_merge_frac) %>% 
    select(-closed_merge_frac)
  

  
  pred_result <- kfold_predictive %>% select(userId)
  
  for (name in c("fol_scaled", "following_scaled", "age_scaled", "has_name", "has_bio", "has_blog", "has_location", "email_domain", "has_company", "hireable")) {
    if (identical(name, "good_coder") | identical(name, "userId")) {
      next
    }
    
    tmp <- compDegree(df=pred_fixed, n=k, var=name) %>% select(-good_coder, -fold, -has_bio, -has_location, -has_company,
                                                               -has_blog, -has_name, -hireable, -above_pop, -following_scaled, -fol_scaled,
                                                               -bio_scaled, -repos_scaled, -gists_scaled, -age_scaled, -email_domain, -bio_length)
    pred_result <- left_join(pred_result, tmp, by="userId")
  }
  
  pred_result <- left_join(pred_result, select(pred_fixed, userId, fold, good_coder))
  my_data <- pred_result %>% select(-userId)

  #data <- pred_fixed %>% select(above_pop, fol_scaled, bio_scaled, bio_length, hireable,
  #                              repos_scaled)
  
  total_rsme_train <- data.frame()
  total_rsme_test <- data.frame()
  avg_rsme_test <- c()
  avg_rsme_train <- c()
  
  for (i in 1:folds) {
    test <- my_data %>% filter(fold==i)
    train <- my_data %>% filter(fold!=i)
    model <- lm(good_coder ~ ., data = train)
    
    train <- train %>% add_predictions(model)
    test <- test %>% add_predictions(model)
    
    rsme_train <- train %>% summarize(rsme_train=sqrt(mean((pred - good_coder)^2)))
    rsme_test <- test %>% summarize(rsme_test=sqrt(mean((pred - good_coder)^2)))
    total_rsme_train <- rbind(total_rsme_train, rsme_train)
    total_rsme_test <- rbind(total_rsme_test, rsme_test)
  }
  
  avg_rsme_train <- total_rsme_train %>% summarize(avg_rsme_train=mean(rsme_train))
  avg_rsme_test <- total_rsme_test %>% summarize(avg_rsme_test=mean(rsme_test))
  se_rsme_train <- total_rsme_train %>% summarize(se_rsme_train=sd(rsme_train)/5)
  se_rsme_test <- total_rsme_test %>% summarize(se_rsme_test=sd(rsme_test)/5)
  results <- rbind(results, 
                   data.frame(
                     degree=k,
                     avg_rsme_test=avg_rsme_test[1],
                     avg_rsme_train=avg_rsme_train[1],
                     se_validate_err= se_rsme_train[1],
                     se_rsme_test=se_rsme_test[1]
                   ))
  ## Getting the Train Accuracy
  #tmp <- data.frame(pred=(predict(cvfit,newx=train,s="lambda.min", type="class")), real=trainy)
  #acc <- tmp %>% mutate(right=ifelse(X1==real,1,0)) %>% summarize(acc=sum(right)/nrow(tmp))
  
  # Build the DataFrame
  #degree_models[count,] <-c(k, summary(cvfit)$r.squared, as.numeric(rsme_train[1,1]), as.numeric(rsme_test[1,1]))
}

## Report the Results
results

total_results <- rbind(total_results, filter(results, avg_rsme_test==min(avg_rsme_test)))

ggplot(results,aes(x = degree, y = avg_rsme_test)) +
  geom_line(aes(y = avg_rsme_test, color="avg_rsme_test")) +
  geom_line(aes(y = avg_rsme_train, color="avg_rsme_train")) +
  geom_point(aes(color=ifelse(avg_rsme_test == min(avg_rsme_test), "best_degree", "avg_rsme_test"))) +
  scale_size_area(guide = F) +
  labs(title="Test and Train Average Error (Disclosed + Followers/ing)") +
  ylab("Average Error") +
  xlab("Degree")


## Followers, Following, Profile Age, and Repos/Gists

kfold_predictive <- predictive 
kfold_predictive <- kfold_predictive [sample(1:nrow(kfold_predictive)),]
kfold_predictive  <- kfold_predictive %>% mutate(fold=row_number() %% 5 + 1)
results <- data.frame()
for (k in 1:K) {
  pred_fixed <- kfold_predictive %>% mutate(good_coder=closed_merge_frac) %>% 
    select(-closed_merge_frac)
  

  
  pred_result <- kfold_predictive %>% select(userId)
  
  for (name in c("repos_scaled", "gists_scaled", "fol_scaled", "following_scaled", "age_scaled")) {
    if (identical(name, "good_coder") | identical(name, "userId")) {
      next
    }
    
    tmp <- compDegree(df=pred_fixed, n=k, var=name) %>% select(-good_coder, -fold, -has_bio, -has_location, -has_company,
                                                               -has_blog, -has_name, -hireable, -above_pop, -following_scaled, -fol_scaled,
                                                               -bio_scaled, -repos_scaled, -gists_scaled, -age_scaled, -email_domain, -bio_length)
    pred_result <- left_join(pred_result, tmp, by="userId")
  }
  
  pred_result <- left_join(pred_result, select(pred_fixed, userId, fold, good_coder))
  my_data <- pred_result %>% select(-userId)

  #data <- pred_fixed %>% select(above_pop, fol_scaled, bio_scaled, bio_length, hireable,
  #                              repos_scaled)
  
  total_rsme_train <- data.frame()
  total_rsme_test <- data.frame()
  avg_rsme_test <- c()
  avg_rsme_train <- c()
  
  for (i in 1:folds) {
    test <- my_data %>% filter(fold==i)
    train <- my_data %>% filter(fold!=i)
    model <- lm(good_coder ~ ., data = train)
    
    train <- train %>% add_predictions(model)
    test <- test %>% add_predictions(model)
    
    rsme_train <- train %>% summarize(rsme_train=sqrt(mean((pred - good_coder)^2)))
    rsme_test <- test %>% summarize(rsme_test=sqrt(mean((pred - good_coder)^2)))
    total_rsme_train <- rbind(total_rsme_train, rsme_train)
    total_rsme_test <- rbind(total_rsme_test, rsme_test)
  }
  
  avg_rsme_train <- total_rsme_train %>% summarize(avg_rsme_train=mean(rsme_train))
  avg_rsme_test <- total_rsme_test %>% summarize(avg_rsme_test=mean(rsme_test))
  se_rsme_train <- total_rsme_train %>% summarize(se_rsme_train=sd(rsme_train)/5)
  se_rsme_test <- total_rsme_test %>% summarize(se_rsme_test=sd(rsme_test)/5)
  results <- rbind(results, 
                   data.frame(
                     degree=k,
                     avg_rsme_test=avg_rsme_test[1],
                     avg_rsme_train=avg_rsme_train[1],
                     se_validate_err= se_rsme_train[1],
                     se_rsme_test=se_rsme_test[1]
                   ))
  ## Getting the Train Accuracy
  #tmp <- data.frame(pred=(predict(cvfit,newx=train,s="lambda.min", type="class")), real=trainy)
  #acc <- tmp %>% mutate(right=ifelse(X1==real,1,0)) %>% summarize(acc=sum(right)/nrow(tmp))
  
  # Build the DataFrame
  #degree_models[count,] <-c(k, summary(cvfit)$r.squared, as.numeric(rsme_train[1,1]), as.numeric(rsme_test[1,1]))
}

## Report the Results
results
total_results <- rbind(total_results, filter(results, avg_rsme_test==min(avg_rsme_test)))

ggplot(results,aes(x = degree, y = avg_rsme_test)) +
  geom_line(aes(y = avg_rsme_test, color="avg_rsme_test")) +
  geom_line(aes(y = avg_rsme_train, color="avg_rsme_train")) +
  geom_point(aes(color=ifelse(avg_rsme_test == min(avg_rsme_test), "best_degree", "avg_rsme_test"))) +
  scale_size_area(guide = F) +
  labs(title="Test and Train Average Error (Following/ers, Profile Age + Repos/Gists)") +
  ylab("Average Error") +
  xlab("Degree")


## All Features

kfold_predictive <- predictive 
kfold_predictive <- kfold_predictive [sample(1:nrow(kfold_predictive)),]
kfold_predictive  <- kfold_predictive %>% mutate(fold=row_number() %% 5 + 1)
results <- data.frame()
for (k in 1:K) {
  pred_fixed <- kfold_predictive %>% mutate(good_coder=closed_merge_frac) %>% 
    select(-closed_merge_frac)
  

  
  pred_result <- kfold_predictive %>% select(userId)
  
  for (name in c("has_name", "has_bio", "has_blog", "has_location", "email_domain", "has_company", "hireable", "repos_scaled", "gists_scaled", "fol_scaled", "following_scaled", "age_scaled")) {
    if (identical(name, "good_coder") | identical(name, "userId")) {
      next
    }
    
    tmp <- compDegree(df=pred_fixed, n=k, var=name) %>% select(-good_coder, -fold, -has_bio, -has_location, -has_company,
                                                               -has_blog, -has_name, -hireable, -above_pop, -following_scaled, -fol_scaled,
                                                               -bio_scaled, -repos_scaled, -gists_scaled, -age_scaled, -email_domain, -bio_length)
    pred_result <- left_join(pred_result, tmp, by="userId")
  }
  
  pred_result <- left_join(pred_result, select(pred_fixed, userId, fold, good_coder))
  my_data <- pred_result %>% select(-userId)

  #data <- pred_fixed %>% select(above_pop, fol_scaled, bio_scaled, bio_length, hireable,
  #                              repos_scaled)
  
  total_rsme_train <- data.frame()
  total_rsme_test <- data.frame()
  avg_rsme_test <- c()
  avg_rsme_train <- c()
  
  for (i in 1:folds) {
    test <- my_data %>% filter(fold==i)
    train <- my_data %>% filter(fold!=i)
    model <- lm(good_coder ~ ., data = train)
    
    train <- train %>% add_predictions(model)
    test <- test %>% add_predictions(model)
    
    rsme_train <- train %>% summarize(rsme_train=sqrt(mean((pred - good_coder)^2)))
    rsme_test <- test %>% summarize(rsme_test=sqrt(mean((pred - good_coder)^2)))
    total_rsme_train <- rbind(total_rsme_train, rsme_train)
    total_rsme_test <- rbind(total_rsme_test, rsme_test)
  }
  
  avg_rsme_train <- total_rsme_train %>% summarize(avg_rsme_train=mean(rsme_train))
  avg_rsme_test <- total_rsme_test %>% summarize(avg_rsme_test=mean(rsme_test))
  se_rsme_train <- total_rsme_train %>% summarize(se_rsme_train=sd(rsme_train)/5)
  se_rsme_test <- total_rsme_test %>% summarize(se_rsme_test=sd(rsme_test)/5)
  results <- rbind(results, 
                   data.frame(
                     degree=k,
                     avg_rsme_test=avg_rsme_test[1],
                     avg_rsme_train=avg_rsme_train[1],
                     se_validate_err= se_rsme_train[1],
                     se_rsme_test=se_rsme_test[1]
                   ))
  ## Getting the Train Accuracy
  #tmp <- data.frame(pred=(predict(cvfit,newx=train,s="lambda.min", type="class")), real=trainy)
  #acc <- tmp %>% mutate(right=ifelse(X1==real,1,0)) %>% summarize(acc=sum(right)/nrow(tmp))
  
  # Build the DataFrame
  #degree_models[count,] <-c(k, summary(cvfit)$r.squared, as.numeric(rsme_train[1,1]), as.numeric(rsme_test[1,1]))
}

## Report the Results
results
total_results <- rbind(total_results, filter(results, avg_rsme_test==min(avg_rsme_test)))

ggplot(results,aes(x = degree, y = avg_rsme_test)) +
  geom_line(aes(y = avg_rsme_test, color="avg_rsme_test")) +
  geom_line(aes(y = avg_rsme_train, color="avg_rsme_train")) +
  geom_point(aes(color=ifelse(avg_rsme_test == min(avg_rsme_test), "best_degree", "avg_rsme_test"))) +
  scale_size_area(guide = F) +
  labs(title="Test and Train Average Error (All)") +
  ylab("Average Error") +
  xlab("Degree")

## Reporting the Total Results of the Model:
total_results

## Getting the Coefficients of All Degree-1 Model

pred_fixed <- predictive %>% mutate(good_coder=closed_merge_frac) %>% 
  select(-closed_merge_frac)

my_data <- pred_fixed %>% select(good_coder, has_name, has_bio, has_blog, has_location, 
                                 email_domain, has_company, hireable, repos_scaled,
                                 gists_scaled, fol_scaled, following_scaled, age_scaled)

model <- lm(good_coder ~ ., data = my_data)
summary(model)


## Holdout Set Evaluation
holdout_results <- data.frame()

## All the Different Models

## Indicators
my_data <- pred_fixed %>% select(good_coder, has_name, has_bio, has_blog, has_location, 
                                 email_domain, has_company, hireable)

model_indicators <- lm(good_coder ~ ., data = my_data)

holdout <- holdout %>%  mutate(good_coder=closed_merge_frac) %>% 
    select(-closed_merge_frac)
holdout_cpy <- holdout
holdout_cpy <- holdout_cpy %>% add_predictions(model_indicators)
    
rsme_holdout <- holdout_cpy %>% summarize(rsme_train=sqrt(mean((pred - good_coder)^2)))
holdout_results <- rbind(holdout_results, data.frame(rsme=rsme_holdout[1]))

## Repos
my_data <- pred_fixed %>% select(repos_scaled, gists_scaled)

model_repos <- lm(good_coder ~ ., data = my_data)

holdout_cpy <- holdout
holdout_cpy <- holdout_cpy %>% add_predictions(model_repos)
    
rsme_holdout <- holdout_cpy %>% summarize(rsme_train=sqrt(mean((pred - good_coder)^2)))
holdout_results <- rbind(holdout_results, data.frame(rsme=rsme_holdout[1]))

## Followers/ings
my_data <- pred_fixed %>% select(fol_scaled, following_scaled, age_scaled)

model_fol <- lm(good_coder ~ ., data = my_data)

holdout_cpy <- holdout
holdout_cpy <- holdout_cpy %>% add_predictions(model_fol)
    
rsme_holdout <- holdout_cpy %>% summarize(rsme_train=sqrt(mean((pred - good_coder)^2)))
holdout_results <- rbind(holdout_results, data.frame(rsme=rsme_holdout[1]))

## Indicators + repos
my_data <- pred_fixed %>% select(good_coder, has_name, has_bio, has_blog, has_location, 
                                 email_domain, has_company, hireable, repos_scaled, gists_scaled)

model_indi_repo <- lm(good_coder ~ ., data = my_data)

holdout_cpy <- holdout
holdout_cpy <- holdout_cpy %>% add_predictions(model_indi_repo)
    
rsme_holdout <- holdout_cpy %>% summarize(rsme_train=sqrt(mean((pred - good_coder)^2)))
holdout_results <- rbind(holdout_results, data.frame(rsme=rsme_holdout[1]))

## Indicators + Followers/ings
my_data <- pred_fixed %>% select(good_coder, has_name, has_bio, has_blog, has_location, 
                                 email_domain, has_company, hireable, fol_scaled, following_scaled, age_scaled)

model_indi_fol <- lm(good_coder ~ ., data = my_data)

holdout_cpy <- holdout
holdout_cpy <- holdout_cpy %>% add_predictions(model_indi_fol)
    
rsme_holdout <- holdout_cpy %>% summarize(rsme_train=sqrt(mean((pred - good_coder)^2)))
holdout_results <- rbind(holdout_results, data.frame(rsme=rsme_holdout[1]))

## Repos + Followers/ings
my_data <- pred_fixed %>% select(repos_scaled, gists_scaled, fol_scaled, following_scaled, age_scaled)

model_repos_fol <- lm(good_coder ~ ., data = my_data)

holdout_cpy <- holdout
holdout_cpy <- holdout_cpy %>% add_predictions(model_repos_fol)
    
rsme_holdout <- holdout_cpy %>% summarize(rsme_train=sqrt(mean((pred - good_coder)^2)))
holdout_results <- rbind(holdout_results, data.frame(rsme=rsme_holdout[1]))


## All
my_data <- pred_fixed %>% select(good_coder, has_name, has_bio, has_blog, has_location, 
                                 email_domain, has_company, hireable, repos_scaled,
                                 gists_scaled, fol_scaled, following_scaled, age_scaled)

model_all <- lm(good_coder ~ ., data = my_data)

holdout_cpy <- holdout
holdout_cpy <- holdout_cpy %>% add_predictions(model_all)
    
rsme_holdout <- holdout_cpy %>% summarize(rsme_train=sqrt(mean((pred - good_coder)^2)))
holdout_results <- rbind(holdout_results, data.frame(rsme=rsme_holdout[1]))

holdout_results


#### Let's Try doing Linear Regression with Interaction Terms
compInteraction <- function(df) {
  interactions <- combn(names(df), 2)
  
  for (i in 1:ncol(interactions)){
    varname <- paste(interactions[1, i], interactions[2, i], sep=".")
    df[[varname]] <- with(df, df[[interactions[1,i]]] * df[[interactions[2,i]]])
  }
  df
}

kfold_predictive <- predictive 
kfold_predictive <- kfold_predictive [sample(1:nrow(kfold_predictive)),]
kfold_predictive  <- kfold_predictive %>% mutate(fold=row_number() %% 5 + 1)

pred_fixed <- kfold_predictive %>% mutate(good_coder=closed_merge_frac) %>% 
  select(-closed_merge_frac)

interaction_terms <- compInteraction(pred_fixed)

interaction_terms <- left_join(interaction_terms, select(pred_fixed, userId, fold, good_coder))
my_data <- interaction_terms %>% select(-userId)

#data <- pred_fixed %>% select(above_pop, fol_scaled, bio_scaled, bio_length, hireable,
#                              repos_scaled)

# 90% Train-Test Split
total_rsme_train <- data.frame()
total_rsme_test <- data.frame()
avg_rsme_test <- c()
avg_rsme_train <- c()

for (i in 1:folds) {
  test <- my_data %>% filter(fold==i)
  train <- my_data %>% filter(fold!=i)
  model <- lm(good_coder ~ ., data = train)
  
  train <- train %>% add_predictions(model)
  test <- test %>% add_predictions(model)
  
  rsme_train <- train %>% summarize(rsme_train=sqrt(mean((pred - good_coder)^2)))
  rsme_test <- test %>% summarize(rsme_test=sqrt(mean((pred - good_coder)^2)))
  total_rsme_train <- rbind(total_rsme_train, rsme_train)
  total_rsme_test <- rbind(total_rsme_test, rsme_test)
}

avg_rsme_train <- total_rsme_train %>% summarize(avg_rsme_train=mean(rsme_train))
avg_rsme_test <- total_rsme_test %>% summarize(avg_rsme_test=mean(rsme_test))
se_rsme_train <- total_rsme_train %>% summarize(se_rsme_train=sd(rsme_train)/5)
se_rsme_test <- total_rsme_test %>% summarize(se_rsme_test=sd(rsme_test)/5)
avg_rsme_train
avg_rsme_test
se_rsme_train
se_rsme_test

## Haven't had any luck with linear, let's try Random Forests?

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
tmp <- data.frame(pred=(predict(forrest,newx=train)), real=trainy)
acc <- tmp %>% mutate(right=ifelse(pred==real,1,0)) %>% summarize(acc=sum(right)/nrow(tmp))
acc ## Accuracy

### Doing the Top Quartile and the Bottom Quartile

pred_fixed <- predictive %>% filter(closed_merge_frac>quantile(closed_merge_frac,0.75) | closed_merge_frac<quantile(closed_merge_frac,0.25)) %>% 
  mutate(good_coder=ifelse(closed_merge_frac>quantile(closed_merge_frac, 0.75), 1, 0)) %>% 
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

# Train Accuracy
tmp <- data.frame(pred=(predict(forrest,newx=train)), real=trainy)
acc <- tmp %>% mutate(right=ifelse(pred==real,1,0)) %>% summarize(acc=sum(right)/nrow(tmp))
acc ## Here' the Accuracy
