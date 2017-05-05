### Generates the Graphs Seen in the Exploratory Data Analysis Section

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
                                      email_domain=ifelse(grepl("^.*@gmail.com", email), 0, 1),
                                      good_coder=ifelse(closed_merge_frac>quantile(closed_merge_frac, 0.25), 1, 0)) %>%
  select(userId, has_bio, has_location, has_company, has_blog, has_name, following_scaled, repos_scaled, gists_scaled,
         above_pop, fol_scaled, bio_length, bio_scaled, hireable, age_scaled, email_domain, closed_merge_frac)


