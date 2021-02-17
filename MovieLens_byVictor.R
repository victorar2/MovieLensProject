
#======Install of all necesary library========================================
if(!require(dplyr)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("data.table", repos = "http://cran.us.r-project.org")

#------Calling of library-----------------------------------------------------
library(dplyr, warn.conflicts = FALSE)
library(tidyverse)
library(caret)
library(stringr)
library(data.table)
library(ggplot2)


#======the Following code belong to course excercise download code
#------data of movielens , this part could take several minutes
#------
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#----------------Split data on train and test----------

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.15, list = FALSE)
edx_train <- edx[-test_index,]
temp1 <- edx[test_index,]
# Make sure userId and movieId in edx_train set are also in edx_test set
edx_test <- temp1 %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")
# Add rows removed from edx_test set back into edx_train
removed <- anti_join(temp1, edx_test)
edx_train <- rbind(edx_train, removed)

#------RMSE function-------------
RMSE<-function(predicted,real){
  sqrt(mean((predicted-real)^2))}  #function to predict RMSE

#-----starting with average----
u<- mean(edx_train$rating)
edx_test%>%ggplot(aes(x=rating))+geom_bar(aes(y=..prop..,group=1))+
  geom_vline(xintercept =3.512)+scale_y_continuous(labels = scales::percent_format())
edx_test<-edx_test%>%mutate(p1=mean(edx_train$rating))
edx_train<-edx_train%>%mutate(p1=mean(edx_train$rating))

#-----Adding movie effect---------
m<-c(1:10) # Vector of m values to test
# Function to calculate RMSE with each new m value
best_m<- function(train,test,m){
  train<-train%>%mutate(u=mean(train$rating))
  test<-test%>%mutate(u=mean(train$rating))
  data1<-suppressWarnings(train%>%mutate(b1=rating-u)%>%group_by(movieId)%>%summarize(movie_b=sum(b1)/(n()+m)))
  test<-test%>%left_join(data1,by="movieId")
  test<-test%>%mutate(pred=u+movie_b)
  RMSE(test$rating,test$pred)
}
m_graphic<- sapply(m,best_m,train=edx_train,test=edx_test)
data_movie <-edx_train%>%mutate(b1=rating-u)%>%group_by(movieId)%>%summarise(movie_b=sum(b1)/(n()+2))
edx_train<-edx_train%>%left_join(data_movie,by="movieId")#adding movie effect on edx_train
edx_test<-edx_test%>%left_join(data_movie,by="movieId")#adding movie effect on edx_test
edx_test<-edx_test%>%mutate(p2=u+movie_b)

#---Adding User effect------------
m<-c(1:10) # Vector of m values to test
# Function to calculate RMSE with each new m value
best_m2<- function(train,test,m){
  data1<-suppressWarnings(train%>%mutate(b2=rating-(u+movie_b))%>%group_by(userId)%>%summarize(user_b=sum(b2)/(n()+m)))
  test1<-test%>%left_join(data1,by="userId")
  test1<-test1%>%mutate(pred=u+movie_b+user_b)
  RMSE(test$rating,test1$pred)
}
m_graphic<- sapply(m,best_m2,train=edx_train,test=edx_test)
data_user<-edx_train%>%mutate(b2=rating-(u+movie_b))%>%group_by(userId)%>%summarize(user_b=sum(b2)/(n()+5))
edx_train<-edx_train%>%left_join(data_user,by="userId")
edx_test<-edx_test%>%left_join(data_user,by="userId")
edx_test<-edx_test%>%mutate(pred=u+movie_b+user_b)

#---Adding Genre effect------------
edx_train<-edx_train%>%
  mutate(Drama=ifelse(str_detect(genres,"Drama"),str_c(userId,"-Drama"),"x"))%>%
  mutate(Comedy=ifelse(str_detect(genres,"Comedy"),str_c(userId,"-Comedy"),"x"))%>%
  mutate(Action=ifelse(str_detect(genres,"Action"),str_c(userId,"-Action"),"x"))%>%
  mutate(Thriller=ifelse(str_detect(genres,"Thriller"),str_c(userId,"-Thriller"),"x"))
edx_train<-edx_train%>% mutate(pred=u+movie_b+user_b)%>%mutate(err=rating-pred)  #this error will be grouped by user and genre
data_drama<-edx_train%>% group_by(Drama)%>%summarise(Drama_b=mean(err))%>%mutate(Drama_b=ifelse(Drama=="x",0,Drama_b))
data_comedy<-edx_train%>% group_by(Comedy)%>%summarise(Comedy_b=mean(err))%>%mutate(Comedy_b=ifelse(Comedy=="x",0,Comedy_b))
data_action<-edx_train%>% group_by(Action)%>%summarise(Action_b=mean(err))%>%mutate(Action_b=ifelse(Action=="x",0,Action_b))
data_thriller<-edx_train%>% group_by(Thriller)%>%summarise(Thriller_b=mean(err)) %>%mutate(Thriller_b=ifelse(Thriller=="x",0,Thriller_b))

validation1<-validation%>%mutate(u=u)%>%left_join(data_movie,by="movieId")%>%left_join(data_user,by="userId")%>%mutate(Drama=ifelse(str_detect(genres,"Drama"),str_c(userId,"-Drama"),"x"))%>% mutate(Comedy=ifelse(str_detect(genres,"Comedy"),str_c(userId,"-Comedy"),"x"))%>% mutate(Action=ifelse(str_detect(genres,"Action"),str_c(userId,"-Action"),"x"))%>% mutate(Thriller=ifelse(str_detect(genres,"Thriller"),str_c(userId,"-Thriller"),"x"))%>%left_join(data_drama,by="Drama")%>% left_join(data_comedy,by="Comedy")%>% left_join(data_action,by="Action")%>% left_join(data_thriller,by="Thriller")%>%replace_na(list(Drama_b=0,Comedy_b=0,Action_b=0,Thriller_b=0)) %>%mutate(pred=u+movie_b+user_b+Drama_b+Comedy_b+Action_b+Thriller_b)
RMSE(validation1$pred,validation1$rating)
