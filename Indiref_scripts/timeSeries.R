#plot the time series
#get data from 'cleanTweets.R'

head(data)

#create date field
data_ <- data %>%
	dplyr::mutate(date=substr(created_at, 1, 10))

head(data_)

#get organic tweets
englandOrg <- data_ %>% 
  dplyr::filter(class=="England") %>%
  dplyr::select(user_id, status_id, created_at, text, is_retweet, reply_to_status_id, date) %>%
  dplyr::filter(is_retweet==FALSE) %>%
  dplyr::filter(is.na(reply_to_status_id))%>%
  dplyr::group_by(date) %>%
  dplyr::summarise(organic=n())%>%
  dplyr::summarise(organic=sum(organic))

#get retweet 
englandRetw <- data_ %>% 
  dplyr::filter(class=="England") %>%
  dplyr::select(user_id, status_id, created_at, text, is_retweet, reply_to_status_id, date) %>%
  dplyr::filter(is_retweet==TRUE) %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(retweet=n())%>%
  dplyr::summarise(retweet=sum(retweet))

#get reply
englandRepl <- data_ %>% 
  dplyr::filter(class=="England") %>%
  dplyr::select(user_id, status_id, created_at, text, is_retweet, reply_to_status_id, date) %>%
  dplyr::filter(is.na(reply_to_status_id))%>%
  dplyr::group_by(date) %>%
  dplyr::summarise(reply=n())%>%
  dplyr::summarise(reply=sum(reply))


#------------------------------------------------

#get organic tweets
walesOrg <- data_ %>% 
  dplyr::filter(class=="Wales") %>%
  dplyr::select(user_id, status_id, created_at, text, is_retweet, reply_to_status_id, date) %>%
  dplyr::filter(is_retweet==FALSE) %>%
  dplyr::filter(is.na(reply_to_status_id))%>%
  dplyr::group_by(date) %>%
  dplyr::summarise(organic=n())%>%
  dplyr::summarise(organic=sum(organic))

#get retweet 
walesRetw <- data_ %>% 
  dplyr::filter(class=="Wales") %>%
  dplyr::select(user_id, status_id, created_at, text, is_retweet, reply_to_status_id, date) %>%
  dplyr::filter(is_retweet==TRUE) %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(retweet=n())%>%
  dplyr::summarise(retweet=sum(retweet))

#get reply
walesRepl <- data_ %>% 
  dplyr::filter(class=="Wales") %>%
  dplyr::select(user_id, status_id, created_at, text, is_retweet, reply_to_status_id, date) %>%
  dplyr::filter(is.na(reply_to_status_id))%>%
  dplyr::group_by(date) %>%
  dplyr::summarise(reply=n())%>%
  dplyr::summarise(reply=sum(reply))

#------------------------------------------------

#get organic tweets
nirOrg <- data_ %>% 
  dplyr::filter(class=="Northern Ireland") %>%
  dplyr::select(user_id, status_id, created_at, text, is_retweet, reply_to_status_id, date) %>%
  dplyr::filter(is_retweet==FALSE) %>%
  dplyr::filter(is.na(reply_to_status_id))%>%
  dplyr::group_by(date) %>%
  dplyr::summarise(organic=n())%>%
  dplyr::summarise(organic=sum(organic))

#get retweet 
nirRetw <- data_ %>% 
  dplyr::filter(class=="Northern Ireland") %>%
  dplyr::select(user_id, status_id, created_at, text, is_retweet, reply_to_status_id, date) %>%
  dplyr::filter(is_retweet==TRUE) %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(retweet=n())%>%
  dplyr::summarise(retweet=sum(retweet))

#get reply
nirRepl <- data_ %>% 
  dplyr::filter(class=="Northern Ireland") %>%
  dplyr::select(user_id, status_id, created_at, text, is_retweet, reply_to_status_id, date) %>%
  dplyr::filter(is.na(reply_to_status_id))%>%
  dplyr::group_by(date) %>%
  dplyr::summarise(reply=n())%>%
  dplyr::summarise(reply=sum(reply))


#------------------------------------------------

#get organic tweets
scotlandOrg <- data_ %>% 
  dplyr::filter(class=="Scotland") %>%
  dplyr::select(user_id, status_id, created_at, text, is_retweet, reply_to_status_id, date) %>%
  dplyr::filter(is_retweet==FALSE) %>%
  dplyr::filter(is.na(reply_to_status_id))%>%
  dplyr::group_by(date) %>%
  dplyr::summarise(organic=n())%>%
  dplyr::summarise(organic=sum(organic))

#get retweet 
scotlandRetw <- data_ %>% 
  dplyr::filter(class=="Scotland") %>%
  dplyr::select(user_id, status_id, created_at, text, is_retweet, reply_to_status_id, date) %>%
  dplyr::filter(is_retweet==TRUE) %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(retweet=n())%>%
  dplyr::summarise(retweet=sum(retweet))

#get reply
scotlandRepl <- data_ %>% 
  dplyr::filter(class=="Scotland") %>%
  dplyr::select(user_id, status_id, created_at, text, is_retweet, reply_to_status_id, date) %>%
  dplyr::filter(is.na(reply_to_status_id))%>%
  dplyr::group_by(date) %>%
  dplyr::summarise(reply=n())%>%
  dplyr::summarise(reply=sum(reply))

#combine all

englandOrg
englandRepl
englandRetw

walesOrg
walesRepl
walesRetw


nirOrg
nirRepl
nirRetw

scotlandOrg
scotlandRepl
scotlandRetw


#head(englandTwt)

 %>%
  arrange(user_id, created_at) %>%
  mutate(day=subtr(created_at, 1, 10))

  group_by(user_id, created_at) %>%


#get count
data_ <- data_ %>% 
	group_by(class, date)%>%
	dplyr::summarise(count=n())

data_ <- data.frame(data_)
head(data_)

library(reshape)  
library(ggplot2)          # install 'reshape' if you don't have it
Time = data_$date
ggplot(data=data_, aes(x=Time, y=count, group=class))   +
      geom_line()                                    +
      guides(colour=FALSE)                            +
      facet_wrap(~class, ncol=2, scales='free_y')  +
      ylab('')	
	  
dev.off()	

#get tweet, retweet, and  replies