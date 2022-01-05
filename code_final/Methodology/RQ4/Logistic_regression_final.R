########################################################################
# Logistic Regression models RQ4
########################################################################

library(readr)
library(stargazer)
library(car)
library(lmtest)
library(sandwich)
library(ggplot2)

norm <- function(vec){
  maxx = max(vec)
  minn = min(vec)
  norm_vec = c()
  for(i in 1:length(vec)){
    val = vec[i]
    val_norm = (val-minn)/(maxx-minn)
    norm_vec = c(norm_vec,val_norm)
  }
  return(norm_vec)
} # normalizing for improved data visualization 
########################################################################
# Data
########################################################################
company_data = read_csv('company_data_regression.csv')
company_data = company_data[!(company_data$abi %in%  c(712510839,401877377)),] # exclude Facebook and Twitter


degree = read_csv('degree.csv')
close = read_csv('closeness.csv')
betw = read_csv('betweenness.csv')
geo = read_csv('mean_geographical_proximity.csv')
cog = read_csv('cognitive_proximity_numeric.csv')

########################################################################
# Degree
########################################################################
## Question: Does sales empl, year have significant influence on the probability of a company to have 
## equaly high values of degree in both graphs?

company_data$y1 = norm(log(degree$deg_h_out+1))
company_data$y2 = norm(log(degree$deg_f_out+1))
company_data$category = 0
sliding_window = 0.2
start = 0.2
counter=0
for(i in 1:nrow(company_data)){
  if ( company_data[i,9]>start && company_data[i,10]>start){
    if( company_data[i,9] <= (company_data[i,10]+sliding_window) && company_data[i,9] >=( company_data[i,10]-sliding_window)){
      company_data$category[i] = 1
      counter= counter+1
    }
  }
}
counter
ggplot( aes(y1,y2, color=factor(category)),data=company_data)+geom_point()+
  scale_color_manual(values=c("blue", "red"))+ xlab("Hyperlink Network")+ylab("Follower Network")


logit_m1 = glm(category~year+log(sales+1)+log(empl+1), family = 'binomial', data = company_data)
stargazer(logit_m1, type='text')
vif(logit_m1)
coeftest(logit_m1, vcov=vcovHC(logit_m1))


########################################################################
# Closeness
########################################################################
## Question: Does sales empl, year have significant influence on the probability of a company to have 
## equaly high values of closeness in both graphs?
sliding_window = 0.3
start = 0.01


company_data$y1 = norm(log(close$hyp_out_close+1))
company_data$y2 = norm(log(close$foll_out_close+1))
company_data$category = 0

counter=0
for(i in 1:nrow(company_data)){
  if ( company_data[i,9]>start && company_data[i,10]>start){
    if( company_data[i,9] <= (company_data[i,10]+sliding_window) && company_data[i,9] >=( company_data[i,10]-sliding_window)){
      company_data$category[i] = 1
      counter= counter+1
    }
  }
}
counter
ggplot( aes(y1,y2, color=factor(category)),data=company_data)+geom_point()+
  scale_color_manual(values=c("blue", "red"))+ xlab("Hyperlink Network")+ylab("Follower Network")


logit_m1 = glm(category~year+log(sales+1)+log(empl+1), family = 'binomial', data = company_data)
stargazer(logit_m1, type='text')
vif(logit_m1)
coeftest(logit_m1, vcov=vcovHC(logit_m1))


########################################################################
# Betweenness
########################################################################
## Question: Does sales empl, year have significant influence on the probability of a company to have 
## equaly high values of betweenness in both graphs?

sliding_window = 0.3
start = 0.01

company_data$y1 = norm(log(betw$betw_hyp+1))
company_data$y2 = norm(log(betw$betw_foll+1))
company_data$category = 0

counter=0
for(i in 1:nrow(company_data)){
  if ( company_data[i,9]>start && company_data[i,10]>start){
    if( company_data[i,9] <= (company_data[i,10]+sliding_window) && company_data[i,9] >=( company_data[i,10]-sliding_window)){
      company_data$category[i] = 1
      counter= counter+1
    }
  }
}
counter
ggplot( aes(y1,y2, color=factor(category)),data=company_data)+geom_point()+
  scale_color_manual(values=c("blue", "red"))+ xlab("Hyperlink Network")+ylab("Follower Network")


logit_m1 = glm(category~year+log(sales+1)+log(empl+1), family = 'binomial', data = company_data)
stargazer(logit_m1, type='text')
vif(logit_m1)
coeftest(logit_m1, vcov=vcovHC(logit_m1))

########################################################################
# Geographical Proximity
########################################################################
## Question: Does sales empl, year have significant influence on the probability of a company to have 
## equaly high values of geographical proximity in both graphs?

sliding_window = 0.2
start = 0.2

company_data[company_data$company_names != geo$nodes,]
target <- company_data$company_names
df = geo
geo1= df[match(target, df$nodes),]
company_data[company_data$company_names != geo1$nodes,]

company_data$y1 = norm(log(geo1$mean_geographical_proximity_hyp+1))
company_data$y2 = norm(log(geo1$mean_geographical_proximity_foll+1))
company_data$category = 0

counter=0
for(i in 1:nrow(company_data)){
  if ( company_data[i,9]>start && company_data[i,10]>start){
    if( company_data[i,9] <= (company_data[i,10]+sliding_window) && company_data[i,9] >=( company_data[i,10]-sliding_window)){
      company_data$category[i] = 1
      counter= counter+1
    }
  }
}
counter
ggplot( aes(y1,y2, color=factor(category)),data=company_data)+geom_point()+
  scale_color_manual(values=c("blue", "red"))+ xlab("Hyperlink Network")+ylab("Follower Network")


logit_m1 = glm(category~year+log(sales+1)+log(empl+1), family = 'binomial', data = company_data)
stargazer(logit_m1, type='text')
vif(logit_m1)
coeftest(logit_m1, vcov=vcovHC(logit_m1))


########################################################################
# Cognitive Proximity
########################################################################
## Question: Does sales empl, year have significant influence on the probability of a company to have 
## equaly high values of cognitive proximity in both graphs?

sliding_window = 0.2
start = 0.2

target <- company_data$company_names
df = cog
cog1 = df[match(target, df$nodes),]
company_data[company_data$company_names != cog1$nodes,]

company_data$y1 = ((cog1$cog_proxi_h))
company_data$y2 = ((cog1$cog_proxi_f))
df = company_data[!is.na(company_data$y1),]
df = df[!is.na(df$y2),]
df$y1 = norm(log(df$y1+1))
df$y2 = norm(log(df$y2+1))

company_data = df
company_data$category = 0

counter=0
for(i in 1:nrow(company_data)){
  if ( company_data[i,9]>start && company_data[i,10]>start){
    if( company_data[i,9] <= (company_data[i,10]+sliding_window) && company_data[i,9] >=( company_data[i,10]-sliding_window)){
      company_data$category[i] = 1
      counter= counter+1
    }
  }
}
counter
ggplot( aes(y1,y2, color=factor(category)),data=company_data)+geom_point()+
  scale_color_manual(values=c("blue", "red"))+ xlab("Hyperlink Network")+ylab("Follower Network")


logit_m1 = glm(category~year+log(sales+1)+log(empl+1), family = 'binomial', data = company_data)
stargazer(logit_m1, type='text')
summary(logit_m1)
vif(logit_m1)
coeftest(logit_m1, vcov=vcovHC(logit_m1))








