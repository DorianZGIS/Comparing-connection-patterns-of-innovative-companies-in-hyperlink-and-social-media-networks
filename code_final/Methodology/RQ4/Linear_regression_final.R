########################################################################
# Linear Regression models RQ4
########################################################################

library(readr)
library(stargazer)
library(car)
library(lmtest)
library(sandwich)


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

# sanity check
company_data$company_names == degree$nodes

company_data$y1 = degree$deg_h_out
company_data$y2 = degree$deg_f_out
df = company_data
df2 = df[!is.na(df$sales),]
df3 = df2[!is.na(df2$year),]
df4 = df3[!is.na(df3$empl),]

##################
# Hyperlinks
##################
m1 = lm((log(y1+1)) ~ (year)+(log(sales+1))+(log(empl+1)), df4)
#summary(m1)
stargazer(m1,type="text")
vif(m1)
coeftest(m1, vcov=vcovHC(m1))
##################
# Follower
##################
m2 = lm((log(y2+1)) ~ (year) +log(sales+1)+log(empl+1),df4)
#summary(m2)
summary(m2,type="text")
# check for correlation between the coefficients
vif(m2)
# calculate robust standart errors
coeftest(m1, vcov=vcovHC(m1))
coeftest(m2, vcov=vcovHC(m2))

#plot(df4$sales,df4$empl)
#df4[df4$empl == max(df4$empl),]$company_names

########################################################################
# Closeness
########################################################################

company_data$y1 = close$hyp_out_close
company_data$y2 = close$foll_out_close
df = company_data
df2 = df[!is.na(df$sales),]
df3 = df2[!is.na(df2$year),]
df4 = df3[!is.na(df3$empl),]

##################
# Hyperlinks
##################
m1 = lm((log(y1+1)) ~ (year)+(log(sales+1))+(log(empl+1)), df4)
#summary(m1)
stargazer(m1,type="text")
vif(m1)
coeftest(m1, vcov=vcovHC(m1))
##################
# Follower
##################
m2 = lm((log(y2+1)) ~ (year) +log(sales+1)+log(empl+1),df4)
#summary(m2)
summary(m2,type="text")
# check for correlation between the coefficients
vif(m2)
# calculate robust standart errors
coeftest(m1, vcov=vcovHC(m1))
coeftest(m2, vcov=vcovHC(m2))

########################################################################
# Betweenness
########################################################################

company_data$y1 = betw$betw_hyp
company_data$y2 = betw$betw_foll
df = company_data
df2 = df[!is.na(df$sales),]
df3 = df2[!is.na(df2$year),]
df4 = df3[!is.na(df3$empl),]

##################
# Hyperlinks
##################
m1 = lm((log(y1+1)) ~ (year)+(log(sales+1))+(log(empl+1)), df4)
#summary(m1)
stargazer(m1,type="text")
vif(m1)
coeftest(m1, vcov=vcovHC(m1))
##################
# Follower
##################
m2 = lm((log(y2+1)) ~ (year) +log(sales+1)+log(empl+1),df4)
#summary(m2)
summary(m2,type="text")
# check for correlation between the coefficients
vif(m2)
# calculate robust standart errors
coeftest(m1, vcov=vcovHC(m1))
coeftest(m2, vcov=vcovHC(m2))


########################################################################
# Geographic Proximity
########################################################################
# matching company attributes to geographic proximity
#company_data[company_data$company_names != geo$nodes,]
#data[!(data$company_names %in% geo$nodes),]
target <- company_data$company_names
df = geo
geo1= df[match(target, df$nodes),]
company_data[company_data$company_names != geo1$nodes,]

company_data$y1 = geo1$mean_geographical_proximity_hyp
company_data$y2 = geo1$mean_geographical_proximity_foll
df = company_data
df2 = df[!is.na(df$sales),]
df3 = df2[!is.na(df2$year),]
df4 = df3[!is.na(df3$empl),]

##################
# Hyperlinks
##################
m1 = lm((log(y1+1)) ~ (year)+(log(sales+1))+(log(empl+1)), df4)
#summary(m1)
stargazer(m1,type="text")
vif(m1)
coeftest(m1, vcov=vcovHC(m1))
##################
# Follower
##################
m2 = lm((log(y2+1)) ~ (year) +log(sales+1)+log(empl+1),df4)
#summary(m2)
summary(m2,type="text")
# check for correlation between the coefficients
vif(m2)
# calculate robust standart errors
coeftest(m1, vcov=vcovHC(m1))
coeftest(m2, vcov=vcovHC(m2))


########################################################################
# Cognitive Proximity
########################################################################
# matching company attributes to cognitive proximity
target <- company_data$company_names
df = cog
cog1= df[match(target, df$nodes),]
company_data[company_data$company_names != cog1$nodes,]

company_data$y1 = cog1$cog_proxi_hyp
company_data$y2 = cog1$cog_proxi_foll
df = company_data
df2 = df[!is.na(df$sales),]
df3 = df2[!is.na(df2$year),]
df4 = df3[!is.na(df3$empl),]

##################
# Hyperlinks
##################
m1 = lm((log(y1+1)) ~ (year)+(log(sales+1))+(log(empl+1)), df4)
#summary(m1)
stargazer(m1,type="text")
vif(m1)
coeftest(m1, vcov=vcovHC(m1))
##################
# Follower
##################
m2 = lm((log(y2+1)) ~ (year) +log(sales+1)+log(empl+1),df4)
#summary(m2)
summary(m2,type="text")
# check for correlation between the coefficients
vif(m2)
# calculate robust standart errors
coeftest(m1, vcov=vcovHC(m1))
coeftest(m2, vcov=vcovHC(m2))



