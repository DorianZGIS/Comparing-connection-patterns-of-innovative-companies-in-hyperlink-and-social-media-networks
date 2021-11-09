#####################
# Main questions
#####################
# 1.) Is there a difference between those 30 detected Twitter handles and those for which the hyperlinks were found?

# 2.) Is there a difference between 30_twitter detected_hyp and all detectable companies? 
#            2.1) twitter vs all detectable Twitter 
#            2.2) hyperlink vs all detectable Hyperlinks

# 3.) is there a difference between the 11k companies and the 100k companies
#           
#


################################################################
# Note: category c:

# 0: Not detectable  (Having different name up to 4 differences)

# 2: Found correct website handle

# 4: Found nothing at all, but would be detectable

# 5: Found wrong website handle, but not detectable  

# 6: Found wrong Twitter handle, but would be detectable

# Not detectable: 0,5  (For Hyperlink Network: Corresponding website does not exists; For Twitter: Correpsonding Twitter handle does not exists)
# Detectable: 2,4,6    (For Hyperlink Network: Corresponding website exists; For Twitter: Correpsonding Twitter handle exists)
################################################################


library(readr)
library(npmv)

twitter = read_csv('200_sample.csv')
hyperlinks = read_csv('200_sample_hyperlinks.csv')
head(twitter)
colnames(hyperlinks)


################################################################################################################################################
################################################################################################################################################
###### 1.) c=2 of twitter vs c=2 hyperlinks:

# Data preprocessing:

twitter.q1 = twitter[twitter$c==2,c("ABI","Company","Year Established","Employee Size (5) - Location","Sales Volume (9) - Location",'c')]
nrow(twitter.q1)
colnames(twitter.q1)

hyp.q1 = hyperlinks[hyperlinks$c==2,c("ABI","Company","Year Established","Employee Size (5) - Location","Sales Volume (9) - Location",'c')]
nrow(hyp.q1)
colnames(hyp.q1)

twitter.abi = twitter.q1$ABI
hyp.q1.2= hyp.q1[!hyp.q1$ABI %in% c(twitter.abi),]
data_q1 = rbind(hyp.q1.2,twitter.q1) 

data_q1$c2 = c(rep(1,nrow(hyp.q1.2)),rep(2,nrow(twitter.q1)))
colnames(data_q1) = c("ABI","Company","year","empl","sales",'c','c2')

data_q1

data_q1_clean = data_q1[!is.na(data_q1$year),]
nonpartest(formula = empl+year~c2,data=data_q1_clean,permreps=1000, permtest = T,plots=F)
# Hence no significant difference between hyperlinks and Twitter with 
# regards to empl and year
# sales can not be used as it leads to multicolinearity!!!





################################################################################################################################################
################################################################################################################################################
###### 2.1) twitter vs all detectable Twitter 

twitter.q2 = twitter.q1
nrow(twitter.q2)
twitter.q2_abi = twitter.q2$ABI
twitter.q2_detectable = twitter[twitter$c %in% c(2,4,6),c("ABI","Company","Year Established","Employee Size (5) - Location","Sales Volume (9) - Location",'c')]
nrow(twitter.q2_detectable)
twitter.q2_detectable_clean = twitter.q2_detectable[!twitter.q2_detectable$ABI %in% c(twitter.q2_abi),]
nrow(twitter.q2_detectable_clean)

data_q2 = rbind(twitter.q2_detectable_clean, twitter.q2)
data_q2$c2 = c(rep(1,nrow(twitter.q2_detectable_clean)), 
               rep(2,nrow(twitter.q2)))
colnames(data_q2) = c("ABI","Company","year","empl","sales",'c','c2')

data_q2_clean = data_q2[!is.na(data_q2$year),]
(data_q2_clean$c2)
data_q2_clean$year = log(data_q2_clean$year)
data_q2_clean$empl = log(data_q2_clean$empl+1)
data_q2_clean$sales = log(data_q2_clean$sales+1)
t = nonpartest(formula = sales+empl+year~c2, 
           data=data_q2_clean,permreps=10000, 
           permtest = T,plots=F)
t
# very Good no difference!
#library(stargazer)

stargazer(t,type="text")
################################################################################################################################################
################################################################################################################################################
###### 2.2) hyperlink vs all detectable Hyperlinks

hyp.q2 = hyp.q1
nrow(hyp.q2)
hyp.q2_abi = hyp.q2$ABI
hyp.q2_dect = hyperlinks[hyperlinks$c %in% c(2,4,6),
                         c("ABI","Company","Year Established","Employee Size (5) - Location","Sales Volume (9) - Location",'c')]
nrow(hyp.q2_dect)
hyp.q2_dect_clean = hyp.q2_dect[!hyp.q2_dect$ABI %in% c(hyp.q2_abi),]
data_q2.2 = rbind(hyp.q2_dect_clean, hyp.q2)
data_q2.2$c2 = c(rep(1,nrow(hyp.q2_dect_clean)), 
               rep(2,nrow(hyp.q2)))
colnames(data_q2.2) = c("ABI","Company","year","empl","sales",'c','c2')

data_q2.2_clean = data_q2.2[!is.na(data_q2.2$year),]
(data_q2.2_clean$c2)
data_q2.2_clean$year = log(data_q2.2_clean$year)
data_q2.2_clean$empl = log(data_q2.2_clean$empl+1)
data_q2.2_clean$sales = log(data_q2.2_clean$sales+1)
# heavily skewd towards category 2
nonpartest(formula = empl+year~c2,data=data_q2.2_clean,permreps=1000, permtest = T,plots=F)
# very Good no difference!


################################################################################################################################################
################################################################################################################################################
###### 3.) is there a difference between the 11k companies and the 100k companies

k11 = read_csv('11k_with_attr.csv')
k100 = read_csv('100k.csv')


colnames(k11)
k11_attr = k11[c("ABI","Year Established","Employee Size (5) - Location","Sales Volume (9) - Location")]
colnames(k11_attr) =c('abi','year',"empl","sales")
k100_attr = k100[c("ABI","Year Established","Employee Size (5) - Location","Sales Volume (9) - Location")]
colnames(k100_attr) =c('abi','year',"empl","sales")
# testing for multivariate normality
library(MVN)

k11_attr = k11_attr[!is.na(k11_attr$year),]
k11_attr$year = log(k11_attr$year)
k11_attr$empl = log(k11_attr$empl+1)
k11_attr$sales = log(k11_attr$sales+1)
data_test1 = k11_attr[,c('year',"empl","sales")]
data_test2 = k100_attr[,c('year',"empl","sales")]
plot(k11_attr[,c('year',"empl","sales")])
plot(k100_attr[,c('year',"empl","sales")])
mvn(nrow(data_test1))
mvn(data_test2[sample(nrow(data_test2),5000),])

k11_abi = k11$ABI

cats = c()
counts=0
for (abi in k100$ABI){
  if( abi %in% c(k11_abi)){
    cats = c(cats, 2)
    counts = counts+1
    print(counts)
  }
  else{
    cats = c(cats, 1)
  }
}

for (abi in k100$ABI){
  print(abi)
}
k11_abi
cats
k100_attr$cat = cats


k11_attr = k11_attr[!is.na(k11_attr$year),]
k11_attr$year = log(k11_attr$year)
k11_attr$empl = log(k11_attr$empl+1)
k11_attr$sales = log(k11_attr$sales+1)
data_test1 = k11_attr[,c('year',"empl","sales")]
data_test2 = k100_attr[,c('year',"empl","sales")]

for (i in (1:100)){# 100 samples
  smpl_11 = data_test1[sample(nrow(data_test1),30),]
  smpl_100 = data_test2[sample(nrow(data_test2),30),]
  data_rbind = rbind(smpl_11,smpl_100)
  cat = c(rep(1,30),rep(2,30))
  data_rbind$cat = cat
  nptest = nonpartest(formula = empl+year~cat,data=data_rbind,permreps=1000, permtest = T,plots=F)
  nptest$results$`P-value`
  
  
}

#problem not really good to compare since size of the samples big.
nptest




# No normality!!
library(npmv)

nonpartest(formula = empl+year~c2,data=data_q1_clean,permreps=1000, permtest = T,plots=F)



