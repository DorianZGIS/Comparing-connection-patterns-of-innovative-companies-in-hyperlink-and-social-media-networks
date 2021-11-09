####################################################################################
# Testing for differences in the ampunt of connections for the companies 
# which were selected and those which were not selected
####################################################################################


####################################################################################

# Note: category c:

# 0: Not detectable  (Having different name up to 4 differences)

# 2: Found correct website handle

# 4: Found nothing at all, but would be detectable

# 5: Found wrong website handle, but not detectable  

# 6: Found wrong Twitter handle, but would be detectable

# Not detectable: 0,5  (For Hyperlink Network: Corresponding website does not exists; For Twitter: Correpsonding Twitter handle does not exists)
# Detectable: 2,4,6    (For Hyperlink Network: Corresponding website exists; For Twitter: Correpsonding Twitter handle exists)
####################################################################################




library(readr)
library(npmv)
hyperlinks = read_csv('200_sample_hyperlinks.csv')
head(twitter)
colnames(hyperlinks)

############################
# Hyperlinks
############################

h_d = hyperlinks[hyperlinks$c%in%c(2),'links']
h_d2 = hyperlinks[hyperlinks$c%in%c(4,6),'links']

for(i in 1:nrow(h_d)){
  if(!is.na(h_d[i,1])){
    h_d[i,1] = as.numeric(length(unlist(strsplit(as.character(h_d[i,1] ), ","))))
    
  }
  else{
    h_d[i,1]=0
  }
}

for(i in 1:nrow(h_d2)){
  if(!is.na(h_d2[i,1])){
    h_d2[i,1] = as.numeric(length(unlist(strsplit(as.character(h_d2[i,1] ), ","))))
  }  
  else{
    h_d2[i,1]=0
  }
}

t.test(as.numeric(h_d$links), as.numeric(h_d2$links))

############################
# Twitter followers
############################

twitter = read_csv('200_sample.csv')

t_d = twitter[twitter$c%in%c(2),'num_of_followers_on_Twitter']# detected
t_dnot = twitter[twitter$c%in%c(4,6),'num_of_followers_on_Twitter']# not detected but would be detectable


t.test(as.numeric(t_d$num_of_followers_on_Twitter), as.numeric(t_dnot$num_of_followers_on_Twitter))

a = as.numeric(t_d$num_of_followers_on_Twitter)
mean(a)
b = as.numeric(t_dnot$num_of_followers_on_Twitter)
mean(b)

