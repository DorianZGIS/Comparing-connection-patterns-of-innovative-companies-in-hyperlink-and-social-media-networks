########################################################################
# Statistical Tests RQ2 & RQ3
#
# Testing statistical differences between the distributions of the Hyperlink and Follower Network
# using Kolmogorov-Smirnov test & Wilcoxon signed rank test
########################################################################

library(readr)

########################################################################
# Data
########################################################################
centrality_data = read_csv('centrality_data.csv')
mean_geographical_proximity = read_csv('mean_geographical_proximity.csv')
mean_cognitive_proximity = read_csv('cognitive_proximity_numeric.csv')

head(centrality_data)




########################################################################
# Degree
########################################################################
# median differences 
degree_hyp = centrality_data$degree_hyp
degree_foll = centrality_data$degree_foll
n1 = length(degree_hyp)
n2 = length(degree_foll)
median(normalize(log(degree_hyp+1)), na.rm=T)
median(normalize(log(degree_foll+1)), na.rm=T)
test = wilcox.test((degree_hyp),(degree_foll), mu = 0, alt = 'two.sided', paired = T,
            conf.int = T, conf.level = 0.95, correct =T)
test$p.value
# p-value is very small => indicating that the Nullhypothesis can be rejected
# Hence, there is a statistical significant difference between the group medians

# differences between the distributions
ks.test(scale(log(degree_hyp+1)), (log(degree_foll+1)),alternative = 'two.sided')
# There is a statistical significant difference between the distributions 


########################################################################
# out_closeness
########################################################################
clos_hyp = centrality_data$closeness_hyp
clos_foll = centrality_data$closeness_foll
test = wilcox.test(log(clos_hyp+1),log(clos_foll+1), mu =0, alt = 'two.sided', paired = T,
                   conf.int = T, conf.level = 0.95, correct =T)
test$p.value
# p-value is very small => indicating that the Nullhypothesis can be rejected
# Hence, there is a statistical significant difference between the group medians


# differences between the distributions
ks.test(log(clos_hyp+1),log(clos_foll+1),alternative = 'two.sided')
# There is a statistical significant difference between the distributions 


########################################################################
# out_betweenness
########################################################################
betw_hyp = centrality_data$betweenness_hyp
betw_foll = centrality_data$betweenness_foll
test = wilcox.test(log(betw_hyp+1),log(betw_foll+1), mu =0, alt = 'two.sided', paired = T,
                   conf.int = T, conf.level = 0.95, correct =T)
test$p.value
# p-value is very small => indicating that the Nullhypothesis can be rejected
# Hence, there is a statistical significant difference between the group medians


# differences between the distributions
ks.test(log(betw_hyp+1),log(betw_foll+1),alternative = 'two.sided')
# There is a statistical significant difference between the distributions 


########################################################################
# geopgraphic proximity
########################################################################
mgp_hyp = mean_geographical_proximity$mean_geographical_proximity_hyp
mgp_foll = mean_geographical_proximity$mean_geographical_proximity_foll

test = wilcox.test(log(mgp_hyp+1), log(mgp_foll+1), mu =0, alt = 'two.sided', paired = T,
                   conf.int = T, conf.level = 0.95, correct =T)
test$p.value
# p-value is very small => indicating that the Nullhypothesis can be rejected
# Hence, there is a statistical significant difference between the group medians


ks.test((log(mgp_hyp+1)), (log(mgp_foll+1)))
# There is a statistical significant difference between the distributions 


########################################################################
# cognitive proximity
########################################################################
cog_hyp = mean_cognitive_proximity$cog_proxi_hyp
cog_foll = mean_cognitive_proximity$cog_proxi_foll

length(cog_hyp)
length(cog_foll)
median(cog_hyp,na.rm = T)
median(cog_foll,na.rm = T)

test = wilcox.test(log(cog_hyp+1), log(cog_foll+1), na.rm = TRUE,mu =0, alt = 'two.sided', paired = T,
                   conf.int = T, conf.level = 0.95, correct =T)
test$p.value
# p-value is very small => indicating that the Nullhypothesis can be rejected
# Hence, there is a statistical significant difference between the group medians

ks.test((log(cog_hyp+1)), (log(cog_foll+1)))
# There is a statistical significant difference between the distributions 


