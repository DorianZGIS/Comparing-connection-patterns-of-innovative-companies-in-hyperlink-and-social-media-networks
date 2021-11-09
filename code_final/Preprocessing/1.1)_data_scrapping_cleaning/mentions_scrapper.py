from tweepy import API
from tweepy import Cursor
from tweepy import OAuthHandler
import pandas as pd
import re
import tweepy
import twitter_credentials
import time


class TwitterAuthenticator():
    def authenticate_twitter_app(self):
        auth = OAuthHandler(twitter_credentials.CONSUMER_KEY,twitter_credentials.CONSUMER_SECRET)
        auth.set_access_token(twitter_credentials.ACCESS_TOKEN, twitter_credentials.ACCESS_TOKEN_SECRET)
        return auth
    
def get_mentions(api, max_tweets, search_query, max_ids):
    tweet_count = 0
    mention_tweets_user_ids = []
    retweet_filter='-filter:retweets'
    query = search_query + retweet_filter
    counter = 0
    while tweet_count < max_tweets:
        counter +=1
        try:
            new_tweets_list = api.search(q = query, count = 200, max_id = max_ids) #count=100
            tweet_ids = []
            for tweet in new_tweets_list:
                mention_tweets_user_ids.append(tweet.user.id)
                tweet_ids.append(tweet.id)
            tweet_count += 100
            if tweet_ids != []:
                max_ids = tweet_ids[-1] 
            else:
                print('No mentions of the company: ',search_query)
                return ['no_mentions'], max_ids
            print('Downloading Tweets %d for company %s ' %(tweet_count, search_query))
        except tweepy.TweepError as e:
            # Just exit if any error
            print("Error: " + str(e))
        if tweet_count == max_tweets:
            print('Tweet maximum is reached => exiting!')
    print('The last max_id for company %s was: %d' %(search_query,max_ids))
    return mention_tweets_user_ids, max_ids



def save_user_mention_ids(api,companies, max_tweets, max_ids, location):
    
    estimator = ((len(companies) * max_tweets) / 18000) * 15
    print('Rough time estimation: %d min' %(estimator))
    no_friends = ['no_mentions']*len(company_names)
    df = pd.DataFrame(list(zip(company_names,no_friends)),columns=['company','mentions'])
    counter = 0 
    start = time.time()
    start_unaltered = time.time()
    for comp_name in companies:
        counter +=1
        print('Time since start', int(time.time() - start_unaltered))
        if (time.time() - start) >= 900:
            print('Saving progress after: ', time.time()-start)
            try:
                df.to_csv(location + 'company_mentions_pipeline_output.csv')
                start = time.time()
            except:
                print('#############################################################')
                print('Emergency saving did not work') 
                print('#############################################################')
        print('We are at company: ', counter)
        user_ids_list, max_ids_new = get_mentions(api, max_tweets, comp_name ,max_ids)
        df.loc[df['company'] == comp_name,'mentions'] = str(user_ids_list)
    df.to_csv(location + 'company_mentions_pipeline_output.csv')
    return max_ids_new



if __name__ == '__main__':
    
    data = pd.read_csv('software_firms.csv', sep="\t", engine='python')
    network = pd.read_csv('network.csv')
    companies = data.twitter_handle
    
    auth = TwitterAuthenticator().authenticate_twitter_app()
    api = API(auth, wait_on_rate_limit = True, wait_on_rate_limit_notify = True)
    
    location = '/results/' 
    max_ids = 1315906098776412161 # results will have an ID less than this tweets id (i.e. older than)(This is done in order to mitigate the risk that new tweets are beeing added during the scrapping process which would introduce a temporal bias)
    
    max_tweets = 200  # Maximal amount of tweets scrapped per compnay
    starting_time = time.time()
    max_ids_new = save_user_mention_ids (api,companies, max_tweets, max_ids, location)
    time_needed = int((time.time()-starting_time)/60)
    print('The new max_id is: ', max_ids_new)
    print('Pipeline process is finished after %d min and successfully saved data to: %s' %(time_needed, location))

    
    