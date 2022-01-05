from tweepy import API
from tweepy import Cursor
from tweepy import OAuthHandler
import pandas as pd
import re
import tweepy
import twitter_credentials
import time
import traceback
import smtplib
from importlib.machinery import SourceFileLoader
config = SourceFileLoader('config.py', '').load_module() #load Twitter authentication data


from colorama import Fore, Style, init
init()
import timeout_decorator


class TwitterAuthenticator():
    def authenticate_twitter_app(self):
        auth = OAuthHandler(twitter_credentials.CONSUMER_KEY,twitter_credentials.CONSUMER_SECRET)
        auth.set_access_token(twitter_credentials.ACCESS_TOKEN, twitter_credentials.ACCESS_TOKEN_SECRET)
        return auth

def send_email(subject, msg):
    try:
        server = smtplib.SMTP('smtp.gmail.com:587')
        server.ehlo()
        server.starttls()
        server.login(config.EMAIL_ADDRESS, config.PASSWORD)
        message = 'Subject: {}\n\n{}'.format(subject,msg)
        server.sendmail(config.EMAIL_ADDRESS, '',message) #specify your email adress
        server.quit()
        print('Send email successfully')
    except:
        print('Failed to send email')

@timeout_decorator.timeout(420)
def get_friend_ids(api, name):
    friend_ids_list = []
    counter = 1
    friends_count = api.get_user(name).friends_count
    my_cursor = Cursor(api.friends_ids, screen_name = name, count = 5000).items()
    print(Fore.BLUE + 'company: ' + str(name)+Style.RESET_ALL)
    print('friends count:',friends_count)
    while counter <= friends_count:
        for friend in my_cursor:
            try:
                friend_ids_list.append(friend)
                counter += 1
            except tweepy.TweepError as e:

                if 'Failed to send request:' in e.reason:
                    print (Fore.RED + '1.EXCEPTION: Timeout error caught'+Style.RESET_ALL)
                    time.sleep(60)
                    continue
                if 'Not authorized.' in e.reason:
                    print (Fore.RED + '1.EXCEPTION: Not authorized. error caught'+Style.RESET_ALL)
                    return [name] # if tweets are protected, skip this user 
                print(Fore.RED +  '2.EXCEPTION:'+str(e) +Style.RESET_ALL)
                time.sleep(60)
                continue
            except (TimeoutError, ConnectionError) as exc:  
                print (Fore.RED +  '4.EXCEPTION:'+str(exc) +Style.RESET_ALL)
                time.sleep(60)
                continue
            except Exception as exc:
                print (Fore.RED +  '5.EXCEPTION:'+str(exc) +Style.RESET_ALL)
                time.sleep(60)
                continue
    print('Scrapped %d friends for company %s' %(counter, name))
    print('Sleeping for 60 seconds')
    time.sleep(60)  
    return friend_ids_list


def get_comp_id(company_list,api):
    id_list = []
    for comp in company_list:
        user = api.get_user(screen_name = comp) 
        print(comp)
        print(user.id)
        id_list.append(user.id)
    
    return id_list


if __name__ == '__main__':
    
    companies = pd.read_csv('15k_software_twitter_handle.csv',index_col=0)# load Twitter handles of the 15k Companies
    companies = companies.drop_duplicates(['ABI'])
    comp_id = companies.comp_id
    
    no_friends = ['no_friends']*len(companies.Twitter_handle)
    df = pd.DataFrame(list(zip(companies.Twitter_handle, comp_id ,no_friends)),columns=['company_names','nodes','edges'])
    df['ABI'] = [int(x) for x in companies.ABI]
    
    auth = TwitterAuthenticator().authenticate_twitter_app()
    api = API(auth, wait_on_rate_limit = True, wait_on_rate_limit_notify = True)
    
    counter = 0
    start = time.time()
    start_unaltered = time.time()
    start_email = time.time()
    failed = False
    
    for comp_name in companies.Twitter_handle[counter:15123]:
        counter +=1
        print('Time since start %d seconds, for company_number %s'%(round((time.time() - start_unaltered),2), comp_name))
        if (time.time() - start) >= 900:
            try:
                df.to_csv('company_friends_pipeline_output.csv')
                print(Fore.GREEN +  'Successfully saved after: ' +str(round((time.time()-start),2)) +Style.RESET_ALL)
                start = time.time()
            except:
                print('#############################################################')
                print(Fore.RED + 'Emergency saving did not work') 
                print('Emergency Exit: company name: ', comp_name)
                send_email('EXCEPTION','Emergency: Saving did not work!')
                failed = True
                break
        if round((time.time()-start_email),2)>=3600:
            try:
                send_email('Still alive','Process is still alive and running we are at company %s and time since start is %d'%(comp_name,round((time.time()-start_unaltered),2)) )
                start_email = time.time()
            except:
                print(Fore.RED + 'Failed to send stil alive email'+ Style.RESET_ALL)
        try:
            print(Fore.BLUE + 'We are at company: '+ str(counter) + Style.RESET_ALL)
            df.loc[df['company_names'] == comp_name,'edges'] = str(get_friend_ids(api, comp_name))
        except tweepy.TweepError as e:
            if 'Not authorized.' in e.reason:
                print (Fore.RED + 'Tweepy EXCEPTION: Outer: Not authorized outer error caught'+Style.RESET_ALL)
                continue # if followers are protected, skip this user 
            print(Fore.RED +  'Tweepy EXCEPTION: Outer: '+str(e) +Style.RESET_ALL)
            
        except Exception as e:
            print(Fore.RED + 'Failed to get followers within 80 seconds for company ' + str(comp_name)+ Style.RESET_ALL)
            print(Fore.RED + 'Exception: Outer: ' + str(e)+ Style.RESET_ALL)
            auth = TwitterAuthenticator().authenticate_twitter_app()
            api = API(auth, wait_on_rate_limit = True, wait_on_rate_limit_notify = True)
            print(Fore.BLUE + 'Retry to get followers with new API' + Style.RESET_ALL)
            try:
                df.loc[df['company_names'] == comp_name,'edges'] = str(get_friend_ids(api, comp_name))
            except Exception as e:
                print(Fore.RED +  'EXCEPTION: Outer: Second try with new API failed '+ str(e)+Style.RESET_ALL)
                send_email('Exception','Even retrying with new API was not successfull for company ' +str(comp_name))
                continue
        
        
    print('Exiting the Loop after: ', round((time.time()-start_unaltered),2))
    
    df.to_csv('company_friends_pipeline_final_output.csv')
    if failed==False:
        send_email('SUCCESSFULL','Successfully exited the loop')
    
    

