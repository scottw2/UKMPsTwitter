import tweepy
import time
import pandas
import numpy
from datetime import datetime
import glob


consumer_key = "xxxxxxx"
consumer_secret = "xxxxxxx"

access_token = "xxxxxxx"
access_token_secret = "xxxxxxxx"

auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
auth.set_access_token(access_token, access_token_secret)

api = tweepy.API(auth, wait_on_rate_limit=True, wait_on_rate_limit_notify=True)

# import mp list
MP_screen_names = pandas.read_csv(r"C:\Users\Scott\Documents\blog posts\TwitterMPS\MPsonTwitter_list_name_100221.csv")
MP_screen_names["Screen name"] = MP_screen_names["Screen name"].str.replace("@", "")
MP_screen_names_list = MP_screen_names["Screen name"].tolist()

# 1st quarter done 15/02
list_quarters = numpy.array_split(MP_screen_names_list, 4)
screen_names_list_1 = list_quarters[0].tolist()
screen_names_list_2 = list_quarters[1].tolist()
screen_names_list_3 = list_quarters[2].tolist()
screen_names_list_4 = list_quarters[3].tolist()
# this guy deleted his twitter account after getting sacked apparently, so exclude him
screen_names_list_2.remove('JNHanvey')
# private profile, so exclude
screen_names_list_4.remove('thisischerilyn')
# banned, so also exclude
screen_names_list_4.remove('AJRichardsonMP')

# JUST IDs - seemingly much faster way - just have to "rehydrate" the ids back into the full screen_name name data

friends1_id = []
source_screen_name = []
# for loop that for user in above screen_names_list gets all the ids of people they follow
for screen_name in screen_names_list_4_x:
    for page in tweepy.Cursor(api.friends_ids, screen_name=screen_name, count=5000).pages():
        print("fetching following for", screen_name)
        for user in page:
            # the try and except are for if you go over the 15 minute limit, however error handler doesn't seem to work perfectly.
            try:
                friends1_id.append(user)
                source_screen_name.append(screen_name)
            except tweepy.TweepError as error:
                print("Going to sleep:", error)
                time.sleep(60 * 15)

# if the dataframe is too long can trim down to just those who are followed by atleast 2 MPs
id_list = pandas.DataFrame({'source_screen_name': source_screen_name, 'followed_id': friends1_id})
id_list_filename = "MPFollowingIDs-"+datetime.now().strftime("%Y-%m-%d-%H-%M.csv")
id_list.to_csv(id_list_filename, index=False)

id_list_long = pandas.concat(map(pandas.read_csv, ["MPFollowingIDs-2021-02-15-01-44.csv", "MPFollowingIDs-2021-02-15-23-52.csv", "MPFollowingIDs-2021-02-16-23-53.csv", "MPFollowingIDs-2021-02-17-00-12.csv", "MPFollowingIDs-2021-02-17-22-14.csv", "MPFollowingIDs-2021-02-17-23-49.csv", "MPFollowingIDs-2021-02-18-01-05.csv"]))
id_list_unique_ids = id_list_long.followed_id.unique().tolist()

# gets those followed by >=5 mps
id_list_5 = id_list.groupby("followed_id").filter(lambda x: len(x) >= 5)
# list of unique follows:
id_list_5_unique_ids = id_list_5.followed_id.unique().tolist()
id_list_unique_ids = id_list.followed_id.unique().tolist()

# get info from ids - "rehydrating the ids".
full_friends = []
id_count = int(len(id_list_unique_ids))
# it works - doesn't return exactly correct list length as some users missing/banned etc.

try:
    for i in range((id_count // 100) + 1):
        full_friends.extend(api.lookup_users(user_ids=id_list_unique_ids[i * 100:min((i + 1) * 100, id_count)]))
    print('getting users batch:', i)
except tweepy.TweepError as e:
    print('Something went wrong, quitting...', e)
    time.sleep(15 * 60)

friends = []
friends_screen_name = []
friends_bio = []
friends_followers = []
friends_id = []
friends_location = []
friends_verified = []
friends_status_count = []
friends_favourites_count = []
friends_protected = []

for user in full_friends:
    friends.append(user.name)
    friends_screen_name.append(user.screen_name)
    friends_id.append(user.id)
    friends_bio.append(user.description)
    friends_followers.append(user.followers_count)
    friends_location.append(user.location)
    friends_verified.append(user.verified)
    friends_protected.append(user.protected)
    friends_status_count.append(user.statuses_count)
    friends_favourites_count.append(user.favourites_count)

unique_id_userdata = pandas.DataFrame(
    {'followed_id': friends_id, 'followed_name': friends, 'followed_screen_name': friends_screen_name, 'followed_bio': friends_bio,
     'followed_followers': friends_followers,
     'followed_status_count': friends_status_count, 'followed_like_count': friends_favourites_count,
     'followed_verified': friends_verified, 'followed_protected': friends_protected,
     'followed_location': friends_location})

del full_friends
del friends
del friends_screen_name
del friends_bio
del friends_followers
del friends_id
del friends_location
del friends_verified
del friends_status_count
del friends_favourites_count
del friends_protected
del id_list_long
del id_list_unique_ids

# this merges the userdata into the id list we collected earlier.
id_list_full = id_list_long.merge(unique_id_userdata, on='followed_id', how='left')
id_list_full_filename = "MPFollowingUSERSMORETHAN100-"+datetime.now().strftime("%Y-%m-%d-%H-%M.csv")
id_list_full.to_csv(id_list_full_filename, index=False)

# cleaning----------------

# checks missing users
missed_ids = numpy.setdiff1d(id_list_unique_ids, unique_id_userdata.followed_id.unique().tolist()).tolist()
missed_ids_users = []
for user in missed_ids:
    missed_ids_users.extend(api.get_user(user))
missed_ids_list = missed_ids.followed_id.tolist()
missed_ids = pandas.DataFrame(
    {'followed_id': missed_ids})
missed_ids.to_csv("MissedIDs.csv", index = False)

#remove MPs who have deleted accs or were banned or had private accs
MP_screen_names_list.remove('JNHanvey')
MP_screen_names_list.remove('thisischerilyn')
MP_screen_names_list.remove('AJRichardsonMP')
MP_screen_names= MP_screen_names.query("`Screen name` in @MP_screen_names_list")

# read big mp edge list with all nodes from above
mp_network = pandas.read_csv("MPFollowingUSERSMORETHAN10-2021-02-19-00-22.csv")

# remove missing IDS
missingIDs = pandas.read_csv("MissedIDs.csv")
missingIDslist = missingIDs.followed_id.tolist()
mp_network = mp_network.query("followed_id not in @missingIDslist")

mp_network_unique_followed = mp_network.drop(columns=["source_screen_name"])
mp_network_unique_followed = mp_network_unique_followed.drop_duplicates()

# make network edge list of only MPS
# misses LeeAnderson as no MPs actually follow him
mp_network_only_mps = mp_network.query("followed_screen_name in @MP_screen_names_list")
mp_network_only_mps = mp_network_only_mps.rename(columns={"source_screen_name":"Source", "followed_screen_name":"Target"})
mp_network_only_mps = mp_network_only_mps[['Source', 'Target', 'followed_id', 'followed_name',  'followed_bio', 'followed_followers', 'followed_status_count', 'followed_like_count', 'followed_verified', 'followed_protected', 'followed_location']]

#save this
mp_network_only_mps.to_csv("MPfollowingUSERSOnlyMPSx.csv", index=False)

# make node list with party affiliations
MP_node_list_parties = pandas.merge(MP_screen_names, most_followed_MP, how="left", right_on="followed_screen_name", left_on="Screen name")
MP_node_list_parties = MP_node_list_parties.drop(columns=["Followers", "followed_screen_name"])
MP_node_list_parties = MP_node_list_parties.rename(columns={"followers":"MPFollowers", "Screen name":"id"})
list(MP_node_list_parties.columns.values)
MP_node_list_parties = MP_node_list_parties[['id', 'Name', 'Party', 'Constituency', 'MPFollowers', 'followed_id', 'followed_name', 'followed_bio', 'followed_followers', 'followed_status_count', 'followed_like_count', 'followed_verified', 'followed_protected', 'followed_location']]

#save this
MP_node_list_parties.to_csv("MPNodeListParties1.csv", index=False)
