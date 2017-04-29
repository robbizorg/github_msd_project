import os
import sys
import time
import requests
import json

f = open ("input.txt","rb+")
usernames = []

def updateInput():
	f = open("input.txt","w")
	for username in usernames:
		f.write(username + "\n")
	f.close()


client_id = sys.argv[1]
client_secret = sys.argv[2]


for line in f:
	username = line.strip()
	usernames.append(username);

f.close()


while True:
	if (len(usernames) == 0):
		break

	usr = usernames[0]
	r = requests.get('https://api.github.com/user/' +usr +'?client_id=' + client_id + '&client_secret=' + client_secret)
	#Assuming it clears out
	if r.ok:
		with open("output.txt", "a+") as myfile:
			out = r.json()
			myfile.write(json.dumps(out) + "\n")
		print("successfully got data")

		del usernames[0]
		updateInput()

		print ("Successfully got user id: " + usr + ", " + r.headers['X-RateLimit-Remaining'] + " requests remaining")

	else:
		print(r.json())
		if ("rate limit" in r.json()["message"]):#error is from rate limit
			print("Rate limit issue, waiting 10 minutes")
			time.sleep(60 * 10)
		else:
			print("ERROR: Something other than rate limit, moving user to end of list and continuing")
			usernames += [usernames.pop(0)]
			updateInput()
		


