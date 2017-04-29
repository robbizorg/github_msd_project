import json
import os

i = 0
with open('./users.txt', 'w') as userfile:
	for json_file in os.listdir("./contributors"):
		with open("./contributors/" + json_file) as data_file:    
		    data = json.load(data_file)
		    # print json_file
		    for user in data:
		    	try:
		    		userfile.write(str(user["id"]) + "\n")
		    	except:
		    		print(json_file + " failed")

