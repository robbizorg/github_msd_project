## File to Scrape the Top Repos from GitHub

import requests
import secret
import json

NUM_REPOS = 2000
repos = []
query = "repositories"

# 1,12
i = 1 # Counter for the Page
while len(repos) < NUM_REPOS:
    # r  = requests.get("https://github-ranking.com/" + query)
    r = requests.get('https://api.github.com/search/repositories' + '?q=stars:500..600&sort=stars&order=desc&client_id=' + 
        secret.client_id + '&client_secret=' + secret.client_secret + "&page=" + str(i) + "&per_page=100")
    data = r.json()
    # print data["items"]
    i+=1

    try:
        for obj in data["items"]:
            repos.append(obj["full_name"])
    except:
        break
        # print r.text
        # print("Failed for " + str(len(repos)) + " repos and " + str(i) + "page")

  
print("Final i Value: " + str(i))
with open("./repos.csv", 'w') as repofile:
    for repo in repos:
        repofile.write(repo + "\n")