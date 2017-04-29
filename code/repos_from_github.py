## File to Scrape the Top Repos from GitHub

import requests
import secret
from bs4 import BeautifulSoup

NUM_REPOS = 1000
repos = []
query = "repositories"

i = 2 # Counter for the Page
while len(repos) < NUM_REPOS:
    # r  = requests.get("https://github-ranking.com/" + query)
    r = requests.get('https://api.github.com/user/' + '?client_id=' + secret.client_id + '&client_secret=' + secret.client_secret)
    data = r.text

    soup = BeautifulSoup(data)
    result = soup.find_all("div", {"class":"list-group"})
    for res in result:
        for link in res.find_all('a'):
            print((link.get('href'), len(repos)))
            repos.append(link.get('href'))

    ## Going to the Next Page
    query = "repositories?page=" + str(i)
    i += 1

with open("./repos.csv", 'w') as repofile:
    for repo in repos:
        repofile.write(repo + "\n")

#for link in soup.find_all('a'):
#    print(link.get('href'))