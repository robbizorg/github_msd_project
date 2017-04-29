# File that Gets the Contributors
import requests
## Put your GitHub OAuth info in a file called secret.py
import secret

repos = []
with open('./repos.csv') as repofile:
	for line in repofile:
		repos.append(line.rstrip())

i = 0
for repo in repos:
	# print repo
	r  = requests.get("https://api.github.com/repos" + repo + "/contributors?client_id=" + secret.client_id + "&client_secret=" + secret.client_secret)
	print (r.headers['X-RateLimit-Remaining'], r.headers['X-RateLimit-Limit'])
	# print r.text
	with open("./contributors/" + repo.split("/")[-1] + ".json", 'w') as phil:
		phil.write(r.text)