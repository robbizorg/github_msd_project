import json

f = open("output.txt","r")
i =0
j = 0
for line in f:
	js = json.loads(line)
	#print(js["bio"])
	if js["bio"] != None and ("university" in (js["bio"]).lower() or "institute" in (js["bio"]).lower() or "college" in (js["bio"]).lower()):
		print(js["bio"])
		i+=1
	j+=1

print(float(i)/float(j))