import json

with open("Untitled-1.json") as f:
    data = json.load(f)


values = data["widgets"][0]["data"]
print(min(values), max(values))

print(len([x for x in values if x > 40]))
print(len([x for x in values if x > 30 and x < 40]))