import yaml
import json
import requests
params=yaml.load(open('request.yaml'), Loader=yaml.FullLoader)
json.dump(params, open('request.json', 'w'), indent=4)
# Set the API endpoint URL
url = 'https://api.ipums.org/extracts?collection=usa&version=2'

# Get the API key from environment variable
api_key ='59cba10d8a5da536fc06b59db6be3fccc50047d8ac579069242699cd'

# Set the headers
headers = {
    'Authorization': api_key,
    'Content-Type': 'application/json'
}
params
# Construct the JSON payload
# Send the POST request
response = requests.post(url, headers=headers, json=params)

res=requests.get(url = 'https://api.ipums.org/extracts/36/?collection=usa&version=2', headers=headers)
res.json()
# Print the response
print(response.status_code)
print(response.json())