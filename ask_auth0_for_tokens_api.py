#https://manage.auth0.com/dashboard/eu/mbads-thesis/apis/606a2372bc792e00d1af949c/test

import http.client
import pandas as pd


conn = http.client.HTTPSConnection("mbads-thesis.eu.auth0.com")

payload = "{\"client_id\":\"ZTLJo9DtATO1OTKZOQ1XUAJwDMIwO6hf\",\"client_secret\":\"ZnyOHJFUEy8kIjjQl5Oykp2WYwy44L6U9GcHZpXXoO3_pJI8n-xloeEUkhkmKqAz\",\"audience\":\"https://mbads-thesis.eu.auth0.com/api/v2/\",\"grant_type\":\"client_credentials\"}"

headers = { 'content-type': "application/json" }

conn.request("POST", "/oauth/token", payload, headers)

res = conn.getresponse()
data = res.read()

ask_api = pd.DataFrame(eval(data),index=[0])

