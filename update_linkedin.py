#!/usr/bin/env python3

import secrets

# destructure a dict into a tuple
pluck = lambda dict, *args: (dict[arg] for arg in args)

# documentation:
# https://docs.microsoft.com/en-us/linkedin/shared/integrations/people/profile-edit-api?context=linkedin/compliance/context#how-to-perform-a-profile-edit

# to change:
URL = 'https://api.linkedin.com/v2/people/(id:{person ID})'
REDIRECT_URI = 'https://jacob.chvatal.com'

CLIENT_ID = secrets.CLIENT_ID
CLIENT_SECRET = secrets.CLIENT_SECRET

PREFERRED_LOCALE =  {
    "country": "US",
    "language": "en"
}

def get_access_token():
    def generateScope(scopes):
        return ''

    # GET https://www.linkedin.com/oauth/v2/authorization
    body = {
        response_type: 'code',
        client_id: CLIENT_ID,
        redirect_uri: REDIRECT_URI,
        scope: generateScopes([])
    }

    # response 1
    code = 3
    state = False

    # POST https://www.linkedin.com/oauth/v2/accessToken
    # Content-Type x-www-form-urlencoded
    # grant_type: authorization_code
    # code: authoriation code from 

    body = {
        grant_type: code,
        code: code,
        redirect_uri: REDIRECT_URI,
        client_id: CLIENT_ID,
        client_secret: CLIENT_SECRET
    }

    # response 2
    access_token = None
    expires_in = None

    # after getting the access token, Authorization: Bearer {access_token}
    return access_token

def localize(field):
    return {
        "localized": {
            "en_US": field
        },
        "preferredLocale": PREFERRED_LOCALE
    }

def set(obj):
    return {
        "patch": {
            "$set": obj
        }
    }

# fields:
def update_address(addressStr):
    request = set({"address": localize(addressStr)})

    r = requests.post('URL', auth=('user', 'pass'))

def update_name(firstName, lastName):
    request = set({"firstName": firstName})
    request = set({"lastName": lastName})

def update_summary(summary):
    request = set({"summary": localize(summary)})

# format: [{website: website, label: label, category: category}]
def update_websites(websiteList):
    def createWebsiteRequest(website):
        url, label, category = pluck(website)
        return {
            "category": category,
            "label": localize(label)
            "url": localize(url)
        }
    
    reqList = map(createWebsiteRequest, websiteList)
    request = set({"websites": reqList})
