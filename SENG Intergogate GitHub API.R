install.packages("jsonlite")
library(jsonlite)
install.packages("httpuv")
library(httpuv)
install.packages("httr")
library(httr)

# Can be github, linkedin etc depending on application 
oauth_endpoints("github") ## This sets the API we want to interrgogate as being GitHub

GitHubSENGMeasApp <- oauth_app(appname = "GitHub_SENG_Meas_App",
                   key = "8758a6bf9a146e1da0c1",
                   secret = "b9504edde46b794414495bd9c33ea28cbfd87824")

# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)

# Take action on http error
stop_for_status(req)

# Extract content from a request
json1 = content(req)

# Convert to a data.frame
gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))

# Subset data.frame
gitDF[gitDF$full_name == "jtleek/datasharing", "created_at"] 
