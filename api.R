
library(hakaiApi)

hplc_url <- "https://hecate.hakai.org/api/eims/views/output/hplc?limit=1"
hplc_fields_url <- "https://hecate.hakai.org/api/eims/views/output/hplc?limit=1&fields=site_id,hakai_id"
hplc_time_url <- "https://hecate.hakai.org/api/eims/views/output/hplc?limit=-1&fields=site_id,hakai_id,date&date>2024-01-01&site_id=QU39"
#limit=-1 gives everything


# Initialize the client
client <- Client$new()

# https://hecate.hakai.org/api/auth/logout
# https://accounts.google.com/o/oauth2/v2/auth/oauthchooseaccount?scope=email%20profile&prompt=select_account&state=https://hecate.hakai.org/api-client-login&response_type=code&client_id=289782143400-a1qkmq66sdeu4udtaehrgcrnekcf2bm2.apps.googleusercontent.com&redirect_uri=https://hecate.hakai.org/api/auth/client/code2jwt&service=lso&o2v=2&flowName=GeneralOAuthFlow
# https://hakaiinstitute.github.io/hakai-api/endpoints/


# Request some data (request chlorophyll data here)
data <- client$get(hplc_url)
data_fields <- client$get(hplc_fields_url)
data_time <- client$get(hplc_time_url)

# View out the data
View(data)


fields <- "site_id,hakai_id"
endpoint <- "/eims/views/output/chlorophyll"
hplc_fields_url <- paste0("https://hecate.hakai.org/api", endpoint,"?limit=1&", fields)

