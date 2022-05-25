#-----------------------------------------------------------------------------------------
# Test autoemailing
# This script is reliant on files that are not stored on the public github:
# gmailr.json and .secret

# Good advice here: https://github.com/jennybc/send-email-with-r
# The gmailr.json was generated once using the gmail API https://console.cloud.google.com/apis/dashboard 
# via 'credentials' -> create and save a new JSON -> change file name.
# The .secret was then generated using the R library gmailr, which uses the .json
#-----------------------------------------------------------------------------------------
library(gmailr)
gm_auth_configure(path='../tools/email/gmailr.json')
gm_auth(email = TRUE, cache = "../tools/email/.secret")

test_email <- gm_mime(
    To = "a.timpson@ucl.ac.uk",
    From = "BIAD.committee@gmail.com",
    Subject = "more testing",
    body = "Im calling from the server")

gm_send_message(test_email)
#-----------------------------------------------------------------------------------------
