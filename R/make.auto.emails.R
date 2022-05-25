#-----------------------------------------------------------------------------------------
# Test autoemailing
# This script is reliant on files that are not stored on the public github:
# gmailr.json and .secret stored in tools/email
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
