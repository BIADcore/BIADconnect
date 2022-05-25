#-----------------------------------------------------------------------------------------
# Test autoemailing
#-----------------------------------------------------------------------------------------
library(gmailr)
gm_auth_configure(path='../tools/email/gmailr.json')
gm_auth(email = TRUE, cache = "../tools/email/.secret")

test_email <- gm_mime(
    To = "a.timpson@ucl.ac.uk",
    From = "BIAD.committee@gmail.com",
    Subject = "more testing",
    body = "Im calling from desktop")

gm_send_message(test_email)
#-----------------------------------------------------------------------------------------
