setwd("C:/restore2/hp39wasi/sWormDatabaseRelease")


# sent <- read.csv("C:\\Google Drive\\sWorm\\sWorm_WS\\EarthwormAnalysis\\Manuscripts\\Data Paper\\DataProviders\\members_SWormDatabase_DataProviders_V2_sent_May_30_2019.csv")
# This doesn't include those that bounced
sent <- read.csv("C:\\Google Drive\\sWorm\\sWorm_WS\\EarthwormAnalysis\\Manuscripts\\Data Paper\\DataProviders\\members_SWormDatabase_DataProviders_V3_sent_Jun_4_2019.csv")



form <- read.csv("C:\\restore2\\hp39wasi\\sWormDatabaseRelease\\Authorship\\Co-Authors on the data paper (Responses) - Form Responses 1.csv")

form_emails <- form$Email.address
form_surnames <- form$Please.enter.your.surname..family.name.

all(form_emails %in% sent$Email.Address)
wrongemail <- which(!(form_emails %in% sent$Email.Address))

all(form_surnames %in% sent$Last.Name)
wrongsurname <- which(!(form_surnames %in% sent$Last.Name))

## Just to check

form[intersect(wrongemail, wrongsurname),]
## That's fine. It's just becauyse of the encoding

## So remove from 'sent' rows where email address are already in the 'form'
tosend <- sent[-(which(sent$Email.Address %in% form_emails)),]


## Some people used different email addresses to sign up
## So a list of surnames

tosend[which(tosend$Last.Name %in% form_surnames),]

tosend <- tosend[-(which(tosend$Last.Name %in% form_surnames)),]


## To this file, I will do a manual check (becuase of formatting of anmes)
## and append on previously bounced emails
## Then resend to the list

write.csv(tosend, file = file.path("Authorship", "toResend_2019-06-04.csv"), row.names = FALSE)
