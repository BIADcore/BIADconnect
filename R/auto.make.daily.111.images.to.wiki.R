#--------------------------------------------------------------------------------------------------
# Move images over BIADwiki for hosting, using REST API
#--------------------------------------------------------------------------------------------------
require(httr)
require(tools)
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
# get authorisation token
token <- readLines('../tools/API/API.txt')

# set headers
headers <- add_headers(Authorization = paste('Bearer',token))

# URL for the POST request
url <- 'https://biadwiki.org/u'

# get image filepaths and types
path <- list.files('../tools/plots', full.names=TRUE)
type <- paste('image',file_ext(path),sep='/')

# loop for each image file
N <- length(path)
for(n in 1:N){

	# Set up the body of the request
	body <- list(
		mediaUpload = upload_file(path = path[n], type = type[n]),
		mediaUpload = '{"folderId":0}'
		)

	# Make the POST request
	result <- POST(url, headers, body = body)

	# Print the result
	print(paste(path[n],content(result, "text")))
	}
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
