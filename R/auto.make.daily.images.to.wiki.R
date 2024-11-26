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
headers <- httr::add_headers(Authorization = paste('Bearer',token))

# URL for the POST request
url <- 'https://biadwiki.org/u'

# get image filepaths and types
path <- list.files('../tools/plots', full.names=TRUE)
type <- paste('image',file_ext(path),sep='/')
path <- c(list.files('../tools/templates', full.names=TRUE),path)
type <- c(paste('csv',file_ext(path),sep='/'),type)
path <- c(list.files('../tools/table_comments', full.names=TRUE),path)
type <- c(paste('html',file_ext(path),sep='/'),type)
path <- c(list.files('../tools/summary_stats', full.names=TRUE),path)
type <- c(paste('html',file_ext(path),sep='/'),type)

# loop for each image file
N <- length(path)
print(paste("sending",N,"files to wiki"))

for(n in 1:N){
time.start <- proc.time()[3]
    prog  <-  paste0("sending file: ", n, "/", N, " [", basename(path[n]), "]")
    cat('\r',sprintf("%-*s", 80, prog), sep="")

    tryCatch({
        # Set up the body of the request
        body <- list(
                     mediaUpload = httr::upload_file(path = path[n], type = type[n]),
                     mediaUpload = '{"folderId":0}'
        )

        Sys.sleep(.1)
        # Make the POST request
        result <- httr::POST(url, headers, body = body)
	time.end <- proc.time()[3]
		print(paste('time taken to upload to wiki:', round(time.end-time.start,2),'seconds'))

        # Print the result if problematic
        res <- httr::content(result, "text")
        if(res!='ok')print(res)
    },error=function(err){
        print(err)
    })
	}
cat("\n")
#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
