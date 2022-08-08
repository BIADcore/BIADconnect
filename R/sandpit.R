#-----------------------------------------------------------------------------------------
# general sandpit
#-----------------------------------------------------------------------------------------
source('functions.R')
#-----------------------------------------------------------------------------------------
# import problems
#-----------------------------------------------------------------------------------------
pha <- sql.wrapper("SELECT * FROM BIAD.Phases",user,password,hostname,hostuser,keypath,ssh)
new <- read.csv('C14Import.csv', encoding='UTF8')
both <- merge(pha[,c('PhaseID','SiteID','Period')],new[,c('PhaseID','SiteID','Period')],by='PhaseID')

bad.i <- (both$SiteID.x!=both$SiteID.y) | (both$Period.x!=both$Period.y)
both[bad.i,]

new <- read.csv('Isot.csv', encoding='UTF8')
#-----------------------------------------------------------------------------------------
# merge FaunalBones and FaunalBiometrics ino a new FaunalBones
#-----------------------------------------------------------------------------------------
bo <- sql.wrapper("SELECT * FROM BIAD.FaunalBones",user,password,hostname,hostuser,keypath,ssh)
bi <- sql.wrapper("SELECT * FROM BIAD.FaunalBiometrics",user,password,hostname,hostuser,keypath,ssh)
both <- merge(bo,bi,by='BoneID')
newID <- both$MetricID+9000000
newID <- as.character(newID)
newID <- paste('M',substring(newID,2),sep='')
stamps <- data.frame(time_added=rep(NA,nrow(both)), user_added=rep(NA,nrow(both)), time_last_update=both$timestamp.x, user_last_update=both$userstamp.x)
castrate <- data.frame(Castrate=NA)
new <- cbind(data.frame(MetricID=newID),both[,c('BoneID','PhaseID','TaxonCode','Element','Sex')],castrate,both[,c('Measurement','Value')],stamps)

i <- new$Sex=='Undet'
new$Sex[i] <- 0.5; new$Castrate[i] <- NA
i <- new$Sex=='Castrate'
new$Sex[i] <- 0; new$Castrate[i] <- 1
i <- new$Sex=='Castrate?'
new$Sex[i] <- NA; new$Castrate[i] <- 0.5
i <- new$Sex=='Male'
new$Sex[i] <- 0; new$Castrate[i] <- 0
i <- new$Sex=='Male?'
new$Sex[i] <- 0.25; new$Castrate[i] <- NA
i <- new$Sex=='Female'
new$Sex[i] <- 1; new$Castrate[i] <- 0
i <- new$Sex=='Female?'
new$Sex[i] <- 0.75; new$Castrate[i] <- NA

sql <- c()
for(n in 1:nrow(new)){
	i <- !is.na(new[n,])
	names <- paste(names(new)[i],collapse="`, `")
	values <- paste(new[n,i], collapse="', '")
	sql[n] <- paste("INSERT INTO `BIAD`.`FaunalAdrian` (`",names, "`) VALUES ('", values, "')",sep="")
	}

sql.wrapper(sql,user,password,hostname,hostuser,keypath,ssh)

#-----------------------------------------------------------------------------------------
# quick look at interacting with google scholar
#-----------------------------------------------------------------------------------------
library(rvest)
url <- "https://scholar.google.com/scholar?q=apples+pears"
url <- "https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=apples+pears&btnG="

page <- url %>% read_html() %>% html_elements




page <- read_html(url)
all.links <- page%>% html_nodes("a") %>% html_attr( "href")
#-----------------------------------------------------------------------------------------
page <- read_html(url)
page %>% html_elements("section")

#-----------------------------------------------------------------------------------------
url<- "https://scholar.google.com/scholar?q=adrian+timpson"
require(rvest)
require(xml2)
require(selectr)
require(stringr)
require(jsonlite)
require(purrr)

wp <- read_html(url)

titles <- html_text(html_nodes(wp, '.gs_rt'))
authors_years <- html_text(html_nodes(wp, '.gs_a'))
authors <- gsub('^(.*?)\\W+-\\W+.*', '\\1', authors_years, perl = TRUE)
years <- gsub('^.*(\\d{4}).*', '\\1', authors_years, perl = TRUE)
titles <- gsub('\\[.+?\\]', '', titles, perl = TRUE)
titles <- trimws(titles)
authors <- trimws(authors)

leftovers <- authors_years %>% 
		str_remove_all(authors) %>% 
		str_remove_all(years)

journals <- str_split(leftovers, "-") %>% 
            map_chr(2) %>% 
            str_extract_all("[:alpha:]*") %>% 
            map(function(x) x[x != ""]) %>% 
            map(~paste(., collapse = " ")) %>% 
            unlist()

df <- data.frame(titles = titles, authors = authors, years = years, journals = journals, stringsAsFactors = FALSE)

#------------------------------------------------------------------------------------
remotes::install_github("ropensci/fulltext")

https://stackoverflow.com/questions/55064193/retrieve-citations-of-a-journal-paper-using-r
library(fulltext)
res1 <- ft_search(query = "Protein measurement with the folin phenol reagent", from = "crossref")
res1 <- ft_links(res1)
res1$crossref$ids













# File-Name: GScholarScraper_3.R
# Date: 2012-08-22
# Author: Kay Cichini
# Email: kay.cichini@gmail.com
# Purpose: Scrape Google Scholar search result
# Packages used: XML
# Licence: CC BY-SA-NC
#
# Arguments:
# (1) input:
# A search string as used in Google Scholar search dialog
#
# (2) write:
# Logical, should a table be writen to user default directory?
# if TRUE a CSV-file with hyperlinks to the publications will be created.
#
# Caveat: if a submitted search string gives more than 1000 hits there seem
# to be some problems (I guess I'm being stopped by Google for roboting the site..)

GScholar_Scraper <- function(input, write = F) {

    require(XML)

    # putting together the search-url:
    url <- paste("http://scholar.google.com/scholar?q=", input, "&num=1&as_sdt=1&as_vis=1", 
        sep = "")

    # get content and parse it:
    doc <- htmlParse(url)
    
    # number of hits:
    x <- xpathSApply(doc, "//div[@id='gs_ab_md']", xmlValue)
    y <- strsplit(x, " ")[[1]][2] 
    num <- as.integer(sub("[[:punct:]]", "", y))
    
    # If there are no results, stop and throw an error message:
    if (num == 0 | is.na(num)) {
        stop("\n\n...There is no result for the submitted search string!")
    }
    
    pages.max <- ceiling(num/100)
    
    # 'start' as used in url:
    start <- 100 * 1:pages.max - 100
    
    # Collect urls as list:
    urls <- paste("http://scholar.google.com/scholar?start=", start, "&q=", input, 
        "&num=100&as_sdt=1&as_vis=1", sep = "")
    
    scraper_internal <- function(x) {
        
        doc <- htmlParse(x, encoding="UTF-8")
        
        # titles:
        tit <- xpathSApply(doc, "//h3[@class='gs_rt']", xmlValue)
        
        # publication:
        pub <- xpathSApply(doc, "//div[@class='gs_a']", xmlValue)
        
        # links:
        lin <- xpathSApply(doc, "//h3[@class='gs_rt']/a", xmlAttrs)
        
        # summaries are truncated, and thus wont be used..  
        # abst <- xpathSApply(doc, '//div[@class='gs_rs']', xmlValue)
        # ..to be extended for individual needs
        
        dat <- data.frame(TITLES = tit, PUBLICATION = pub, LINKS = lin)
        return(dat)
    }

    result <- do.call("rbind", lapply(urls, scraper_internal))
    if (write == T) {
      result$LINKS <- paste("=Hyperlink(","\"", result$LINKS, "\"", ")", sep = "")
      write.table(result, "GScholar_Output.CSV", sep = ";", 
                  row.names = F, quote = F)
      shell.exec("GScholar_Output.CSV") 
      } else {
      return(result)
    }
}

input <- "allintitle:live on mars"
x <- GScholar_Scraper(input, write = F)


library(scholar) 
coauthor_network <- get_coauthors('amYIKXQAAAAJ&hl')

get_scholar_id(last_name = "timpson", first_name = "adrian", affiliation = NA)
