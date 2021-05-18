# G3_journal_scraping

In this project, we built a specialized R program to crawl, parse and extract useful information from the G3-Genes Genomes Genetics journal.
Given an input year, our objective is to extract all articles published in/after that year from the G3-Genes Genomes Genetics journal.
Required to extract the following 9 fields for each article:

 •	Title
 
 •	Authors
 
 •	Author Affiliations
 
 •	Correspondence Author
 
 •	Correspondence Authors Email
 
 •	Publish Date
 
 •	Abstract
 
 •	Keywords
 
 •	Full Paper (Text format).
 


Project1_journal_scraping R file

#install packages

#package will load the core tidyverse packages:

#ggplot2, for data visualisation.

#dplyr, for data manipulation.

#tidyr, for data tidying.

#readr, for data import.

#purrr, for functional programming.

#tibble, for tibbles, a modern re-imagining of data frames.

#stringr, for strings.

#forcats, for factors.

install.packages("tidyverse")

#robotstxt - The package provides a simple ‘robotstxt’ class and accompanying methods to parse and check ‘robots.txt’ files.

#Robots.txt files are a way to kindly ask webbots, spiders, crawlers, wanderers and the like to access or not access certain parts of a webpage.

install.packages("robotstxt")

#rvest - Wrappers around the 'xml2' and 'httr' packages to make it easy to download, then manipulate, HTML and XML

install.packages("rvest")

#xml2 - Work with XML files using a simple, consistent interface. Built on top of the 'libxml2' C library.

install.packages("xml2")

#lubridate - Lubridate makes it easier to do the things R does with date-times and possible to do the things R does not.

install.packages("lubridate")


#load packages

library(tidyverse)

library(robotstxt)

library(rvest)

library(selectr)

library(xml2)

library(magrittr)

library(lubridate)

g3_journal = function(year){
  
  #check if a bot has permissions to access page(s)
  #paths for which to check bot's permission, defaults to "/".
  #note that path to a folder should end with a trailing slash ("/").
  
  paths_allowed(
    paths = c(paste("https://www.g3journal.org/content/by/year/",year))
  )
  
  #read_html - read in the content from a .html file.
  load = read_html(paste("https://www.g3journal.org/content/by/year/",year))

  #assign necessary vectors as empty vectors. 
  doi = c()
  title = c()
  author = c()
  affiliates = c()
  corresp_author = c()
  emails = c()
  publish_date = c()
  abstract = c()
  keywords = c()
  full_paper = c()
  
  #used html_nodes to extract pieces out of HTML documents.
  #the '.' indicates the class
  nodes = html_nodes(load, ".hw-issue-meta-data") 
  #url for each month
  month_url = nodes %>% 
    #extract attribute. The HREF is an attribute of the anchor tag, which is also used to identify sections within a document.
    html_attr("href") 
  
  #url for each month as a vector
  indiv_url = c(month_url)
  
  #the number of months articles were published in the given year
  print(paste(length(month_url), " months of ","g3 Genes, Genomes and Genectics journals were published in ",year))
  
  #assign base url
  base_url = "https://www.g3journal.org"
  
  #nested loop - loops through all article for each month of the given year.
  
  for(i in 1:length(indiv_url)){
    
    #individual month link used to open all the journals for a month.
    #the gsub() function  used to replace the strings with input strings or values. 
    journal_all = gsub(" ","",paste(base_url,indiv_url[i]))
    
    #read_html loads page for a specific month
    #used html_nodes to extract pieces out of HTML documents
    #the '.' indicates the class
    nodes1 = html_nodes(read_html(journal_all), ".highwire-cite-linked-title")   
    #list of the journals in a month
    journals_month_list = nodes1 %>% 
      #extract raw underlying text from html
      html_text()
    
    #read_html - read in the content from a .html file.
    #used html_nodes to extract pieces out of HTML documents
    #the '.' indicates the class
    nodes2 = html_nodes(read_html(journal_all), ".highwire-cite-linked-title")   
    #link of the journals in a month
    journals_month_url = nodes2 %>% 
      #extract attribute. The HREF is an attribute of the anchor tag, which is also used to identify sections within a document.
      html_attr("href")  
    
    #read_html - read in the content from a .html file.
    #used html_nodes to extract pieces out of HTML documents
    #the '.' indicates the class
    nodes3 = html_nodes(read_html(journal_all), "#page-title") 
    #month name
    month_name = nodes3 %>%
      #extract raw underlying text from html
      html_text()   
    
    #number of journals in a specific month for the given year
    print(paste("There are a total of ",length(journals_month_list)," articles published for ",month_name))
    
    #loops through each journal and extracts all the details below:
    #Title
    #Authors
    #Author Affiliations
    #Correspondence Author
    #Correspondence Authors Email
    #Publish Date
    #Abstract
    #Keywords
    #Full Paper (Text format).
    for(j in 1:length(journals_month_list)){
      
      #url of journals in each month as a vector
      month_url = c(journals_month_url)
      #the gsub() function  used to replace the strings with input strings or values.
      article_url = gsub(" ","",paste(base_url,month_url[j]))
      
      #load the the page for a specific journal
      #read_html - read in the content from a .html file.
      load_journal_page = read_html(article_url)
      
      #bring in Title of the journal from the html page
      Title = load_journal_page %>% 
        #as.character - attempts to coerce load_journal_page to character type
        as.character() %>% 
        #read_html - read in the content from a .html file.
        read_html()
      
      #used html_nodes to extract pieces out of HTML documents using XPath
      #the '.' indicates the class
      Title = html_nodes(Title, xpath = '//meta[@name="DC.Title"]')
      Title = Title %>%
        #extract attribute - content
        html_attr('content')
      title = c(title,Title[1])
      
      #used html_nodes to extract pieces out of HTML documents
      #the '.' indicates the class
      nodes4 = html_nodes(load_journal_page, ".kwd")   
      #bring in keywords of the journal from the html page
      Keywords_journal = nodes4 %>% 
        #extract raw underlying text from html
        html_text()  
      #the gsub() function  used to replace the strings with input strings or values
      Keywords_journal = gsub(" ",",",paste(Keywords_journal,collapse=" "))
      keywords = c(keywords,Keywords_journal)
      
      #used html_nodes to extract pieces out of HTML documents
      #the '.' indicates the class
      nodes5 = html_nodes(load_journal_page, ".highwire-cite-metadata-doi") 
      #bring in DOI of the journal from the html page
      DOI = nodes5 %>% 
        #extract raw underlying text from html
        html_text()  
      doi = c(doi,DOI[1])
      
      #used html_nodes to extract pieces out of HTML documents
      #the '.' indicates the class
      nodes6 = html_nodes(load_journal_page, ".section.abstract")   
      #bring in the abstract of the journal from the html page
      Abstract = nodes6 %>% 
        #extract raw underlying text from html
        html_text()  
      
      #if/else clause to print article url if abstract is present.
      if(length(Abstract)<1){
        print(article_url)
        #if not present, print "NA"
        abstract = c(abstract,"NA") 
      }else{
        #pprint article url if abstract is present
        abstract = c(abstract,Abstract)
      }
      
      #used html_nodes to extract pieces out of HTML documents
      #the '.' indicates the class
      nodes7 = html_nodes(load_journal_page, ".highwire-citation-authors")  
      #bring in Authors from the html page
      Authors = nodes7 %>% 
        #extract raw underlying text from html
        html_text() 
      author = c(author,Authors[1])

      #used html_nodes to extract pieces out of HTML documents
      #the '.' indicates the class
      nodes8 = html_nodes(load_journal_page, ".highwire-cite-metadata-date") 
      #bring in the publication date from the html page
      Publish_Date = nodes8 %>% 
        #extract raw underlying text from html
        html_text() 
      publish_date = c(publish_date,Publish_Date[1])
      
      #new url for the remaining fields
      #the gsub() function  used to replace the strings with input strings or values.
      url_new = gsub(" ","",paste(article_url,".article-info"))
      #read_html - read in the content from a .html file.
      url_new = read_html(url_new)
      
      #used html_nodes to extract pieces out of HTML documents
      #the '.' indicates the class
      nodes9 = html_nodes(url_new, ".corresp") 
      #bring in corresponding authors from the html page
      Corresp_Author = nodes9 %>% 
        #extract raw underlying text from html
        html_text() 
      corresp_author = c(corresp_author,Corresp_Author[1])
      
      #used html_nodes to extract pieces out of HTML documents
      #the '.' indicates the class
      nodes10 = html_nodes(url_new, ".corresp")
      #bring in the corresponding authors email from html page
      Emails = nodes10 %>% 
        #extract raw underlying text from html
        html_text() 
      #the gsub() function  used to replace the strings with input strings or values.
      email_r = gsub("{at}","@",Emails[1],fixed=TRUE)
      
      #enabling retrieval of the matching substrings
      #extract or replace matched substrings from match data obtained by gregexpr 
      var = c(regmatches(email_r, gregexpr('[^\\s@<>]+@[^\\s@<>]+\\.[^\\s@<>]+', email_r, perl=T))[[1]])
      
      #collapse - an optional character string to separate the results
      Emails = paste(var,collapse=",")
      emails = c(emails,Emails[1])
      
      #used html_nodes to extract pieces out of HTML documents
      #the '.' indicates the class
      nodes11 = html_nodes(url_new, ".aff") 
      #bring in author affiliations from html page
      Affiliates = nodes11 %>% 
        #extract raw underlying text from html
        html_text() 
      affiliates = c(affiliates,paste(Affiliates,collapse="//"))
      
      #bring in url of the full paper from html page
      #the gsub() function  used to replace the strings with input strings or values.
      Full_Paper = gsub(" ","",paste(article_url,".full"))
      full_paper = c(full_paper,Full_Paper)
    }
  }
  #form a Data Frame from the vectors that were assigned earlier
  Journal_File = data.frame(DOI=doi,
                            Title=title,
                            Authors=author,
                            Author_Affiliations=affiliates,
                            Corresponding_Author=corresp_author,
                            Corresponding_Authors_Email=emails,
                            Publication_Date=publish_date,
                            Abstract=abstract,
                            Keywords=keywords,
                            Full_Paper=full_paper)
  
   #write the result into a csv file
  write.csv(Journal_File,"C:/Users/Parvathy/OneDrive/Desktop/Data Analytics with R/G3.csv", row.names = FALSE)
  #write the result into a text file
  write.table(Journal_File,"C:/Users/Parvathy/OneDrive/Desktop/Data Analytics with R/G3.txt", sep = "\t", row.names = FALSE)
}

#enter the year you want to scrape from the journal
g3_journal(2020)

read_output R file

#read the output in csv form
journal_csv = read.csv("C:/Users/Parvathy/OneDrive/Desktop/Data Analytics with R/G3.csv")
#read the output in text form
journal_txt = read.table("C:/Users/Parvathy/OneDrive/Desktop/Data Analytics with R/G3.txt", header = TRUE, sep = "\t")

#displaying first few records
head(journal_txt)
head(journal_csv)
