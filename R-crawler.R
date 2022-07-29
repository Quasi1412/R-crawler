library(rvest)
library(stringr)
library(openxlsx)

#Extract links and published year for each article
get_links <- function(main_url,page_pattern,maximum_pages){
    link <- c()
    published_year <-c()
    links_df <-data.frame(matrix(ncol=2,nrow=0,dimnames = list(NULL,c('links','published_year'))))
    
    for(i in 1:maximum_pages){
        page_url <- paste0(main_url,page_pattern,i)
        page_html <- read_html(page_url)
        link <- page_html%>%html_nodes(xpath = "//h3[@class = 'c-listing__title']/a")%>%
                html_attr('href')%>%str_replace_all('/articles','')%>%append(link)
        
        published_year <- page_html%>%html_nodes(xpath="//span[@itemprop='datePublished']")%>%
                          html_text()%>%str_replace_all(" ","-")%>%as.Date(format='%d-%b-%Y')%>%substr(0,4)%>%append(published_year)
    }

    for(i in 1:length(link)){
        links_df[i,] <- c(link[i],published_year[i])
    }
    return(links_df)
}


#function to replace character(0) to NA
check_empty<- function(x){
  if(length(x)==0){
    x='NA'
  }
  return(x)
}

crawler <- function(year){
  
  main_url <- 'https://almob.biomedcentral.com/articles'
  pagenation_link <- '?searchType=journalSearch&sort=PubDate&page='
  
  main_html <- read_html(main_url)
  
  #identify the max pages
  cur_page <- main_html%>%html_node(xpath="//p[@class='u-text-sm u-reset-margin']")%>%html_text()%>%str_split(' ')%>%unlist
  max_page <- as.numeric(cur_page[4])
  
  all_links <- get_links(main_url, pagenation_link,max_page)
  
  #Filter requested year and after
  filtered_links <- all_links[all_links$published_year >= year,]
  
  #create dataframe
  colnames<-c('doi','title','authors','affiliations','corresponding_author','corresponding_author_email','publication_date','abstract','keywords','full_paper')
  df<-data.frame(matrix(ncol=length(colnames),nrow=0, dimnames=list(NULL, colnames)))
  
  #Iterate through all the filtered links and extract data
  for (i in 1:length(filtered_links$links)){
    
    url <- paste0(main_url,filtered_links$links[i])
  
    article_html <- read_html(url)
  
    doi <- article_html%>%html_node(xpath = "//*[@class = 'c-bibliographic-information__list-item c-bibliographic-information__list-item--doi']//
                                  span[@class = 'c-bibliographic-information__value']")%>%html_text()%>%check_empty()
  
    title <- article_html%>%html_node('h1')%>%html_text()
  
    authors <- article_html%>%html_nodes(xpath = "//li[@class = 'c-article-author-list__item']/a")%>%html_text()%>%paste(collapse = ', ')
  
    affiliations <- article_html%>%html_nodes(xpath = "//*[@id='author-information-content']/ol")%>%html_text()%>%unique()%>%check_empty()%>%paste(collapse = ', ')
  
    corresponding_author <- article_html%>%html_nodes(xpath = "//a[@id='corresp-c1']")%>%html_text()%>%check_empty()
  
    corresponding_author_email <- article_html%>%html_nodes(xpath = "//a[@id='corresp-c1']")%>%html_attr('href')%>%str_replace('mailto:',"")%>%check_empty()
  
    publication_date <- article_html%>%html_nodes(xpath = "//li[@class='c-article-identifiers__item']/a/time")%>%html_text()%>%check_empty()
  
    abstract <- article_html%>%html_nodes(xpath = "//*[@id='Abs1-content']")%>%html_text()%>%check_empty()
  
    keywords <- article_html%>%html_nodes(xpath = "//*[@class = 'c-article-subject-list__subject']")%>%html_text()%>%check_empty()%>%paste(collapse = ', ')
    
    content <- article_html%>%html_node(xpath="//*[@id='main-content']/main/article")%>%html_text()%>%str_remove_all("\n")%>%str_squish()%>%paste(collapse = ',')
  
    df[nrow(df)+1,] <- list(doi, title, authors, affiliations, corresponding_author, corresponding_author_email, publication_date,
                          abstract, keywords,content)
  
  }
  #wb <- createWorkbook()
  #addWorksheet(wb, "export")
  #writeDataTable(wb, 1, df, tableStyle = "TableStyleLight16")
  #export to excel file
  #saveWorkbook(wb, "export.xlsx", overwrite = TRUE)
  return(df)
}

year <- as.integer(readline(prompt = "Enter Year: "))

output<-crawler(year)
write.csv(output, 'output.csv', row.names = FALSE)



