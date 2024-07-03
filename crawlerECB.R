library(rvest)
library(httr)
library(xml2)
library(dplyr)
library(rmarkdown)
library(stringr)
library(pdftools)
library(knitr)
#################################################
# Enter things here
# url to the last meeting you want to scipt, needs to be precise
url = "https://www.ecb.europa.eu/press/pressconf/press_conference/html/index.en.html?date=2023-05-04"

#last meeting date, needs to be precise
previous_date = "2023-05-04" 

#The first meeting date, no need to be precise, meetings with a date before or equal to this date are collected
end_date = as.Date("2017-01-01") 
#################################################



list_of_text = list() # Will be filled with text retrieved
list_of_date = list(previous_date) # Will be filled with dates of meetings we processed

while (as.Date(previous_date) >= end_date) {
  web_page = read_html(url) # Reading webpage
  
  # Retriving the date to the previous meeting for next iteration
  buttons = web_page %>% html_elements("button") # retrive all buttons
  previous_button = xml_attrs(buttons[[3]])[["onclick"]] #retrive the "previous meeting" button
  previous_date <- sub('.*\\?date=(\\d{4}-\\d{2}-\\d{2}).*',
                       '\\1',
                       previous_button) #retrive the previous date
  
  #Go to the press conference page, which href is contained on the home page of the current meeting
  links_pressconf <- web_page %>% # Finding the links to the page that contains ECB meeting minutes
    html_nodes("a[href*='/press/press_conference/']")
  link_article = paste0("https://www.ecb.europa.eu", as.character(xml_attrs(links_pressconf[[5]]))) #link to the article
  article_page = read_html(link_article) # Reading pressconference page
  
  # Extracting text
  webtxt <- article_page %>%
    # extract paragraphs
    html_nodes("p") %>%
    # extract text
    html_text()
  
  
  # Trimming copyright disclaimer
  webtxt <- head(webtxt, length(webtxt) - 6)
  
  # Appending text and date to the list prepared
  list_of_text = list_of_text %>%
    append(list(webtxt))
  list_of_date = list_of_date %>%
    append(previous_date)
  
  # Go to the next link
  url = paste0(
    "https://www.ecb.europa.eu/press/pressconf/press_conference/html/index.en.html?date=",
    previous_date
  )
}

# Use the list of dates to name the data collected
names(list_of_text) = list_of_date[-length(list_of_date)] %>%
  unlist() %>%
  as.Date() %>%
  format("%d-%m-%y")



##############################################################################################
###### This will output the text into txt or pdf. By default to the working directory ########
##############################################################################################



### PART I: OUTPUT TO TXT ####
for (i in seq_along(list_of_text)) {
  # Example list of characters
  my_list <- list_of_text[[i]]
  
  # Concatenate the list elements with a newline character between each paragraph
  text <- paste(my_list, collapse = "\n\n")
  
  # Write the text to a file as separate paragraphs
  writeLines(text, paste0(names(list_of_text)[i], ".txt"))
}



### PART II: OUTPUT TO PDF ####
# Get a list of all the text files in your working directory
txt_files <- list.files(pattern = "*.txt")

# Loop over each text file and render it to PDF using rmarkdown
for (i in seq_along(txt_files)) {
  # Get the filename without the extension
  file_name <- tools::file_path_sans_ext(txt_files[i])
  
  # Render the file to PDF
  rmarkdown::render(txt_files[i],
                    output_format = "pdf_document",
                    output_file = paste0(file_name, ".pdf"))
}




