
is.withdrawal <- function(text) {
  return(str_detect(text, "DOCUMENT WITHDRAWAL RECORD"))
}

is.diary <- function(text) {
  return(str_detect(text, "DAILY DIARY"))
}

is.same.date <- function(date, text, debug = FALSE, year) {
  regex.pattern <- "(January|February|March|April|May|June|July|August|September|October|November|December).+(\\d{1,2}),\\s+(\\d{3,4})"
  date = anydate(date)
  date_extract <- str_extract(text, regex(regex.pattern, ignore_case = TRUE, multiline = TRUE))
  #Add dropped 9
  if (!is.na(date_extract)) {
    if (str_sub(date_extract,-1,-1) == '6') {
      date_extract <- paste0(date_extract, '9')
    }
  }
  date_text <- anydate(date_extract)
  if (is.na(date_text) & !is.na(date_extract)) {
    #locate end of month and first number
    month_end <- str_locate(date_extract, regex("(January|February|March|April|May|June|July|August|September|October|November|December)", ignore_case = TRUE))[2]
    first_digit <- str_locate(date_extract, regex("[:digit:]", ignore_case = TRUE))[1]
    str_sub(date_extract, month_end+1, first_digit-1) <- " "
    date_text <- anydate(date_extract)
  }
  if (debug == TRUE) {
    return(list(date, date_text, date_extract))
  }
  if (is.na(date_text)) {
    return(FALSE)
  }
  return(date == date_text)
}

generate_toc <- function(filename) {
  
  toc <- as.list(unlist(pdf_toc(filename)$children))
  
  tmp <- character()
  for(i in 1:length(toc)) {
    
    #Store for combine with Document Withhold request
    if (names(toc)[i] != 'children.children.title' & toc[[i]] != 'Document Withdrawal Record') {
      tmp <- toc[[i]]
    }
    else {
      toc[[i]] <- paste(tmp, toc[[i]])
    }
  }
  #Delete section header
  toc <- (toc[names(toc) != 'title'])
  
  return(toc)
}


create.doc.lists <- function(raw.data, toc) {
  doc.list <- vector("list", length = length(toc))
  page_count <- 1
  for (i in 1:500) {
    if (!str_detect(toc[[i]], "Appendix") & !str_detect(toc[[i]], "Withdrawal")) {
      print(paste(i, page_count, toc[[i]],
                              is.same.date(toc[[i]], raw.data[page_count], debug = TRUE)[[2]], 
                              is.same.date(toc[[i]], raw.data[page_count], debug = TRUE)[[3]]))
    }
    #Catch doc withdrawals
    if (str_detect(toc[[i]], "Document Withdrawal Record")) {
      doc.list[[i]] <- list(name = toc[[i]], text = raw.data[page_count])
      page_count <- page_count + 1
    }
    #diary pages
    else if (!str_detect(toc[[i]], "Appendix")){
      doc.list[[i]] <- list(name = toc[[i]], pages = list(raw.data[page_count]))
      page_count <- page_count + 1
      while (is.diary(raw.data[page_count]) & is.same.date(toc[[i]], raw.data[page_count])) {
        #print(paste(i, page_count, is.same.date(toc[[i]], raw.data[(page_count)])))
        append(doc.list[[i]]$pages, list(raw.data[page_count]))
        page_count <- page_count + 1
      }
    }
    #Create sub-list if appendix A and add all appendix pages
    else if (str_detect(toc[[i]], "Appendix A")) {
      doc.list[[i - 1]]$appendicies <- list(raw.data[page_count])
      page_count <- page_count + 1
      last_appendix <- i
      while (!is.diary(raw.data[page_count]) & !is.withdrawal(raw.data[page_count])) {
        append(doc.list[[i]]$appendicies, list(raw.data[page_count]))
        page_count <- page_count + 1
      }
    }
    #append appendix
    else {
      append(doc.list[[last_appendix]]$appendicies, list(raw.data[page_count]))
      page_count <- page_count + 1
    }
  }
  
  return(doc.list)
}
