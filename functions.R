source_python("functions.py")

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

get.level <- function(x) x[[1]]

build_toc <- function(filename) {
  toc_py <- py$get_bookmarks(filename)
  toc_py <- toc_py[lapply(toc_py, get.level) != 1]
  toc_r <- generate_toc(filename)
  #Replace title with R title
  for (i in 1:length(toc_py)) toc_py[[i]][[2]] <-  toc_r[[i]]
  return(toc_py)
}



create.doc.lists <- function(raw.data, toc) {
  #Get pages under each item
  for (i in 1:length(toc)) {
    if (i != length(toc)) {
      #Get all the pages between sections
      page_list <- head(toc[[i]][[3]]:toc[[i+1]][[3]], -1)
      toc[[i]][["pages"]] <-  raw.data[page_list]
    }
    else {
      page_list <- (toc[[i]][[3]]:length(raw.data))
      toc[[i]][["pages"]] <-  raw.data[page_list]
    }
  }
  
  #Get appendices under the correct date
  for (i in 1:length(toc)) {
    if (toc[[i]][[1]] == 3) {
      #Look backwards to find parent
      parent_index <- i-1
      while (toc[[parent_index]][[1]] != 2) {
        parent_index <- parent_index - 1
      }
      #Made Appendix list if doesn't exist
      if (!("appendices" %in% names(toc[[parent_index]]))) {
        toc[[parent_index]][["appendicies"]] <- list(A = toc[[i]][["pages"]])
      }
      else{
        letter <- str_sub(toc[[i]][[2]], -1)
        toc[[parent_index]][["appendices"]][[letter]] <- toc[[i]][["pages"]]
      }
    }
  }
  
  #Delete appendix entries 
  toc <- toc[lapply(toc, get.level) != 3]
  
  #Delete level number
  toc <- lapply(toc, function(x) (x <- x[-1]))
  
  #Rename sublist items
  toc <- lapply(toc, function(x) {names(x)[c(1,2)] <- c("date", "page.number"); x})
  
  #Rename list items
  names(toc) <- lapply(toc, function(x) x[["date"]])
  
  return(toc)
}
