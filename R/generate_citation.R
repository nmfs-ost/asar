generate_citation <- function(
    author = NULL,
    title = NULL,
    year = NULL,
    office = NULL
    ){

  if(office %in% c('NEFSC','SEFSC','NWFSC','SWFSC','PIFSC','AFSC')){
    off_title <- "NOAA Fisheries Science Center"
  }

  office_loc <- read.csv(here::here('inst', 'resources', 'affiliation_info.csv')) |>
    dplyr::filter(affiliation==office)
  # Check
  if(nrow(office_loc)>1){stop("There is more than one office being selected in this funciton. Please review.")}

  loc_city <- office_loc$city
  loc_state <- office_loc$state

  # Author naming convention formatting
  if(length(author)>1){
    author1 <- unlist(strsplit(author[1], split = " "))
    author1 <- paste0(
      ifelse(length(author1)==3, author1[3], author1[2]), ", ",
      substring(author1[1], 1, 1), ".",
      ifelse(length(author1)==3, author1[2], "")
    )
  author_list <- paste0(author1)
    for(i in 2:length(author)){
      auth_extract <- unlist(strsplit(author[i], split = " "))
      auth_extract2 <- paste0(
        ifelse(length(auth_extract)==3, auth_extract[3], auth_extract[2]), ", ",
        substring(auth_extract[1], 1, 1), ".",
        ifelse(length(auth_extract)==3, auth_extract[2], "")
        )
      author_list <- paste0(author_list, ", ", auth_extract2)
    }
  } else {
    author_spl <- unlist(strsplit(author, split = " "))
    author_list <- paste0(
      ifelse(length(author_spl)==3, author_spl[3], author_spl[2]), ", ",
      substring(author_spl[[1]][1], 1, 1), ".",
      ifelse(length(author_spl)==3, author_spl[2], "")
    )
  }

  # Create citation string
  if(office!="SEFSC"){
  cit <- paste0("\n",
                "Please cite this publication as \n",
                author_list, ". ", year, ". ",
                title, ". ", off_title, ", ",
                loc_city, ", ", loc_state, ". "
                )
  } else {
    cit <- paste0("\n",
                  "Please cite this publication as \n",
                  "SEDAR. ", year, ". ", title,
                  "SEDAR, North Charleston SC. XXpp. ",
                  "available online at: http://sedarweb.org/"
                 )
  }

  # Add citation as .qmd to add into template
  return(cit)

}
