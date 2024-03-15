generate_citation <- function(
    authors = NULL,
    title = NULL,
    year = NULL,
    office = NULL
    ){

  if(office %in% c('NEFSC','SEFSC','NWFSC','SWFSC','PIFSC','AFSC')){
    off_title <- "NOAA Fisheries Science Center"
  }

  # Author naming convention formatting
  author2 <- c()
  for(i in 1:length(author)){
    auth_extract <- strsplit(author[1], split = " ")
    auth_extract2 <- paste0(
      tail(auth_extract[[1]]), ",",
      substring(auth_extract[[1]][1], 1, 1), ".",
      ifelse(length(auth_extract[[1]])==3, substring(auth_extract[[1]][2], 1, 1), NA), "."
    )
  }

  # Create citation string
  cit <- paste0("\n",
                "Please cite this publication as \n",
                )

  # Add citation as .qmd to add into template

}


Johnson, K.F., C.R. Wetzel, N. Tolimieri. 2023. Status of sablefish
(Anoplopoma fimbria) along the U.S. West Coast in 2023. Pacific Fishery
Management Council, Portland, Oregon. 145p.
