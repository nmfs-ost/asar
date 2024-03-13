report_format <- function(
    type = NULL,
    office = NULL){
  # This may need to be changed bc not every region has different format - maybe just different format based on report type (OA vs RT)
  # Add report formatting based on report type (operational/research)
  if(type=="OA" | type=="UP" | type=="MT"){
    # Add report formatting based on region
    if(office=="AFSC"){

    } else if(office=="NEFSC"){

    } else if(office=="NWFSC"){
      template <- paste0(
        "output: ",
        "  ", "sa"
      )
    } else if(office=="PIFSC"){

    } else if(office=="SEFSC"){

    } else if(office=="SWFSC"){

    } else {
      print("office (FSC) is not defined. Please define which office you are associated with.")
    }
  } else if (type=="RT" | type=="FULL"){
    # Add report formatting based on region
    if(office=="AFSC"){

    } else if(office=="NEFSC"){

    } else if(office=="NWFSC"){

    } else if(office=="PIFSC"){

    } else if(office=="SEFSC"){

    } else if(office=="SWFSC"){

    } else {
      print("office (FSC) is not defined. Please define which office you are associated with.")
    }
  } else {
    print("Type of assessment report is not defined")
  }
}
