# Inst

This folder contains various files which support the creation of materials for running functions in {asar}.

* WORDLIST contains a list of words that are used in the spell check workflow. It is a plain text file with one word per line. The words in this list are considered correct and will not be flagged as spelling errors during the spell check process. Words can be added or removed from this list by running `spelling::update_wordlist()`.

## Sub-folders

* The `extdata` sub-folder contains an example of an SS3 report file that can be input into `convert_output()`.
* The `glossary` sub-folder contains the materials used to create the {asar} glossary (`report_glossary.tex`).
* The `resources` sub-folder contains files used in the development of {asar} reports. This includes:
  * author affiliation information (`affiliation_info.csv`)
  * citations for use in the report bibliography (`citations.bib`)
  * citation formatting file in the style of the Canadian Journal of Fisheries and Aquatic Sciences (CJFAS) (`cjfas.csl`)
  * LaTeX partials used to format the Quarto-based report (`formatting_files` folder)
  * NOAA logo placed on report title page (`NOAA_Transparent_Logo.png`)
  * `preamble` R file used to extract key quantities from model results file used in asar report (`preamble.R`)
  * Images of species used for asar report cover pages (`spp_img` folder)
  * US Dept of Commerce logo placed on report title page (`us_doc_logo.png`)
* The `templates` sub-folder contains the child documents used for each report "type": Northeast management track (nemt); Pacific Fisheries Marine Council (pfmc); Stock Assessment and Fishery Evaluation (safe); standard report format (skeleton)
