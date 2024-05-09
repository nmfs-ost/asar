test_that("Can trace template files from package", {
  path <- system.file("templates", "skeleton", package = "ASAR")
  base_temp_files <- c(
    "abstract.qmd",
    "acknowledgements.qmd",
    "appendix.qmd",
    "assessment_glossaries.tex",
    "data.qmd",
    "discussion.qmd",
    "executive_summary.qmd",
    "figures.qmd",
    "in-header.tex",
    "introduction.qmd",
    "model.qmd",
    "proj_table.qmd",
    "references.qmd",
    "refpt_table.qmd",
    "results.qmd",
    "tables.qmd",
    "title.tex"
  )
  expect_equal(list.files(path), base_temp_files)
})

# Test it creates the current full template before cat() and save
# This will need to be update

# test_that("Create accurrate skeleton script."{
#   expect_snapshot(create_template(
#     new_template = TRUE,
#     format = "docx",
#     office = "SWFSC",
#     species = "Pacific sardine",
#     spp_latin = "Sardinops sagax",
#     year = 2020,
#     type = "RT",
#     author = c("Peter T. Kuriyama", "Caitlin Allen Akselrud", "Juan P. Zwolinski", "Kevin T. Hill"),
#     include_affiliation = TRUE,
#     simple_affiliation = TRUE,
#     param_names = c("NBC","PNW"),
#     param_values = c("northern Baja California", "Pacific Northwest"),
#     resdir = "C:/Users/samantha.schiano/Documents/Stock Assessment Workflow/Regional FSC Resources/SWFSC/2024_sardine/model/modb8i_base",
#     model_results = c("Report.sso","ss_summary.sso","CumReport.sso"),
#     model = "SS",
#     custom = TRUE,
#     custom_sections = c("introduction","data","model","acknowledgements","tables","figures","appendix","references"),
#     new_section = c("Harvest Control Rules", "Regional Management Considerations","Research and Data Needs"),
#     section_location = c("after-model", "after-model", "after-model")
#   ))
#   test_skeleton <- create_template(
#     new_template = TRUE,
#     format = "docx",
#     office = "SWFSC",
#     species = "Pacific sardine",
#     spp_latin = "Sardinops sagax",
#     year = 2024,
#     type = "RT",
#     author = c("Peter T. Kuriyama", "Caitlin Allen Akselrud", "Juan P. Zwolinski", "Kevin T. Hill"),
#     include_affiliation = TRUE,
#     simple_affiliation = TRUE,
#     param_names = c("NBC","PNW"),
#     param_values = c("northern Baja California", "Pacific Northwest"),
#     resdir = "C:/Users/samantha.schiano/Documents/Stock Assessment Workflow/Regional FSC Resources/SWFSC/2024_sardine/model/modb8i_base",
#     model_results = c("Report.sso","ss_summary.sso","CumReport.sso"),
#     model = "SS",
#     custom = TRUE,
#     custom_sections = c("introduction","data","model","acknowledgements","tables","figures","appendix","references"),
#     new_section = c("Harvest Control Rules", "Regional Management Considerations","Research and Data Needs"),
#     section_location = c("after-model", "after-model", "after-model")
#   )
#
#   verify_skeleton <- "---\ntitle: 'Status of the Pacific sardine stock along the U.S. West Coast in 2020'\nauthor:\n  - name: 'Caitlin Allen Akselrud'\n    affiliations: 'Southwest Fisheries Science Center'\n  - name: 'Kevin T. Hill'\n    affiliations: 'Southwest Fisheries Science Center'\n  - name: 'Peter T. Kuriyama'\n    affiliations: 'Southwest Fisheries Science Center'\n  - name: 'Juan P. Zwolinski'\n    affiliations: 'Southwest Fisheries Science Center'\ndate: today\nformat: \n  docx: \n    toc: true \n    keep-tex: true \n    template-partials: \n       - title.tex \n    include-in-header: \n       - in-header.tex \nparams:\n   office: 'SWFSC'\n   species: 'Pacific sardine'\n   spp_latin: 'Sardinops sagax'\n   NBC: 'northern Baja California'\n   PNW: 'Pacific Northwest'\n---\n```{r} \n#| label: 'model_output'\n#| echo: false \n#| warning: false \n#| eval: false \nconvert_output(output.file = c('Report.sso', 'ss_summary.sso', 'CumReport.sso'), model = 'SS', outdir = 'C:/Users/samantha.schiano/Documents/Stock Assessment Workflow/Regional FSC Resources/SWFSC/2024_sardine/model/modb8i_base')\n``` \n\n{{< pagebreak >}} \n\nPlease cite this publication as \n\nKuriyama, P.T., C.Allen Akselrud, J.P. Zwolinski, K.T. Hill. 2020. Status of the Pacific sardine stock along the U.S. West Coast in 2020. NOAA Fisheries Science Center, La Jolla, CA. \n\n {{< pagebreak >}} \n\n```{r, results='asis'}\n#| label: '02_introduction'\n#| eval: true\n#| echo: false\n#| warning: false\na <- knitr::knit_child('02_introduction.qmd', quiet = TRUE)\ncat(a, sep = '\\n')\n```\n\n {{< pagebreak >}} \n\n```{r, results='asis'}\n#| label: '03_data'\n#| eval: true\n#| echo: false\n#| warning: false\na <- knitr::knit_child('03_data.qmd', quiet = TRUE)\ncat(a, sep = '\\n')\n```\n\n {{< pagebreak >}} \n\n```{r, results='asis'}\n#| label: '04_model'\n#| eval: true\n#| echo: false\n#| warning: false\na <- knitr::knit_child('04_model.qmd', quiet = TRUE)\ncat(a, sep = '\\n')\n```\n\n {{< pagebreak >}} \n\n```{r, results='asis'}\n#| label: 'Harvest_Control_Rules'\n#| eval: true\n#| echo: false\n#| warning: false\na <- knitr::knit_child('Harvest_Control_Rules.qmd', quiet = TRUE)\ncat(a, sep = '\\n')\n```\n\n {{< pagebreak >}} \n\n```{r, results='asis'}\n#| label: 'Regional_Management_Considerations'\n#| eval: true\n#| echo: false\n#| warning: false\na <- knitr::knit_child('Regional_Management_Considerations.qmd', quiet = TRUE)\ncat(a, sep = '\\n')\n```\n\n {{< pagebreak >}} \n\n```{r, results='asis'}\n#| label: 'Research_and_Data_Needs'\n#| eval: true\n#| echo: false\n#| warning: false\na <- knitr::knit_child('Research_and_Data_Needs.qmd', quiet = TRUE)\ncat(a, sep = '\\n')\n```\n\n {{< pagebreak >}} \n\n```{r, results='asis'}\n#| label: '07_acknowledgements'\n#| eval: true\n#| echo: false\n#| warning: false\na <- knitr::knit_child('07_acknowledgements.qmd', quiet = TRUE)\ncat(a, sep = '\\n')\n```\n\n {{< pagebreak >}} \n\n```{r, results='asis'}\n#| label: '09_tables'\n#| eval: true\n#| echo: false\n#| warning: false\na <- knitr::knit_child('09_tables.qmd', quiet = TRUE)\ncat(a, sep = '\\n')\n```\n\n {{< pagebreak >}} \n\n```{r, results='asis'}\n#| label: '10_figures'\n#| eval: true\n#| echo: false\n#| warning: false\na <- knitr::knit_child('10_figures.qmd', quiet = TRUE)\ncat(a, sep = '\\n')\n```\n\n {{< pagebreak >}} \n\n```{r, results='asis'}\n#| label: '11_appendix'\n#| eval: true\n#| echo: false\n#| warning: false\na <- knitr::knit_child('11_appendix.qmd', quiet = TRUE)\ncat(a, sep = '\\n')\n```\n\n {{< pagebreak >}} \n\n```{r, results='asis'}\n#| label: '08_references'\n#| eval: true\n#| echo: false\n#| warning: false\na <- knitr::knit_child('08_references.qmd', quiet = TRUE)\ncat(a, sep = '\\n')\n```\n"
#
#   expect_equal(cat())
#
# })
