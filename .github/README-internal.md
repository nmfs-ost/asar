# GitHub Workflows and Templates

This folder contains GitHub workflows that perform actions that facilitate and semi-automate the development process. For example, these actions set up checklists that help pull request authors verify that they've completed necessary steps before merging their code into `asar` (`pull_request_template.md`), checks the spelling of words in the repository (`call-spell-check.yml`), calculates code coverage summaries (`call-calc-cov-summaries.yml`), and more. Many of these actions were made using [`ghactions4r`](https://github.com/nmfs-ost/ghactions4r), a collection of reusable workflows that are useful for R packages.


* the pull request template (`pull_request_template.md`)
* checks for the repository and code (e.g., code coverage check, code styling and adjustment, CMD check, spell check) (`workflows` folder)
* issue templates which translate to pre-made 
templates that are shown when a user opens an issue in the repository (`ISSUE_TEMPLATE` folder)