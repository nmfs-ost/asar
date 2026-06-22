# Adds new table from tables folder.

    Code
      cat(fc_pasted)
    Output
      # Tables {#sec-tables}
       
      ```{r} 
      #| label: 'set-rda-dir-tbls'
      #| echo: false 
      #| warning: false 
      #| include: false
      library(gt)
      tables_dir <- fs::path('C:/Users/samantha.schiano.NMFS/Documents/GitHub/asar/tests/testthat', 'tables')
      ``` 
      
      ```{r} 
      #| label: 'tab-landings-setup'
      #| warnings: false 
      #| eval: true
      # load rda
      load(file.path(tables_dir, 'landings_table.rda'))
      
      # save rda with table-specific name
      landings_table_rda <- rda
      
      # save table and caption as separate objects
      landings_table <- landings_table_rda$table
      landings_cap <- landings_table_rda$caption
      ``` 
      
      ::: {.landscape}
      
      ```{r} 
      #| label: 'tbl-landings1'
      #| echo: false 
      #| tbl-cap: !expr paste0(landings_cap, ' (1 of 6)') 
      #| tbl-pos: 't'
      # plot table 1
      landings_table |>
        gt::tab_options(
          table.width = pct(100),
          table.layout = 'auto'
        ) |>
        gt::cols_width(
          everything() ~ pct(20)
        ) |> 
       asar::gt_split(row_every_n = 28) |>
       gt::grp_pull(1)
      
      ``` 
      :::
      ::: {.landscape}
      
      ```{r} 
      #| label: 'tbl-landings2'
      #| echo: false 
      #| tbl-cap: !expr paste0(landings_cap, ' (2 of 6)') 
      #| tbl-pos: 't'
      # plot table 2
      landings_table |>
        gt::tab_options(
          table.width = pct(100),
          table.layout = 'auto'
        ) |>
        gt::cols_width(
          everything() ~ pct(20)
        ) |> 
       asar::gt_split(row_every_n = 28) |>
       gt::grp_pull(2)
      
      ``` 
      :::
      ::: {.landscape}
      
      ```{r} 
      #| label: 'tbl-landings3'
      #| echo: false 
      #| tbl-cap: !expr paste0(landings_cap, ' (3 of 6)') 
      #| tbl-pos: 't'
      # plot table 3
      landings_table |>
        gt::tab_options(
          table.width = pct(100),
          table.layout = 'auto'
        ) |>
        gt::cols_width(
          everything() ~ pct(20)
        ) |> 
       asar::gt_split(row_every_n = 28) |>
       gt::grp_pull(3)
      
      ``` 
      :::
      ::: {.landscape}
      
      ```{r} 
      #| label: 'tbl-landings4'
      #| echo: false 
      #| tbl-cap: !expr paste0(landings_cap, ' (4 of 6)') 
      #| tbl-pos: 't'
      # plot table 4
      landings_table |>
        gt::tab_options(
          table.width = pct(100),
          table.layout = 'auto'
        ) |>
        gt::cols_width(
          everything() ~ pct(20)
        ) |> 
       asar::gt_split(row_every_n = 28) |>
       gt::grp_pull(4)
      
      ``` 
      :::
      ::: {.landscape}
      
      ```{r} 
      #| label: 'tbl-landings5'
      #| echo: false 
      #| tbl-cap: !expr paste0(landings_cap, ' (5 of 6)') 
      #| tbl-pos: 't'
      # plot table 5
      landings_table |>
        gt::tab_options(
          table.width = pct(100),
          table.layout = 'auto'
        ) |>
        gt::cols_width(
          everything() ~ pct(20)
        ) |> 
       asar::gt_split(row_every_n = 28) |>
       gt::grp_pull(5)
      
      ``` 
      :::
      ::: {.landscape}
      
      ```{r} 
      #| label: 'tbl-landings6'
      #| echo: false 
      #| tbl-cap: !expr paste0(landings_cap, ' (6 of 6)') 
      #| tbl-pos: 't'
      # plot table 6
      landings_table |>
        gt::tab_options(
          table.width = pct(100),
          table.layout = 'auto'
        ) |>
        gt::cols_width(
          everything() ~ pct(20)
        ) |> 
       asar::gt_split(row_every_n = 28) |>
       gt::grp_pull(6)
      
      ``` 
      :::
      {{< pagebreak >}} 
      
      ```{r} 
      #| label: 'tab-land-setup'
      #| warnings: false 
      #| eval: true
      # load rda
      load(file.path(tables_dir, 'land_table.rda'))
      
      # save rda with table-specific name
      land_table_rda <- rda
      
      # save table and caption as separate objects
      land_table <- land_table_rda$table
      land_cap <- land_table_rda$caption
      ``` 
      
      ::: {.landscape}
      
      ```{r} 
      #| label: 'tbl-land1'
      #| echo: false 
      #| tbl-cap: !expr paste0(land_cap, ' (1 of 6)') 
      #| tbl-pos: 't'
      # plot table 1
      land_table |>
        gt::tab_options(
          table.width = pct(100),
          table.layout = 'auto'
        ) |>
        gt::cols_width(
          everything() ~ pct(20)
        ) |> 
       asar::gt_split(row_every_n = 28) |>
       gt::grp_pull(1)
      
      ``` 
      :::
      ::: {.landscape}
      
      ```{r} 
      #| label: 'tbl-land2'
      #| echo: false 
      #| tbl-cap: !expr paste0(land_cap, ' (2 of 6)') 
      #| tbl-pos: 't'
      # plot table 2
      land_table |>
        gt::tab_options(
          table.width = pct(100),
          table.layout = 'auto'
        ) |>
        gt::cols_width(
          everything() ~ pct(20)
        ) |> 
       asar::gt_split(row_every_n = 28) |>
       gt::grp_pull(2)
      
      ``` 
      :::
      ::: {.landscape}
      
      ```{r} 
      #| label: 'tbl-land3'
      #| echo: false 
      #| tbl-cap: !expr paste0(land_cap, ' (3 of 6)') 
      #| tbl-pos: 't'
      # plot table 3
      land_table |>
        gt::tab_options(
          table.width = pct(100),
          table.layout = 'auto'
        ) |>
        gt::cols_width(
          everything() ~ pct(20)
        ) |> 
       asar::gt_split(row_every_n = 28) |>
       gt::grp_pull(3)
      
      ``` 
      :::
      ::: {.landscape}
      
      ```{r} 
      #| label: 'tbl-land4'
      #| echo: false 
      #| tbl-cap: !expr paste0(land_cap, ' (4 of 6)') 
      #| tbl-pos: 't'
      # plot table 4
      land_table |>
        gt::tab_options(
          table.width = pct(100),
          table.layout = 'auto'
        ) |>
        gt::cols_width(
          everything() ~ pct(20)
        ) |> 
       asar::gt_split(row_every_n = 28) |>
       gt::grp_pull(4)
      
      ``` 
      :::
      ::: {.landscape}
      
      ```{r} 
      #| label: 'tbl-land5'
      #| echo: false 
      #| tbl-cap: !expr paste0(land_cap, ' (5 of 6)') 
      #| tbl-pos: 't'
      # plot table 5
      land_table |>
        gt::tab_options(
          table.width = pct(100),
          table.layout = 'auto'
        ) |>
        gt::cols_width(
          everything() ~ pct(20)
        ) |> 
       asar::gt_split(row_every_n = 28) |>
       gt::grp_pull(5)
      
      ``` 
      :::
      ::: {.landscape}
      
      ```{r} 
      #| label: 'tbl-land6'
      #| echo: false 
      #| tbl-cap: !expr paste0(land_cap, ' (6 of 6)') 
      #| tbl-pos: 't'
      # plot table 6
      land_table |>
        gt::tab_options(
          table.width = pct(100),
          table.layout = 'auto'
        ) |>
        gt::cols_width(
          everything() ~ pct(20)
        ) |> 
       asar::gt_split(row_every_n = 28) |>
       gt::grp_pull(6)
      
      ``` 
      :::
      {{< pagebreak >}} 

