# Adds new figure from figures folder.

    Code
      cat(fc_pasted)
    Output
      # Figures {#sec-figures}
       
      #| warnings: false 
      #| eval: true
      # load rda
      load(file.path(figures_dir, 'landings_figure.rda'))
      
      # save rda with plot-specific name
      landings_plot_rda <- rda
      
      # remove generic rda object
      rm(rda)
      
      # save figure, caption, and alt text as separate objects
      landings_plot <- landings_plot_rda$figure
      landings_cap <- landings_plot_rda$caption
      landings_alt_text <- landings_plot_rda$alt_text
      ``` 
      
      ```{r} 
      #| label: 'fig-landings'
      #| echo: false 
      #| warning: false 
      #| fig-cap: !expr landings_cap 
      #| fig-alt: !expr landings_alt_text
      landings_plot
      ``` 
      
      {{< pagebreak >}} 
      
      ```{r} 
      #| label: 'fig-abundance_at_age-setup'
      #| warnings: false 
      #| eval: true
      # load rda
      load(file.path(figures_dir, 'abundance_at_age_figure.rda'))
      
      # save rda with plot-specific name
      abundance_at_age_plot_rda <- rda
      
      # remove generic rda object
      rm(rda)
      
      # save figure, caption, and alt text as separate objects
      abundance_at_age_plot <- abundance_at_age_plot_rda$figure
      abundance_at_age_cap <- abundance_at_age_plot_rda$caption
      abundance_at_age_alt_text <- abundance_at_age_plot_rda$alt_text
      ``` 
      
      ```{r} 
      #| label: 'fig-abundance_at_age'
      #| echo: false 
      #| warning: false 
      #| fig-cap: !expr abundance_at_age_cap 
      #| fig-alt: !expr abundance_at_age_alt_text
      abundance_at_age_plot
      ``` 
      
      {{< pagebreak >}} 

