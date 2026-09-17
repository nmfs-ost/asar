# Adds new figure from figures folder.

    Code
      cat(fc_pasted)
    Output
      # Figures {#sec-figures}
       
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

