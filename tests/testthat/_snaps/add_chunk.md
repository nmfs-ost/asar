# add_chunk() generates correct R Markdown chunk

    Code
      cat(add_chunk("plot(cars$speed, cars$distance)"))
    Output
      ```{r} 
      #| echo: false 
      #| warning: false 
      #| eval: true 
      plot(cars$speed, cars$distance)
      ``` 

---

    Code
      cat(add_chunk("plot(cars$speed, cars$distance)", echo = "true"))
    Output
      ```{r} 
      #| echo: true 
      #| warning: false 
      #| eval: true 
      plot(cars$speed, cars$distance)
      ``` 

