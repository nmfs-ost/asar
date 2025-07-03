# add_chunk() generates correct R Markdown chunk

    Code
      cat(add_chunk("plot(cars$speed, cars$distance)"))
    Output
      ```{r} 
      #| echo: false 
      #| warnings: false 
      #| eval: true
      plot(cars$speed, cars$distance)
      ``` 

---

    Code
      cat(add_chunk("plot(cars$speed, cars$distance)", chunk_option = c("echo:true",
        "warning: false", "eval: true")))
    Output
      ```{r} 
      #| echo:true 
      #| warning: false 
      #| eval: true
      plot(cars$speed, cars$distance)
      ``` 

