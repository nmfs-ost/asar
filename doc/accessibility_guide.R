## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------

## ----warning=FALSE, eval = TRUE, fig.align='center', fig.cap= "Tree circumference and age for 5 orange trees.", fig.alt="A line graph showing how tree circumference increases with age for a set of 5 orange trees. Age, shown on the x axis, is measured in days since 1968/12/31 and spans from 118-1582 days. Circumference, shown on the y axis, spans from 30-214 mm. All trees showed an increasing trend of trunk circumference with age, with each tree starting with a circumference of 30-33 mm at age 0 and ending with a circumference of 140-216 mm at age 1582. At age 1582, the tree with the largest circumference was tree 4, followed by trees 2, 5, 1, and 3."----

library(ggplot2)

orange <- as.data.frame(Orange)
orange <- orange |>
  dplyr::mutate(Tree = base::factor(Tree,
                                    levels = c(1,2,3,4,5))) |>
  dplyr::rename(Age = age,
                Circumference = circumference)

ggplot2::ggplot(data = orange,
                aes(x = Age,
                    y = Circumference,
                    color = Tree)) +
ggplot2::geom_line(size = 1) +
ggplot2::geom_point(size = 2) +
ggplot2::scale_color_viridis_d() +
ggplot2::xlim(0, NA) +
ggplot2::ylim(0, NA) +
ggplot2::theme_bw() +
  labs(x = "Age (days since 1968/12/31)",
       y = "Orange Tree Circumference (mm)")

