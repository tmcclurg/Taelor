# geom_eqq is a ggplot layer to generate an empirical q-q plot of two variables.
# It may be layered onto an existing ggplot plot as you would any other geom.
# Note that the StateEQQ object must be generated before the function definition 
# for geo,_eqq can be run.
library(tidyverse)
StatEQQ <- ggproto("StatEQQ", Stat,
                   required_aes = c("x", "y"),
                   compute_group = function(data, scales) {
                     n = nrow(data)
                     p = (1:n - .5) / n
                     q = data.frame(x = quantile(data$x, p),
                                    y = quantile(data$y, p))
                     q
                   })
geom_eqq <- function(mapping = NULL,
                     data = NULL,
                     geom = "point",
                     position = "identity", 
                     na.rm = FALSE, 
                     show.legend = NA, 
                     inherit.aes = TRUE, ...) {
  layer(stat = StatEQQ,
        data = data,
        mapping = mapping, 
        geom = geom,
        position = position, 
        show.legend = show.legend, 
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...))
}
ggplot(mpg, aes(x=hwy,y=cty)) +
  geom_eqq()