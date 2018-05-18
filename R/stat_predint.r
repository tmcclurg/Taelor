#A ggplot2 extension that plots prexiction intervals around a 
#linear regression line

StatPredInt = ggplot2::ggproto("StatPrIntUpr", ggplot2::Stat,
                      required_aes = c("x", "y"),
                      compute_group = function(data, scales, 
                                               formula = y ~x,
                                               level=.95) {
                        rng <- seq(min(data$x, na.rm = TRUE), max(data$x, na.rm = TRUE),
                                   by=.1)
                        grid <- data.frame(x = rng)
                        
                        fit = lm(y ~ x, data=data)
                        pr = predict(fit, newdata=grid, interval="prediction",
                                     level=level)
                        grid$ymin = pr[,2] #the lower bound of the prediction interval
                        grid$ymax = pr[,3] #the upper bound of the prediction interval
                        
                        grid
                      }
)


stat_predint = function(mapping = NULL, data = NULL, geom = "ribbon",
                        position = "identity", na.rm = FALSE, 
                        show.legend = NA, inherit.aes = TRUE,
                        fill = "grey60", alpha=.2, ...) {
  layer(
    stat = StatPredInt, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, fill=fill, alpha=alpha,  ...) 
  ) 
}

devtools::use_package("ggplot2")
