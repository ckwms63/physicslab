#' Plot data with fitted line
#'
#' Plot the linear dependence between the concentration and conductivity of a dilute electrolyte
#'
#' @param concentration numeric vector of concentration
#' @param resistance numeric vector of resistance
#
#' @export
#' @examples
#' concentration <- c(1.e-3, 2.5e-3, 5.e-3, 7.5e-3, 1.e-2)
#' resistance <- c(5.31, 2.66, 1.40, 0.92, 0.78)
#' plotfit(concentration, resistance)

plotfit <-
  function (concentration, resistance) {

# calculate the conductivity
k <- 1.e-4*resistance[1]
conductivity <- k/resistance

# define the labels on x- and y-axis
xlabel <- expression(paste(italic(c)," [mol/L]"))
ylabel <- expression(paste(sigma," [(",Omega," cm)"^-1,"]"))

# plot the data
plot(concentration, conductivity, xlab = xlabel, ylab = ylabel,
     xlim = c(0, 0.01), ylim = c(0, 7.e-4))
grid(col = "darkgray")

# find the best fit
cond.fit <- lm(conductivity ~ concentration,
               data = data.frame(concentration, conductivity))

# plot the fitted line
abline(cond.fit)

}
