#' @title plot.rolls
#' @description plot input roll and times in bar chart
#' @param roll roll outcomes of input device
#' @param times total times of rolls
#' @return side, count, probability summary of the input roll
#' @export
#'
plot.rolls = function(roll, times){
  sum = summary(roll)
  df = sum$freqs
  ggplot(df, aes(x = df$side, y = df$prop))+
    geom_bar(stat = "identity", col = "grey")+
    ylab("relative frequencies")+
    xlab("sides of device")+
    ggtitle(paste0("Relative Frequencies in a series of ",roll$total, " rolls"))
}
#plot(fair_50rolls)
