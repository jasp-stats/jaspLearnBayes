#
# Copyright (C) 2019 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

LSBuffonsneedlemanipulation   <- function(jaspResults, dataset, options, state = NULL){
 
  # input values
  crosses <- options[["k"]]
  observations <- options[["n"]]
  a <- options[["a"]]
  b <- options[["b"]]
  lengthRatio <- options[["length"]]
  CI <- options[["CI"]]
  plot2 = options[["plot2"]]
  plot3 = options[["plot3"]]
  CIArrow = options[["CIArrow"]]
  legend1 = options[["legend1"]]
  legend2 = options[["legend2"]]
  
  
  

  # example d for computation
  d <- 5
  l <- lengthRatio*d/100

  # some warnings
  if(crosses > observations)
    .quitAnalysis(gettext("Warning: Number of crosses needs to be smaller than or equal to number of observations!"))


  ## Summary Table
  summaryTable <- createJaspTable(title = gettext("Summary Table"))
  summaryTable$position <- 1
  summaryTable$dependOn(c("crosses", "observations", "a", "b", "lengthRatio", "CI"))
  summaryTable$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
  
  summaryTable$addColumnInfo(name = "NumCrosses", title = gettext("Crosses"), type = "string")
  summaryTable$addColumnInfo(name = "NumObservations", title = gettext("Observations"), type = "string")
  summaryTable$addColumnInfo(name = "Median", title = gettext(paste("Median for", "\u03c0")),   type = "string")
  summaryTable$addColumnInfo(name = "lowerCI", title = gettext("Lower"), type = "string", 
                             overtitle = paste0(CI*100, "% Credible Interval"))
  summaryTable$addColumnInfo(name = "upperCI", title = gettext("Upper"), type = "string", 
                             overtitle = paste0(CI*100, "% Credible Interval")) 

  # fill in the table
  CI95lower <- 2 * l / (qbeta((1-CI)/2, crosses, observations - crosses, lower.tail = FALSE) * d)
  CI95lower = round(CI95lower, digit = 2)
  
  med <- 2 * l / (qbeta(.5, crosses, observations - crosses, lower.tail = FALSE) * d)
  med = round(med, digit = 2)
  
  CI95upper <- 2 * l / (qbeta(1-(1-CI)/2, crosses, observations - crosses, lower.tail = FALSE) * d)
  CI95upper = round(CI95upper, digit = 2)
  
  summaryTable$addRows(list(NumCrosses = crosses, NumObservations = observations,
                            lowerCI = CI95lower, Median = med,   upperCI = CI95upper))
  jaspResults[["summaryTable"]] <- summaryTable
  
  ## 1. prior and posterior plot for proportion of crosses
  if (plot2){
    propPlot <- createJaspPlot(title = paste("Prior and Posterior for Proportion of Crosses"),
                               width = 480, height = 320)
    propPlot$position <- 2
    propPlot$dependOn(c("crosses", "observations", "a", "b", "lengthRatio", "CI", "plot2"))
    propPlot$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
    
    # values
  
    xValue <- seq(0,1,0.005)
    propPost <- dbeta(xValue, a + crosses, b + observations - crosses)
    propPrior <-dbeta(xValue, a, b)
    dataProp = data.frame(values = c(xValue, xValue),
                          density = c(propPost, propPrior),
                          group = c(rep("Posterior",201), rep("Prior",201))
    )
    # axis specification
    propPlot0 <- ggplot2::ggplot(data = dataProp,  ggplot2::aes(x = values, y = density)) +
      ggplot2::xlab("Proportion of Crosses") +
      ggplot2::ylab("Density") #+
    #ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1.6*max(propPost)))
    
    # fill in the plot
    propPlot$plotObject <- jaspGraphs::themeJasp(propPlot0) +
      ggplot2::geom_line(ggplot2::aes(linetype = group), size = 1) +
      ggplot2::scale_linetype_manual("", values = c("Posterior" = "solid",
                                                    "Prior" = "dashed")) 
    if (legend1){
      propPlot$plotObject <-  propPlot$plotObject + 
        ggplot2::theme(legend.position = c(.17, .9))
    }
    jaspResults[["propPlot"]] <- propPlot
  }
  
  
  ## 2. Distribution Plot
  if (plot3){
    distPlot <- createJaspPlot(title = paste("Implied Prior and Posterior for", "\u03c0"),  width = 480, height = 320)
    distPlot$position <- 3
    distPlot$dependOn(c("crosses", "observations", "a", "b", "lengthRatio", "CI", "plot3", "CIArrow"))
    distPlot$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")
    
    # values
    x <- seq(2,4,0.01)
    yPost <- 2 * l / (x^2 * d) * dbeta((2 * l / (x * d)), a + crosses, b + observations - crosses)
    yPrior <- 2 * l / (x^2 * d) * dbeta((2 * l / (x * d)), a, b)
    yPi <- seq(0, 1.6*max(yPost), 1.6*max(yPost)/99)
    
    data = data.frame(values = c(x, x, rep(pi, 100)),
                      density = c(yPost, yPrior, yPi),
                      group = c(rep("Implied Posterior",201), rep("Implied Prior",201), rep("\u03c0", 100))
    )
    data$group<-factor(data$group, levels=c("Implied Posterior","Implied Prior","\u03c0"))
    
    # axis specification
    distPlot0 <- ggplot2::ggplot(data = data,  ggplot2::aes(x = values, y = density)) +
      ggplot2::ggtitle("") + # for , pi
      ggplot2::xlab("\u03c0") +
      ggplot2::ylab("Density") +
      ggplot2::coord_cartesian(xlim = c(2, 4), ylim = c(0, 1.6*max(yPost)))
    
    # fill in the plot
    distPlot$plotObject <- jaspGraphs::themeJasp(distPlot0) +
      ggplot2::geom_line(ggplot2::aes(color = group, linetype = group), size = 1) +
      ggplot2::scale_color_manual("", values = c("Implied Posterior" = "black",
                                                 "Implied Prior" = "black",
                                                 "\u03c0" = "red")) +
      ggplot2::scale_linetype_manual("", values = c("Implied Posterior" = "solid",
                                                    "Implied Prior" = "dashed",
                                                    "\u03c0" = "solid"))
    if (legend2){
      distPlot$plotObject <-  distPlot$plotObject + 
        ggplot2::theme(legend.position = c(.24, .9))
    }

    if (CIArrow){
      distPlot$plotObject <- distPlot$plotObject +
        ggplot2::annotate("text", x = 3.7, y = 1.6*max(yPost), 
                          label = paste0(CI*100, "% CI: ",
                                         "[",CI95lower,", ", CI95upper, "]"),
                          size = 6
        ) + 
        ggplot2::annotate("segment", x = CI95lower, xend = CI95upper, 
                          y = 1.45*max(yPost), yend = 1.45*max(yPost),
                          arrow = grid::arrow(ends = "both", angle = 90, length = grid::unit(.2,"cm")),
                          size = 1)
      
    }
    jaspResults[["distPlot"]] <- distPlot
  }

  return()
}
