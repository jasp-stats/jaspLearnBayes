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

LSgameofchance   <- function(jaspResults, dataset, options, state = NULL){

  # input values
  nPlayers  <- length(options[["players"]])
  probWin   <- sapply(options[["players"]], function(p)p[["values"]][[1]])
  xPoints   <- sapply(options[["players"]], function(p)p[["values"]][[2]])
  pointsToWin <- options[["pointsToWin"]]
  numberOfSimulatedGames     <- options[["numberOfSimulatedGames"]]

  # normalizing the probability of wining:
  probWin   <- probWin / sum(probWin) # normalit


  ## check errors
  if(nPlayers < 2)
    .quitAnalysis(gettext("Warning: The number of players must be at least 2. Adjust the inputs!"))
  if(nPlayers > 9)
    .quitAnalysis(gettext("Warning: The number of players must be at most 9. Adjust the inputs!"))
  
  if(pointsToWin < 1)
    .quitAnalysis(gettext(
      "Warning: The number of point(s) required to win should be at least 1!"
    ))

  if(max(xPoints) >= pointsToWin)
    .quitAnalysis(gettextf(
      "Warning: Player %1$s has already won the game. Adjust the inputs!",
      LETTERS[xPoints == max(xPoints)[1]]
    ))

  if(sum(c(xPoints, probWin)) != sum(abs(c(xPoints, probWin))))
    .quitAnalysis(gettext("Warning: No negative input values! Adjust the inputs!"))

  #if(numberOfSimulatedGames<100)
  #  .quitAnalysis(gettext(
  #    "Warning: The number of simulated games should not be smaller than 100!"
  #    ))


  ## Summary Table
  summaryTable <- createJaspTable(title = gettext("Summary Table"))

  summaryTable$dependOn(c("players", "pointsToWin", "numberOfSimulatedGames", "ci"))
  summaryTable$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")

  summaryTable$addColumnInfo(name = "players",   title = gettext("Players"),   type = "string")
  summaryTable$addColumnInfo(name = "pPoint",       title = gettext("p(win 1 point)"), type = "number")
  summaryTable$addColumnInfo(name = "pointsGained", title = gettext("Points Gained"),   type = "integer")
  summaryTable$addColumnInfo(name = "pA",   title = gettext("Analytical"),   type = "number",
                             overtitle = gettext("p(win the game)"))
  summaryTable$addColumnInfo(name = "pS",   title = gettext("Simulated"),   type = "number",
                             overtitle = gettext("p(win the game)"))

  # add footnote for normalization of prob
  # if(sum(as.numeric(unlist(strsplit(probWin,",")))) != 1){
  #   summaryTable$addFootnote("The probability entered does not sum to 1. Already normalized!")
  # }

  ## Credible Interval Plot
  ciPlot <- createJaspPlot(title = "Probability of Player A Winning",  width = 480, height = 320)
  ciPlot$dependOn(c("players", "pointsToWin", "numberOfSimulatedGames", "ci"))
  ciPlot$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")

  # column specification
  ciPlot0 <- ggplot2::ggplot(data= NULL) +
    #ggplot2::ggtitle("Probability of Player 1 Winning") +
    ggplot2::xlab("Number of Simulated Games") +
    ggplot2::ylab("p(A Wins the Game)") +
    ggplot2::coord_cartesian(xlim = c(0, numberOfSimulatedGames), ylim = c(0, 1))

  ## fill in the table and the plot
  if (nPlayers == 2&&max(xPoints) < pointsToWin){

    # output of compareChanceTwoPlayers, when there are two players
    result <- compareChanceTwoPlayers(probWin[1], xPoints[1], xPoints[2], pointsToWin, numberOfSimulatedGames)

    # fill in the table
    summaryTable$addRows(list(players = "A", pPoint = probWin[1],   pointsGained = xPoints[1], pA = result[[2]],   pS = result[[1]]))
    summaryTable$addRows(list(players = "B", pPoint = 1-probWin[1], pointsGained = xPoints[2], pA = 1-result[[2]], pS = 1-result[[1]]))

    # fill in the plot
    if (options[["ci"]]){ # whether plot ci or not

      # Credibility interval (highest posterior density interval)
      SimulResult <- result[[4]] # store the simulated result
      SimulMatrix <- matrix(0, nrow = 1000, ncol = numberOfSimulatedGames) # the matrix of samples from posterior distribution based on simulated result

      for (i in 1:numberOfSimulatedGames){
        SimulMatrix[ , i] <- rbeta(1000, SimulResult[i]*i+1, i-SimulResult[i]*i+1)
      }
      CredInt <- apply(SimulMatrix, 2, HDInterval::hdi) # record the credibility interval
      y.upper <- CredInt[1,]
      y.lower <- CredInt[2,]
      ciPlot0 <- ciPlot0 +
        ggplot2::geom_polygon(ggplot2::aes(x = c(1:numberOfSimulatedGames,numberOfSimulatedGames:1), y = c(y.upper, rev(y.lower))),
                     fill = "lightsteelblue")  # ci
    }

    ciPlot$plotObject <- jaspGraphs::themeJasp(ciPlot0) +
      ggplot2::geom_line(color = "darkred", ggplot2::aes(x = c(1:numberOfSimulatedGames), y = rep(result[[2]], numberOfSimulatedGames))) +  # analytical prob
      ggplot2::geom_line(data= NULL, ggplot2::aes(x = c(1:numberOfSimulatedGames), y = result[[4]])) # simulated prob

  }else if (nPlayers >= 3&&max(xPoints) < pointsToWin){
    # output of compareChanceNPlayers, when there are three or more players
    result <- compareChanceNPlayers(xPoints, pointsToWin, probWin, numberOfSimulatedGames)

    # a vector of analytical p for all players, calculated by switching with player 1
    Analytical_Prob <- vector()
    for (i in 1:length(probWin)){
      k_copy <- replace(xPoints, c(1, i), xPoints[c(i, 1)])
      p_copy <- replace(probWin, c(1, i), probWin[c(i, 1)])
      Analytical_Prob[i] <- compareChanceNPlayers(k_copy, pointsToWin, p_copy, numberOfSimulatedGames)[[2]]
    }

    # fill in the table
    for (i in 1:nPlayers){
      summaryTable$addRows(list(
        players = LETTERS[i],
        pPoint = probWin[i],
        pointsGained = xPoints[i],
        pA = Analytical_Prob[i],
        pS = result[[1]][i])
      )
    }

    # fill in the plot
    if (options[["ci"]]){ # whether plot ci or not

      # Credibility interval (highest posterior density interval)
      SimulResult <- result[[4]] # store the simulated result
      SimulMatrix <- matrix(0, nrow = 1000, ncol = numberOfSimulatedGames) # the matrix of samples from posterior distribution based on simulated result

      for (i in 1:numberOfSimulatedGames){
        SimulMatrix[ , i] <- MCMCpack::rdirichlet(1000, result[[4]][ ,i]*i+1)[ ,1]
      }
      CredInt <- apply(SimulMatrix, 2, HDInterval::hdi) # record the credibility interval
      y.upper <- CredInt[1,]
      y.lower <- CredInt[2,]
      ciPlot0 <- ciPlot0 +
        ggplot2::geom_polygon(ggplot2::aes(x = c(1:numberOfSimulatedGames,numberOfSimulatedGames:1), y = c(y.upper, rev(y.lower))),
                     fill = "lightsteelblue")  # ci
    }

    ciPlot$plotObject <- jaspGraphs::themeJasp(ciPlot0) +
      ggplot2::geom_line(color = "darkred", ggplot2::aes(x = c(1:numberOfSimulatedGames), y = rep(result[[2]], numberOfSimulatedGames))) +   # analytical prob
      ggplot2::geom_line(data= NULL, ggplot2::aes(x = c(1:numberOfSimulatedGames), y = result[[4]][1,])) # simulated prob
  }


  jaspResults[["summaryTable"]] <- summaryTable
  jaspResults[["ciPlot"]] <- ciPlot

  return()
}
