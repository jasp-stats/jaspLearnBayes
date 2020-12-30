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


LSgameofskill   <- function(jaspResults, dataset, options, state = NULL){

  # input values
  nPlayers  <- length(options[["players"]])
  priorSkill   <- sapply(options[["players"]], function(p)p[["values"]][[1]])
  xPoints   <- sapply(options[["players"]], function(p)p[["values"]][[2]])
  winPoints <- options[["winPoints"]]
  nSims     <- options[["nSims"]]
  
  
  ## check errors
  if(nPlayers < 2)
    .quitAnalysis(gettextf("Warning: The number of players must be at least 2. Adjust the inputs!"))

  if(nPlayers != length(xPoints))
    .quitAnalysis(gettextf(
      "The number of players (%1$i) does not equal the numbers of points for each player when interrupted (%2$i). Please check the appropriate settings.",
      nPlayers,
      length(xPoints)
    ))

  if(max(xPoints) >= winPoints)
    .quitAnalysis(gettextf(
      "Warning: Player %1$i has already won the game. Adjust the inputs!",
      which(xPoints == max(xPoints))
    ))

  if(sum(c(xPoints,priorSkill) > 0) != length(c(xPoints,priorSkill)))
    .quitAnalysis(gettextf(
      "Warning: No negative input values! Adjust the inputs!"
    ))


  ## Summary Table
  summaryTable <- createJaspTable(title = gettext("Summary Table"))

  summaryTable$dependOn(c("players", "nSims", "winPoints", "CI"))
  summaryTable$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")

  summaryTable$addColumnInfo(name = "players",   title = gettext("Players"),   type = "string")
  summaryTable$addColumnInfo(name = "prior",   title = gettext("Prior Skill"),   type = "string")
  summaryTable$addColumnInfo(name = "points",   title = gettext("Points Gained"),   type = "string")
  summaryTable$addColumnInfo(name = "pA",   title = gettext("Analytical"),   type = "number",
                             overtitle = gettext("p(win the game)"))
  summaryTable$addColumnInfo(name = "pS",   title = gettext("Simulated"),   type = "number",
                             overtitle = gettext("p(win the game)"))


  ## Credible Interval Plot
  CIPlot <- createJaspPlot(title = "Probability of Player 1 Winning",  width = 480, height = 320)
  CIPlot$dependOn(c("players", "nSims", "winPoints", "CI"))
  CIPlot$addCitation("JASP Team (2018). JASP (Version 0.9.2) [Computer software].")

  # column specification
  CIPlot0 <- ggplot2::ggplot(data= NULL) +
    #ggtitle("Probability of Player 1 Winning") +
    ggplot2::xlab("Number of Simulated Games") +
    ggplot2::ylab("Pr(Winning the Game)") +
    ggplot2::coord_cartesian(xlim = c(0, nSims), ylim = c(0, 1))

  ## fill in the table and the plot
  if (nPlayers == 2 & max(xPoints) < winPoints){

    # output of compareSkillTwoPlayers, when there are two players
    result <- compareSkillTwoPlayers(xPoints[1],xPoints[2],winPoints,priorSkill[1],priorSkill[2],nSims)

    # fill in the table
    summaryTable$addRows(list(players = 1, prior = priorSkill[1], points = xPoints[1], #
                              pA = result[[2]], pS = result[[1]]))
    summaryTable$addRows(list(players = 2, prior = priorSkill[2], points = xPoints[2],#
                              pA = 1-result[[2]], pS = 1-result[[1]]))

    # fill in the plot
    if (options[["CI"]]){ # whether plot CI or not

      # Credibility interval (highest posterior density interval)
      SimulResult <- result[[4]] # store the simulated result
      SimulMatrix <- matrix(0, nrow = 1000, ncol = nSims) # the matrix of samples from posterior distribution based on simulated result

      for (i in 1:nSims){
        SimulMatrix[ , i] <- rbeta(1000, SimulResult[i]*i+1, i-SimulResult[i]*i+1)
      }  #1 + SimulResult[i]*i, 1 + i-SimulResult[i]*i) #posterior dist
      CredInt <- apply(SimulMatrix, 2, HDInterval::hdi) # record the credibility interval
      y.upper <- CredInt[1, ]
      y.lower <- CredInt[2, ]
      CIPlot0 <- CIPlot0 +
        ggplot2::geom_polygon(ggplot2::aes(x = c(1:nSims,nSims:1), y = c(y.upper, rev(y.lower))),
                     fill = "lightsteelblue")  # CI
    }

    CIPlot$plotObject <- jaspGraphs::themeJasp(CIPlot0) +
      ggplot2::geom_line(color = "darkred", ggplot2::aes(x = c(1:nSims), y = rep(result[[2]], nSims))) +  # analytical prob
      ggplot2::geom_line(data= NULL, ggplot2::aes(x = c(1:nSims), y = result[[4]])) # simulated prob


  }else if (nPlayers >= 3 & max(xPoints) < winPoints){
    # output of compareSkillNPlayers, when there are three or more players
    result <- compareSkillNPlayers(xPoints, winPoints, priorSkill, nSims)

    # a vector of analytical p for all players, calculated by switching with player 1
    Analytical_Prob <- vector()
    for (i in 1:length(xPoints)){
      xPoints_copy <- replace(xPoints, c(1, i), xPoints[c(i, 1)])
      priorSkill_copy <- replace(priorSkill, c(1, i), priorSkill[c(i, 1)])
      Analytical_Prob[i] <- compareSkillNPlayers(xPoints_copy, winPoints, priorSkill_copy, nSims)[[2]]
    }

    # fill in the table
    for (i in 1:nPlayers){
      summaryTable$addRows(list(players = i, prior = priorSkill[i], points = xPoints[i],
                                pA = Analytical_Prob[i], pS = result[[1]][i]))
    }

    # fill in the plot
    if (options[["CI"]]){ # whether plot CI or not

      # Credibility interval (highest posterior density interval)
      SimulResult <- result[[4]] # store the simulated result
      SimulMatrix <- matrix(0, nrow = 1000, ncol = nSims) # the matrix of samples from posterior distribution based on simulated result

      for (i in 1:nSims){
        SimulMatrix[ , i] <- MCMCpack::rdirichlet(1000, result[[4]][ ,i]*i+1)[ ,1]
      }  #rdirichlet(1000, result4()[[4]][ ,i]*i + 1)[ ,1]
      CredInt <- apply(SimulMatrix, 2, HDInterval::hdi) # record the credibility interval
      y.upper <- CredInt[1, ]
      y.lower <- CredInt[2, ]
      CIPlot0 <- CIPlot0 +
        ggplot2::geom_polygon(ggplot2::aes(x = c(1:nSims,nSims:1), y = c(y.upper, rev(y.lower))),
                     fill = "lightsteelblue")  # CI
    }

    CIPlot$plotObject <- jaspGraphs::themeJasp(CIPlot0) +
      ggplot2::geom_line(color = "darkred", ggplot2::aes(x = c(1:nSims), y = rep(result[[2]], nSims))) +   # analytical prob
      ggplot2::geom_line(data= NULL, ggplot2::aes(x = c(1:nSims), y = result[[4]][1, ])) # simulated prob

  }
  jaspResults[["summaryTable"]] <- summaryTable
  jaspResults[["CIPlot"]] <- CIPlot

  return()
}
