#' @title A function to simulate a game of gambling (chance) under the gambler's ruin setup.
#'
#' @description The gambler's ruin problem is a classic example, which illustrates
#'              the application of one-dimensional Random Walks - a Stochastic Process.
#'              Simulation of a gambling game under the gambler's ruin setup concerns to
#'              a gambler starting the game with an initial capital, where the probability
#'              of winning a particular round is 'p'. If the gambler wins the round, then
#'              1 unit of money is added to the gambler's existing capital and if the gambler loses a round,
#'              then 1 unit of money is deducted from the gambler's existing capital.
#'              The game stops when the gambler reaches his desired amount of money or gets
#'              totally bankrupted (ruined), these two points are known as the absorbed states
#'              of the game, or equivalently absorbed states in the one-dimensional random walk.
#'
#'              The function 'grp.gameplay()' simulates the above described game, where the simulation
#'              runs until one of the absorbed states are reached, i.e., simulating the game until the gambler
#'              reaches to 0 money, getting ruined or wins the desired or targeted amount, eventually
#'              winning the game.
#'              User inputs are accepted, which includes the initial amount of money with which the gambler enters
#'              the game, the probability 'p' of winning each round of the game and lastly the amount of money, which
#'              the gambler wishes to earn from this game being played.The function facilitates majorly in visualizing
#'              the game trajectory of the gambler, along with the overall probability of the gambler winning the entire game.
#'
#'
#' @author Somjit Roy
#'
#'
#'
#'
#' @param ini.stake The initial capital (money) with which the gambler enters the game.
#' @param p The probability with which the gambler wins each round of the game, 0<p<1.
#' @param win.amt The amount of money which the gambler desires to win from the game.
#'
#' @return A Graphical Plot - graphical representation of the entire trajectory of the money/capital with
#'         the gambler during the course of the game being played, along with the long run
#'         probability of winning the entire game, stated in the graph as "Overall Probability of winning this entire game".
#'
#'
#' @export
#'
#'
#' @seealso The simulation of the gambler's ruin problem helps to demonstrate the idea of one-dimensional random walks,
#'          consequently facilitating the readers to have an example of a stochastic process.
#'          The gambler's ruin problem is a very famous problem, often visited in the course of probability, aimed at explaining
#'          stochastic processes, and two of its major offshoots -  Random Walks and Markov Chains.
#'
#'          The game can be simulated under both biased as well as unbiased situations, depending
#'          on the value of 'p' chosen by the user, thereby giving examples of both biased as well as
#'          unbiased random walks.
#'
#'          NOTE :: Here the wagered amount is 1 unit of money for each round, as dictated by the
#'          setup of the gambler's ruin problem.
#'
#' @references Frederick Mosteller, Fifty Challenging Problems in Probability with Solutions,
#'             1965, Dover Publications.
#'
#'
#' @examples
#'
#'# Suppose a gambler enters a game of gambling under the gambler's ruin setup with an initial
#'# amount (capital) of 100 and wants to reach an amount of 200, where the probability
#'# of winning each round of the game for the gambler is 0.5, i.e., a fair game, then
#'# the gambler's ruin problem under this framework is simulated as follows:
#'
#' \donttest{grp.gameplay(5,0.5,10)}
#'
#'
grp.gameplay = function(ini.stake,p,win.amt)
{

  # ini.stake :: The Initial Stake with which the gambler enters the game.
  # p :: win probability for each round for the gambler, 0<p<1.
  # win.amt :: The amount the gambler wants to win from the game utlimately.

  round.money = array(dim=1) # Money with the gambler in each round of the game.

  round.money[1] = ini.stake # Storing the money for the first round, which is
  # indeed the initial capital.

  ct = 1 # A counter keeping the position count of the array round.money[].

  while(round.money[ct] > 0) # Until going broke
  {
    ct = ct + 1            # Number of Rounds being played.

    if(rbinom(1,1,p) == 1) # The gambler wins the round.
    {
      round.money[ct] = round.money[ct-1] + 1
    }else                  # The gambler loses the round.
    {
      round.money[ct] = round.money[ct-1] - 1
    }

    if(round.money[ct] == win.amt) # If the gambler reaches the winning amount,
      # the gambler stops playing.
    {
      break
    }
  }

  if(p == 0.5)   # The gambler playing a fair/unbiased game.
  {
    # Probability of winning the entire fair game.

    win.prob = ini.stake/win.amt

  }else          # The gambler playing a biased game.
  {
    # Probability of winning the entire biased game.

    win.prob = (1-(((1-p)/p)^ini.stake))/(1-(((1-p)/p)^win.amt))
  }

  # A Data Frame for storing the data, which is to be plotted.
  rounds = 1:ct
  capital = round.money

  df = data.frame(rounds, capital)

  if(round.money[length(round.money)] == 0) # Ultimately losing the game
    # ---> going broke.
  {
    p<-ggplot(df, aes(x = rounds,y = capital,color=capital)) +
      theme_ipsum_rc() + ylab("Capital in Each Round") +
      ggtitle(paste("Overall Probability of winning this entire game\nis ",win.prob
                    ,"\nInitial stake as ",ini.stake,"; Winning Amount = ",win.amt,
                    "\nWin probability in each round is p = ",p,
                    "\nBut Sorry you lost! :( ",
                    "\nNumber Of Rounds Played = ",ct)) +
      geom_line() + theme(legend.position="None") +
      xlab("Rounds of the Game") + transition_reveal(df$rounds) +
      scale_color_viridis(discrete = F) +
      geom_point(size=4,color="#8E44AD")

  }else  # Ultimately winning the game ---> acquiring the winning amount.
  {
    p<-ggplot(df, aes(x = rounds,y = capital,color=capital)) +
      theme_ipsum_rc() + ylab("Capital in Each Round") +
      ggtitle(paste("Overall Probability of winning this entire game\nis ",win.prob,
                    "\nInitial stake = ",ini.stake,"; Winning Amount = ",win.amt,
                    "\nWin probability in each round is p = ",p,
                    "\nCongratulations You Win! :) ",
                    "\nNumber Of Rounds Played = ",ct)) +
      geom_line() + theme(legend.position="None") +
      xlab("Rounds of the Game") + transition_reveal(df$rounds) +
      scale_color_viridis(discrete = F) +
      geom_point(size=4,color="#8E44AD")

  }
  return(p)
}




