#' @title:
#'  Create a new Monty Hall Problem game.
#'
#' @description:
#'`create_game()` generates a new game that consists of two doors
#' with goats behind them, and one with a car.
#'
#' @details:
#'The game setup replicates the game on the TV show "Let's
#'Make a Deal" where there are three doors for a contestant
#'to choose from, one of which has a car behind it and two
#'have goats. The contestant selects a door, then the host
#'opens a door to reveal a goat, and then the contestant is
#'given an opportunity to stay with their original selection
#'or switch to the other unopened door. There was a famous
#'debate about whether it was optimal to stay or switch when
#'given the option to switch, so this simulation was created
#'to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
}



#' @title:
#'  Randomly select on of three doors.
#' @description:
#'  This function creates a choice of three doors. It then randomly picks one door and reveals the door number.
#' @details:
#'  The first assignment, "doors", creates a vector with three numbers. The next assignment, "a.pick" then chooses a random sample of one of the doors. The first position in the parentheses determines what vector to pick from. The next position, "size=1" determines how many choices to select. Since we only want to open one door, the size is set to 1. Finally, we return the door that was selected.
#' @param:
#'  The door number returned is a numeric value.
#' @return:
#'  "a.pick" is set as the return since this displays which door was selected.
#' @examples:
#'   select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title:
#'  Game show host reveals what is behind a different door.
#' @description:
#'  This function builds on which door was selected by the contestant in the previous function. The host selects a different door to reveal what is behind it. Only a non-winning door with a goat behind it shoudl be selected.
#' @details:
#'  The first row sets open_goat_door as a function based on what was returned for a.pick and the correlated "goat" or "car" as set in the "game" assignment. The second row uses an if argument to prompt the host to reveal what is behind a different door. Inside the "if" function, "goat.doors" is set to include only doors that are not the door with a car behind it. Then "opened.door" selects a sample, this time it is picking from "goat.doors" which we just set to be the two doors that do not have cars behind them. The size is set to one so it selects one of those goat doors. The next "if" function runs if the contestant picked a goat door. The host will then reveal a door that is not the car and is not the original door they selected. At the end of the function, it returns the new opened door.
#' @param:
#'  The door number returned is a numeric value.
#' @return:
#'  The function returns the number of the new "opened.door".
#' @examples:
#' open_goat_door()
#' @export
open_goat_door <- function(a.game, a.pick) 
  {
  doors <- c(1, 2, 3)
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  available.doors <- doors[a.game != "car" & doors != a.pick] 
  
  if (length(available.doors) > 1) {
  
    opened.door <- sample(available.doors, size = 1)
  } else {
 
    opened.door <- available.doors
  }
  
  return(opened.door) 
}




#' @title:
#'  Choose to change doors or to stay with the original choice.
#' @description:
#'  The change_door function chooses the final door selection based on whether the contestant stays with the original choice or changes.
#' @details:
#'  The function is based on whether stay is set to TRUE or FALSE, as well as the door opened by the host "opened.door" and the initial pick "a.pick." Is stay is set to TRUE, the initial pick is used as the final selection. If stay is set to FALSE, the contestant switches doors and the door that is not the inital pick and is not the door revelead by the host is set as the final pick.
#' @param:
#'  The door number returned is a numeric value.
#' @return:
#'  Depending on whethey stay was set to TRUE or FALSE, the final door number will be returned as final.pick.
#' @examples
#' change_door( stay=T, opened.door=1, a.pick )
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
  doors <- c(1,2,3)
  if( stay )
  {
    final.pick <- a.pick
  }
  if( ! stay )
  {
    final.pick <- doors[ doors != opened.door & doors != a.pick ]
  }
  return( final.pick )  # number between 1 and 3
}



#' @title:
#'  Win/Lose Revealed
#' @description:
#'  This final step reveals whether the contestant selected the door with the car behind it.
#' @details:
#'  The "determine_winner" function is based on the "final.pick" determined in the previous step. It also calls on the objects assigned to each door in the beginning with the "game" assignment. If the door number selected for the final pick is associated with the car, the word "WIN" is returned. If the final pick is associated with a door that has a goat behind it, the word "LOSE" is returned.
#' @param:
#'  Text is returned as characters.
#' @return:
#'  Depending on whether a car or a goat was selected, the text "WIN" or "LOSE" will be returned.
#' @examples
#' determine_winner(final.pick=2)
#' @export
determine_winner <- function( final.pick, a.game )
{
   if( a.game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
  else
   {
      return( "LOSE")
   }
}





#' @title:
#'  Putting it all together and playing the game.
#' @description:
#'  All of the steps are run sequentially so that a round of the game is played.
#' @details:
#'  The play_game function runs all of the above functions in order. This step adds a "game.results" function that builds on outcomes based on whether the contestant stayed or switched doors. It then builds a data frame to display the final results.
#' @param:
#'  The outcome is returned as characters.
#' @return:
#'  A data frame is returned displaying "WIN" or "LOSE" and "STAY" or "SWITCH".
#' @examples
#' play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title:
#'  Play multiple rounds
#' @description:
#'  Use a simulation to run multiple rounds of the game
#' @details:
#'  The play_n_games function allows you to set the number of iterations to play. Each time the game is played, the games are stored in the data frame.The dplyr package will be required to bind rows in the results table.
#' @param:
#'  Values will be returned as integers.
#' @return:
#'  A data frame is returned displaying a summary of wins, losses, switches, and stays.
#' @examples
#'   play_n_games <- function( n=100 )
  #' @export

  play_n_games <- function( n=100 )
  {
    library( dplyr )
    results.list <- list()   # collector
    loop.count <- 1

    for( i in 1:n )  # iterator
    {
      game.outcome <- play_game()
      results.list[[ loop.count ]] <- game.outcome
      loop.count <- loop.count + 1
    }

    results.df <- dplyr::bind_rows( results.list )

    table( results.df ) %>%
      prop.table( margin=1 ) %>%  # row proportions
      round( 2 ) %>%
      print()

    return( results.df )

  }

play_n_games()









