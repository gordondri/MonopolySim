# simulate a game of monopoly 

################ PART A: STATE SIMPLIFICATIONS ################

# simplifications:
  # no trading
  # no auctioning
  # no mortgages 
  # bank never runs out of money 
  # there are enough homes and hotels to go around 
  # free parking doesn't do anything 
  # no selling 'get out of jail free cards' 

################ PART B: SET INITIALIZATIONS ################

# read in the rents
datapath <- "/Users/gordondri/Desktop/MSCA32010"
rents <- read.csv(file=paste(datapath,"rents.csv",sep="/"),
                                header=TRUE,sep=",", stringsAsFactors = FALSE)

# define the types of squares
# buy column is 1 for squares that can be bought (excluding railroads and utilities)
property.squares <- rents[rents['Buy'] == 1, 1]
# buy column is 2 for railroads
railroad.squares <- rents[rents['Buy'] == 2, 1]
# buy column is 3 for utilities
utilities.squares <- rents[rents['Buy'] == 3, 1]
# buy column is 4 for community chest
community.chest.squares <- rents[rents['Buy'] == 4, 1]
# buy column is 5 for chance
chance.squares <- rents[rents['Buy'] == 5, 1]
# buy column is 6 for taxes
taxes.squares <- rents[rents['Buy'] == 6, 1]

# keep track of the amount of money in the bank per player 
bank.table <- data.frame(Player = c(1,2,3,4), Amount = 1500, stringsAsFactors=FALSE)

# keep track of property ownership
ownership.table <- data.frame(Squares = rents[, 'Title'], Owner = 'NA', 
                              Type = rents[, 'Type'],
                              NumHomes = 0, NumHotels = 0, 
                              JailCard = 0, stringsAsFactors=FALSE)

# define all the community chest cards 
# each community chest card was assigned a number 
comm.chest <- read.csv(file=paste(datapath,"CommunityChest.csv",sep="/"),
                header=TRUE,sep=",", stringsAsFactors=FALSE)

# keep track of the community chest cards that have been chosen
comm.chest.cards <- data.frame(CardNumber = rep(1:16), Chosen = 0)

# group all the community chest and chance cards 
group <- read.csv(file=paste(datapath,"Groups.csv",sep="/"),
                  header=TRUE,sep=",", stringsAsFactors=FALSE)

# group 1 is strictly adding to a bank table.  
# group 2 is adding to a bank table AND moving positions 
# group 3 is strictly adding to the ownership table,
# group 4 is strictly moving positions  
# group 5 is adding to the players bank table which involves other players. 
# group 6 is adding to a bank table AND moving positions AND paying dues 
# group 7 is adding to a bank table based on homes/hotels owned. 
# group 8 is finding nearest spot AND moving positions AND buying property/paying dues  

# group the groups that pertain to the same function
function.1.groups <- c(1, 3, 5, 7)

# define all the chance cards 
chance.selection <- read.csv(file=paste(datapath,"chance.csv",sep="/"),
                       header=TRUE,sep=",")

# keep track of the community chest cards that have been chosen
chance.cards <- data.frame(CardNumber = rep(1:16), Chosen = 0)

# keep track of what square each player's token is on 
player.pos.table <- data.frame(Player = 1:no.players, Square = 1, stringsAsFactors=FALSE)

################ PART C: DEFINE FUNCTIONS ################

# simulate dice roll 
dice.roll <- function(n) {
  die.numbers <- c(1, 2, 3, 4, 5, 6)
  moves <- 0
  for (i in 1:2) {
    die.roll <- sample(x = die.numbers, size = 1)
    moves <<- moves + die.roll 
  }
}

# move player to next space
move.player <- function(curr.player, moves) {
  player.pos.table[curr.player, 'Square'] <- moves
  return(player.pos.table)
}

# player decision for buying a vacant property or adding to an existing property 
buying <- function(player.curr.pos, bank.table, ownership.table, curr.player, player.curr.type) {
  # get the number of properties currently on the land (if any)
  num.homes <- ownership.table[player.cur.pos, 'NumHomes']
  num.hotel <- ownership.table[player.cur.pos, 'NumHotels']
  
  # check the status of the property 
  
  # neither homes nor hotel built 
  if (num.homes == 0 & num.hotel == 0) {
    # check how much the property is to buy 
    buy.price <- rents[player.curr.pos, 'Price']
    case <- 'A'
  } else (num.homes < 4) {
    # check how much a house costs 
    buy.price <- rents[player.curr.pos, 'House.Cost'] 
    case <- 'B'
  } else (num.homes == 4) {
    # check how much a hotel costs 
    buy.price <- rents[player.curr.pos, 'Hotel.Cost'] 
    case <- 'C'
  } else if (ownership.table[player.curr.pos, 'Type'] == 'Railroad') {
    case <- 'A'
  }
  
  if (case == 'B') {
    # subset the ownership table for only properties owned by the current player
    subset.ownership.table <- ownership.table[ownership.table$Owner == curr.player & ownership.table$Type == player.curr.type, ]
    # check the number of houses on other properties of the same colour (if any) to ensure building is done evenly
    if (any(subset.ownership.table[, 'NumHomes'] < num.homes)){
      stop("You can't build here - it is not even")
    }
  }
  
  # check if the player has enough money to buy the property
  if (bank.table[curr.player, 'Amount'] >= buy.price) {
    # check if the player wants to buy the property
    answer <- readline("enter 'yes' if you would like to buy and 'no' if not: ")
    if (answer == 'yes') {
      # update the bank table
      bank.table[curr.player, 'Amount'] <- bank.table[player, 'Amount'] - buy.price
      # update the ownership table accordingly 
      if (case == 'A') {
        ownership.table[player.curr.pos, 'Owner'] <- curr.player
      } else if (case == 'B') {
        ownership.table[player.curr.pos, 'NumHomes'] <- ownership.table[player.curr.pos, 'NumHomes'] + 1
      } else if (case == 'C') {
        ownership.table[player.curr.pos, 'NumHotels'] <- 1
        ownership.table[player.curr.pos, 'NumHomes'] <- 0 
      } 
    } 
  }
  bank.table <<- bank.table
  ownership.table <<- ownership.table 
}

# check ownership of property and calculate rent accordingly 
calculate.rent <- function(ownership.table, player.curr.pos, property.owner, player.curr.type, curr.player) {
  # check how many of the property types the player who owns the current property, owns 
  
  # tally up the properties owned by each player for each type 
  count.properties.table <- count(ownership.table, c("Owner", "Type"))
  # subset the count properties table for the player who owns the current property and for the current type
  subset.count.properties.table <- count.properties.table[count.properties.table$Owner == property.owner & count.properties.table$Type == player.curr.type, ]
  count.property.type <- subset.count.properties.table['freq']
  
  # check what type the current property is 
  
  # current proprety is a property square 
  if (player.curr.type == 1) {
    # check the number of homes and hotel on the property 
    num.homes <- ownership.table[player.cur.pos, 'NumHomes']
    num.hotel <- ownership.table[player.cur.pos, 'NumHotels']
    
    if (num.hotels || num.homes > 0) { # property is improved 
      if (num.homes == 1) {
        rent.owed <- rents[player.cur.pos, 'Rent.1']
      } else if (num.homes == 2) {
        rent.owed <- rents[player.cur.pos, 'Rent.2']
      } else if (num.homes == 3) {
        rent.owed <- rents[player.cur.pos, 'Rent.3']
      } else if (num.homes == 4) {
        rent.owed <- rents[player.cur.pos, 'Rent.4']
      }
    } else { # property is not improved 
      # check if the player has a monopoly on the property 
      if (count.property.type == 3) {
        rent.owed <- rents[player.cur.pos, 'Rent'] * 2 
      } else {
        rent.owed <- rents[player.cur.pos, 'Rent']
      }
    }
  } else if (player.curr.type == 2) {
    # current proprety is a railroad
    if (count.property.type == 1) {
      rent.owed <- rents[player.curr.pos, 'Rent.1']
    } else if (count.property.type == 2) {
      rent.owed <- rents[player.curr.pos, 'Rent.2'] 
    } else if (count.property.type == 3) {
      rent.owed <- rents[player.curr.pos, 'Rent.3'] 
    } else {
      rent.owed <- rents[player.curr.pos, 'Rent.4'] 
    }
  } else if (player.curr.type == 3) {
    # current property is a utility 
    if (count.property.type == 1) {
      # if one utility is owned, rent is 4 times the roll of the dice
      rent.owed <- rent[player.curr.pos, 'Rent.1'] * moves 
    } else {
      # if two utilities are owned, rent is 10 times the roll of the dice 
      rent.owed <- rent[player.curr.pos, 'Rent.2'] * moves 
    }
  }
  # update the bank table for the current player
  bank.table[curr.player, 'Amount'] <<- bank.table[curr.player, 'Amount'] - rent.owed
  # update the bank table for the player who owns the property 
  bank.table[property.owner, 'Amount'] <<- bank.table[property.owner, 'Amount'] + rent.owed
  return(rent.owed) 
}

# simulate picking a chance or community chance card
pick.card <- function(card.df) {
  # find card numbers that have not been chosen (i.e chosen == 0)
  cards <- card.df[card.df$Chosen == 0, 'CardNumber']
  if (length(cards) > 0) {
    card.number <- sample(x = cards, size = 1)
    # change the toggle 'chosen' to 1 after this card has been chosen 
    card.df[card.number, 'Chosen'] <<- 1
    return(card.number) 
  } else {
    card.number <- 0
    return(card.number)
  }
}

# initialize pay, collect, keep, other and additional
set.binary.operators <- function(comm.or.cc.table, com.or.cc.card.chosen) {
  pay <<- com.or.cc.table[com.or.cc.card.chosen, 'Pay']
  collect <<- com.or.cc.table[com.or.cc.card.chosen, 'Collect']
  keep <<- com.or.cc.table[com.or.cc.card.chosen, 'Keep']
  other <<- com.or.cc.table[com.or.cc.card.chosen, 'Other']
  additional <<- com.or.cc.table[com.or.cc.card.chosen, 'Additional']
}

# execute the appropriate functions based on the group 
execute.functions <- function(case, group, com.or.cc.card.chosen, comm.or.cc.table, player.pos.table, curr.player, ownership.table, player.curr.type) {
  if (is.element(group, function.1.groups) || group == 2 || group == 6) {
    # call set binary operators function to get the appropriate pay, collect, keep, other and additional values based on the group we are in 
    set.binary.operators(comm.or.cc.table, com.or.cc.card.chosen)
    # determine what function to call based on what group we are in 
    # call function 1
    add.bank.ownership(pay, collect, keep, other, additional, curr.player, bank.table, ownership.table, comm.or.cc.table, com.or.cc.card.chosen, no.players)
    if (group == 2 || group == 6) {
      # need to call function 2 as well
      # case is 2 because we are moving to jail and not going back 3 spaces 
      move.positions(2, player.pos.table, curr.player, comm.or.cc.table, com.or.cc.card.chosen)
      if (group == 6) {
        # need to pay rents as well 
        calculate.rent(ownership.table, player.curr.pos, property.owner, player.curr.type, curr.player)
      }
    }
  } else if (group == 4) {
    # only call function 2 as we are just moving spaces and not updating any tables
    move.positions(case, player.pos.table, curr.player, comm.or.cc.table, com.or.cc.card.chosen) 
  } else if (group == 9) {
    # call function to find nearest position
    if (com.or.cc.card.chosen == 3) {
      case <- 1
    } else if (comm.or.cc.chisen == 4 || comm.or.cc.chisen == 5) {
      case <- 2
    }
    find.nearest.spot(case, curr.player, comm.or.cc.table, com.or.cc.card.chosen, curr.spot, player.pos.table, ownership.table, player.curr.pos, player.curr.type)
  }
} 

add.bank.ownership <- function(pay, collect, keep, other, additional, curr.player, bank.table, ownership.table, comm.or.cc.table, com.or.cc.card.chosen, no.players) {
  # case 1 is where we are strictly adding to a bank table and doing nothing else 
  if (pay == 1 || pay == 0 & collect == 1 || collect == 0 & keep == 0 & other == 0 & additional == 0) {
    # check if the player is paying money or collecting money 
    if (pay == 0 & collect == 1) {
      bank.table[curr.player, 'Amount'] <<- bank.table[curr.player, 'Amount'] + comm.or.cc.table[com.or.cc.card.chosen, 'Pay.Amount']
    } else if (pay == 1 & collect == 0) {
      bank.table[curr.player, 'Amount'] <<- bank.table[curr.player, 'Amount'] - comm.or.cc.table[com.or.cc.card.chosen, 'Pay.Amount']
    }
    # case 2 is where we are adding to a bank table which involves other players 
  } else if (pay == 1 || pay == 0 & collect == 1 || collect == 0 & keep == 0 & other == 1 & additional == 0) {
    # check if the player is paying money or collecting money 
    if (pay == 0 & collect == 1) {
      # change current player's bank account 
      bank.table[curr.player, 'Amount'] <<- bank.table[curr.player, 'Amount'] + (comm.or.cc.table[com.or.cc.card.chosen, 'Collect.Amount'] * no.players)
      # change all other player's bank account
      for (j in 1:no.players) {
        if (i != curr.player) {
          bank.table[j, 'Amount'] <<- bank.table[j, 'Amount'] - comm.or.cc.table[com.or.cc.card.chosen, 'Subtract.Amount']
        }
      }
    } else if (pay == 1 & collect == 0) {
      # change current player's bank account 
      bank.table[curr.player, 'Amount'] <<- bank.table[curr.player, 'Amount'] - (comm.or.cc.table[com.or.cc.card.chosen, 'Pay.Amount'] * no.players)
      # change all other player's bank account
      for (j in 1:no.players) {
        if (i != curr.player) {
          bank.table[j, 'Amount'] <<- bank.table[j, 'Amount'] + comm.or.cc.table[com.or.cc.card.chosen, 'Subtract.Amount']
        }
      }
    }
    # case 3 is where we adding to a bank table based on homes/hotels owned
  } else if (pay == 1 || pay == 0 & collect == 1 || collect == 0 & keep == 0 & other == 0 & additional == 1) {
    # find out number of homes 
    num.homes <- sum(ownership.table[ownership.table$Owner == curr.player, 'NumHomes'])
    num.hotels <- sum(ownership.table[ownership.table$Owner == curr.player, 'NumHotels'])
    # change the current player's bank account 
    bank.table[curr.player, 'Amount'] <<- bank.table[curr.player, 'Amount'] - (comm.or.cc.table[com.or.cc.card.chosen, 'PerHouse'] * num.homes + comm.or.cc.table[com.or.cc.card.chosen, 'PerHotel'] * num.hotels)
    # case 4 is where we are adding to the ownership table 
  } else if (pay == 0 & collect == 0 & keep == 1 & other == 0 & additional == 0) {
    # check if the player already has a get out of jail free card, if not, give them one, if they do, add 1 
    ownership.table[ownership.table$Owner == curr.player, 'JailCard'] <<- ownership.table[ownership.table$Owner == curr.player, 'JailCard'] + 1
  }
}

move.positions <- function(case, player.pos.table, curr.player, comm.or.cc.table, com.or.cc.card.chosen, ownership.table) {
  # case 1: go back 3 spaces 
  if (case == 1) {
    player.pos.table[curr.player, 'Square'] <<- player.pos.table[curr.player, 'Square'] - comm.or.cc.table[com.or.cc.card.chosen, 'Advance.Pos.1']
    # case 2: go directly to jail without collecting $200 when passing go 
  } else if (case == 2) {
    player.pos.table[curr.player, 'Square'] <<- comm.or.cc.table[com.or.cc.card.chosen, 'Advance.Pos.1']
  }
  # update the property owner based on the new position in which the player is on 
  property.owner <<- ownership.table[comm.or.cc.table[com.or.cc.card.chosen, 'Advance.Pos.1'], 'Owner']
  # update the player's current position
  player.curr.pos <<- player.pos.table[curr.player, 'Square']
}

find.nearest.spot <- function(case, curr.player, comm.or.cc.table, com.or.cc.card.chosen, curr.spot, player.pos.table, ownership.table, player.curr.pos, player.curr.type) {
  # case 1: advance to the nearest utility 
  if (case == 1) {
    possible.values <- comm.or.cc.table[com.or.cc.card.chosen, 6:7]
  } else {
    # case 2: advance to the nearest railroad 
    possible.values <- comm.or.cc.table[com.or.cc.card.chosen, 6:9]
  }
  diff.vector <- c()
  for (k in 1:length(possible.values)) {
    diff.vector[k] <- curr.spot - possible.values[k]
  }
  # only look at spots that are ahead and not 'behind' on the board, these are the nearest 
  # find the element in the diff.vector that corresponds to the least distance
  new.spot.element <- which(diff.vector %in% max(unlist(diff.vector[diff.vector < 0])))
  # use this element and get the actual square from the possible railroads vector
  new.spot <- possible.values[new.spot.element]
  # chance the current players position to this new spot 
  player.pos.table[curr.player, 'Square'] <<- new.spot
  
  # check if the new spot is owned by anybody 
  if (ownership.table[new.spot, 'Owner'] == 'NA') {
    # call the function to see if the player wants to buy it
    buying(player.curr.pos, bank.table, ownership.table, curr.player, player.curr.type)
  } else if (ownership.table[new.spot, 'Owner'] != curr.player) { # property owned by another player
    # case 1: (utility) rent is equal to 10 times the dice roll
    if (case == 1) {
      # call the function to throw the dice 
      dice.roll(2)
      # pay the owner an amount equal to 10 times the dice roll
      # change the current player's bank table
      bank.table[curr.player, 'Amount'] <<- bank.table[curr.player, 'Amount'] - (moves * comm.or.cc.table[com.or.cc.card.chosen, 'Pay.Amount'])
      # change the owner's bank table 
      # get the owner who owns this property 
      curr.owner <- ownership.table[new.spot, 'Owner']
      bank.table[curr.owner, 'Amount'] <<- bank.table[curr.owner, 'Amount'] + (moves * comm.or.cc.table[com.or.cc.card.chosen, 'Pay.Amount'])
    } else if (case == 2) {
      # case 2: (railroad) rent is equal to 2 times the appropriate rent 
      # get the owner who owns this property 
      curr.owner <- ownership.table[new.spot, 'Owner']
      # call the calculate rent function to find out the current rent of the property 
      rent.owed <- calculate.rent(ownership.table, curr.owner, player.curr.type)
      # change the current player's bank table
      bank.table[curr.player, 'Amount'] <<- bank.table[curr.player, 'Amount'] - (chance.selection[chance.card.chosen, 'Pay.Amount'] * rent.owed)
      # change the owner's bank table 
      bank.table[curr.owner, 'Amount'] <<- bank.table[curr.owner, 'Amount'] + (chance.selection[chance.card.chosen, 'Pay.Amount'] * rent.owed)
    }
  }
}

################ PART D: USER INPUTS ################

# define number of players in the game
no.players <- 4

# set number of turns per player you want
no.turns <- 100 

################ PART E: PLAY GAME ################

# initialize number of turns elapsed
no.turns.elapsed <- 0

while(no.turns.elapsed < no.turns || sum(player.pos.table[,'Amount'] == 0)) {
  for (curr.player in no.players) {
    player.pos.table <- move.player(curr.player, moves)
  }
  no.turns.elapsed <- no.turns.elapsed + 1
}





# temporarily store the player's current position 
player.curr.pos <- player.pos.table[1, 'Square']
# temporarily store the player's current position type 
player.curr.type <- rents[player.curr.pos, 'Type']
# temporarily store the owner of the property in which the player is currently on 
property.owner <- ownership.table[player.curr.pos, 'Owner']

# check what type of square the player landed on 

# check if the player landed on a property square
if (is.element(player.curr.pos, property.squares)) {
  # check if the property is vacant
  if (property.owner == "NA") {
    # call the function to update the bank and ownership tables based on user selection
    # if they would like to buy the vacant property 
    buying(player.curr.pos, bank.table, ownership.table, curr.player, player.curr.type)
  } else if (property.owner != curr.player) {
    # another player owns the property and the current player must pay the dues 
    rent.owed <- calculate.rent(ownership.table, property.owner, player.curr.type)
    # adjust the players bank account accordingly 
    bank.table[curr.player, 'Amount'] <- bank.table[curr.player, 'Amount'] - rent.owed 
  } else {
    # the current player owns the property and they may choose to add to their ownership
    # call the function to add homes or hotel 
    buying(player.curr.pos, bank.table, ownership.table, curr.player, player.curr.type)
  }
} else if (is.element(player.curr.pos, railroad.squares)) {
  # call the function to calculate how much rent is owed 
  rent.owed <- calculate.rent(ownership.table, property.owner, player.curr.type)
} else if (is.element(player.curr.pos, utilities.squares)) {
  # call the function to calculate how much rent is owed 
  rent.owed <- calculate.rent(ownership.table, property.owner, player.curr.type)
} else if (is.element(player.curr.pos, community.chest.squares)) {
  # initialize the type of card
  type.card <- 'Community.Chest'
  # randomly select a community chest card 
  # call function to randomly select a card from the pack 
  comm.chest.card.chosen <- pick.card(comm.chest.cards)
  # ensure that card chosen is not 0 which means there are no more community chest cards
  if (comm.chest.card.chosen > 0) {
    # initialize the group for the given card chosen 
    group <- groups[comm.chest.card.chosen , community.chest.squares]
    # execute the appropriate functions given the group 
    execute.functions(2, group, comm.chest.card.chosen, comm.chest)
  }
} else if (is.element(player.curr.pos, chance.squares)) {
  # randomly select a chance card 
  # call function to randomly select a card from the pack 
  chance.card.chosen <- pick.card(chance.cards)
  # decide what to do based on the card chosen 
  # ensure that card chosen is not 0 which means there are no more communiy chest cards
  if (chance.card.chosen > 0) {
    # initialize the group for the given card chosen 
    group <- groups[comm.chest.card.chosen , community.chest.squares]
    # execute the appropriate functions given the group 
    execute.functions(1, group, comm.chest.card.chosen, comm.chest)
  }
} else if (is.element(player.curr.position, taxes.squares)) {
  # player pays this amount to the bank. Deduct the amount from the user's bank account
  bank.table[curr.player, 'Amount'] <- bank.table[curr.player, 'Amount'] - 100 
}

# go to jail and getting out of jail 
# rolling doubles repeat turn 
# rolling doubles three times, go to jail 
# moving around the board and collecting 200 at go 
# handle 'moves' that are greater than 40 
# before rolling check if a player is in jail 
# put everything in a loop 
