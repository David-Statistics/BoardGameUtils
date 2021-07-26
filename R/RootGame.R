#' Generate the setup for a new game of Root
#'
#' Useful utility for getting a new game of Root started using an Advanced
#' setup draft. Tools are provided for choosing random maps, decks, and factions
#' as well as randomizing the clearings for any map (though Autumn requires a
#' bit of a hack).
#'
#' @import R6
#'
RootGame <- R6::R6Class("RootGame",
  public = list(
    #' @field map The map of the game
    map = NULL,

    #' @field deck The deck of the game
    deck = NULL,

    #' @field factions The factions available for a draft
    factions = NULL,

    #' @field available_factions The factions available to be picked
    available_factions = NULL,

    #' @field picked_factions The factions already picked in a draft
    picked_factions = NULL,

    #' @field players The *number* of players in a game
    players = NULL,

    #' @field vagabonds The vagabonds available for a draft
    vagabonds = NULL,

    #' @field roster A vector with the names of the players
    roster = NULL,

    #' @field draft_order A vector with the names of the players in draft order
    #' (first element gets first draft)
    draft_order = NULL,

    #' @field draft_order A vector with the names of the players in play order
    #' (first element gets first turn). This is the reverse of `draft_order`
    play_order = NULL,

    #' @field verbose A boolean to control console output (defaults to `TRUE`
    #' which allows for an interactive feel for the draft)
    verbose = NULL,

    #' @field verbose A vector of clearing suits (first initial abbeviations to
    #' aid with console output)
    clearings = NULL,

    #' @description
    #' Create a new Root game object
    #' @param map Map of the game
    #' @param deck Deck of the game
    #' @param to_exclude Factions to exclude from the draft
    #' @param players Number of player (overwritten by the length of `roster`)
    #' @param vagabonds_to_exclude Vagabond characters to exclude from the draft
    #' @param roster The names of the players in the game
    #' @param verbose Boolean to control console output
    #' @return A new `RootGame` object
    initialize = function(map = NA, deck = NA,
                          to_exclude = NA, players = 4,
                          vagabonds_to_exclude = NA,
                          roster = NA, verbose = TRUE) {
      self$map <- map
      self$deck <- deck
      if(all(is.na(roster))) {
        self$players <- players
      } else {
        self$players <- length(roster)
      }
      self$roster <- roster
      self$verbose <- verbose
      self$factions <- setdiff(c('Marquise de Cat',
                                 'Eyrie Dynasties',
                                 'Underground Duchy',
                                 'Woodland Alliance',
                                 'Vagabond',
                                 'Lizard Cult',
                                 'Riverfolk Company',
                                 'Corvid Conspiracy'),
                               to_exclude)
      self$vagabonds = setdiff(c('Thief', 'Ranger', 'Tinker',
                                 'Vagrant', 'Arbiter', 'Scoundrel',
                                 'Adventurer', 'Ronin', 'Harrier'),
                               vagabonds_to_exclude)
    },
    #' @description
    #' Sets the draft and turn order of the players (`roster` must be a valid
    #' vector of players)
    set_player_order = function() {
      self$play_order <- sample(self$roster)
      self$draft_order <- rev(self$play_order)
      invisible(self)
    },
    #' @description
    #' Creates a valid Advanced Setup draft of factions
    deal_factions = function() {
      red_factions <- intersect(c('Marquise de Cat',
                                  'Eyrie Dynasties',
                                  'Underground Duchy'),
                                self$factions)
      if(length(red_factions) == 0) {
        stop('No red factions available. This use case is outside the bounds of AdSet')
      }
      if(length(self$factions) <= self$players) {
        stop('Not enough factions available.')
      }
      private$make_available(sample(red_factions, 1))
      while(length(self$available_factions) <= self$players) {
        private$make_available(sample(self$factions, 1))
      }
      if(self$verbose) {
        if(length(self$roster) == self$players) {
          if(is.null(self$draft_order)) self$set_player_order()
          private$annouce_draft()
        }
      }
      invisible(self)
    },
    #' @description
    #' An interactive function for going through the draft; accepts either the
    #' fully spelled out faction name or the index of the faction. If the last
    #' faction is white, it will not allow it to be chosen until a red faction
    #' has been chosen.
    pick = function(faction) {
      if(class(faction) == 'numeric') {
        faction <- self$available_factions[faction]
      }
      if(sum(c('Marquise de Cat',
               'Eyrie Dynasties',
               'Underground Duchy') %in%
             self$picked_factions) == 0) {
        if(faction == self$available_factions[length(self$available_factions)]) {
          stop('Last faction locked until a Red faction is chosen.')
        }
      }
      self$available_factions <- setdiff(self$available_factions,
                                         faction)
      self$picked_factions <- c(self$picked_factions,
                                faction)
      if(length(self$roster) == self$players) {
        names(self$picked_factions)[length(self$picked_factions)] <- self$draft_order[length(self$picked_factions)]
        if(self$verbose) {
          if(length(self$picked_factions) == self$players) {
            private$annouce_game()
          } else {
            private$annouce_draft()
          }
        }
      }
      invisible(self)
    },
    #' @description
    #' Simple utility for chosing a random map
    random_map = function() {
      self$map <- sample(c('Autumn', 'Winter', 'Lake', 'Mountain'), 1)
      if(self$verbose) {
        cat(paste0('\nMap is ', self$map, '\n'))
      }
      invisible(self)
    },
    #' @description
    #' Simple utility for chosing a random deck
    random_deck = function() {
      self$deck <- sample(c('Original', 'Exiles & Partisans'), 1)
      if(self$verbose) {
        cat(paste0('\nDeck is ', self$deck, '\n'))
      }
      invisible(self)
    },
    #' @description
    #' Simple utility for setting the clearings of a map
    set_clearings = function(force = FALSE) {
      if(is.na(self$map)) {
        stop('Map is not set yet')
      }
      if(self$map == 'Autumn' & !force) {
        self$clearings = c('F', 'R', 'M', 'R', 'R', 'M', 'F', 'M', 'F', 'R', 'F', 'M', 'R')
      } else {
        self$clearings = sample(c(rep('F', 4), rep('R', 4), rep('M', 4)))
      }
      if(self$verbose) {
        self$show_clearings()
      }
      invisible(self)
    },
    #' @description
    #' Utility for printing the clearings to the console in a reasonable way
    show_clearings = function() {
      if(is.na(self$map) | is.null(self$clearings)) {
        stop('Map and/or clearings not set yet.')
      }
      x = self$clearings
      if(self$map == 'Autumn') {
        cat(paste0(x[1], '     ', x[2], '     ', x[3], '\n',
                   '    ', x[4], '        \n',
                   x[5], '  ', x[6], '    ', x[7], '   ', x[8], '\n',
                   x[9], '  ', x[10], '   ', x[11], '    ', x[12]))
      } else if (self$map == 'Winter') {
        cat(paste0(paste0(x[1:4], collapse = '   '), '\n',
                   paste0(x[5:8], collapse = '   '), '\n',
                   paste0(x[9:12], collapse = '   ')))
      } else if (self$map == 'Lake') {
        cat(paste0(x[1], '   ', x[2], '   ', x[3], '   ', x[4], '\n',
                   '  ', x[5], '          \n',
                   x[6], '       ', x[7], '   ', x[8], '\n',
                   '  ', x[9], '          \n',
                   x[10], '   ', x[11], '       ', x[12]))
      } else if (self$map == 'Mountain') {
        cat(paste0(x[1], '      ', x[2], '   ', x[3], '\n',
                   '   ', x[4], '  ', x[5], '     \n',
                   x[6], '   ', x[7], '  ', x[8], '   ', x[9], '\n',
                   x[10], '     ', x[11], '    ', x[12]))
      } else {
        stop('Map not recognized')
      }
      invisible(self)
    },
    #' @description
    #' Wrapper function that will choose a map & deck, set clearings, and start
    #' up a draft.
    start_new_game = function(map = 'random', deck = 'random', force = TRUE) {
      if(tolower(map) == 'random') {
        self$random_map()
      } else {
        self$map = map
      }
      if(is.null(self$clearings) | force) {
        self$set_clearings()
      }
      cat('\n')
      if(tolower(deck) == 'random') {
        self$random_deck()
      } else {
        self$deck = deck
      }
      self$set_player_order()
      cat(paste0('\nPick order: ', paste0(self$draft_order, collapse = ', '), '\n\n'))
      self$deal_factions()
    }
  ),
  private = list(
    #' @description
    #' Double vagabond is unfortunately a thing that can happen and regardless
    #' we still need to choose random vagabond characters for any vagabonds in
    #' the draft so we need a helper function for making a faction available to
    #' the draft.
    make_available = function(faction) {
      if(faction != 'Vagabond') {
        self$factions <- setdiff(self$factions, faction)
        self$available_factions <- c(self$available_factions,
                                     faction)
      } else {
        vb <- sample(self$vagabonds, 1)
        self$vagabonds <- setdiff(self$vagabonds, vb)
        if(any(grepl('Vagabond', self$available_factions))) {
          self$factions <- setdiff(self$factions, faction)
        }
        self$available_factions <- c(self$available_factions,
                                     paste0(faction, ' - ', vb))
      }
      invisible(self)
    },
    #' @description
    #' Function for making the draft at least feel interactive which announces
    #' the player currently picking and the available faction choises
    annouce_draft = function() {
      current_pick <- self$draft_order[length(self$picked_factions) + 1]
      factions <- paste0(paste0(seq_along(self$available_factions), ': ',
                                self$available_factions), collapse = '\n')
      cat(paste0(current_pick, ', please choose a faction:\n', factions, '\n'))
    },
    #' @description
    #' Function that summarizes the setup - prints map, clearings, deck, turn
    #' order, and faction choices to the console
    annouce_game = function() {
      if(length(self$roster) == self$players) {
        players <- paste0(paste0('\t', rev(names(self$picked_factions)), ': ', rev(self$picked_factions)),
                          collapse = '\n')
      } else {
        players = paste0(paste0('\t', seq_along(self$picked_factions), ': ', rev(self$picked_factions)),
                         collapse = '\n')
      }
      cat(paste0('The war of The Woodland will take place with the following setup:\nMap: ', self$map, '\n'))
      self$show_clearings()
      cat(paste0('\nDeck: ', self$deck,
                 '\nTurn order and factions:\n',
                 players))
      invisible(self)
    }
  )
)
