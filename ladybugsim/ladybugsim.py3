#!/usr/bin/env python3

# Written for Python 3 only

from math import sqrt
import random
import sys
import time

# Global Aphid counts. Their values shouldn't matter at this point
global TotalAphids
global AphidStarvations
TotalAphids = 0
AphidStarvations = []

def main():
    rseed = int(sys.argv[1])
    simVersion = sys.argv[2]
    nruns = int(sys.argv[3])
    nplayers = int(sys.argv[4])
    if TIMED:
        start_time = time.time()

    # outputs contains a list of tuples like:
    # (totalTurns, playerThatWon)
    outputs = []

    if simVersion == 'A':
        board = initBoard()

        random.seed(rseed)
        # Run some number of times
        for i in range(0, nruns):
            outputs.append(runSimulation(board, nplayers))
    if len(outputs) != nruns:
        print("The number of outputs was not equal to the number of runs!")


    nFirstPlayer = 0
    totalRuns = 0
    runSquares = 0
    # number of games stopped due to aphid starvations
    unfinished = 0
    for out in outputs:
        if out[1] == 0:
            nFirstPlayer += 1
        totalRuns += out[0]
        runSquares += (out[0]**2)
        if out[1] == -1:
            unfinished += 1
    if NICE:
        print("Total turns: %i" % totalRuns)
        print("Total first player: %i" % nFirstPlayer)
    fracFirstPlayer = nFirstPlayer / nruns
    avgRuns = totalRuns / nruns
    stdDev = sqrt((runSquares / nruns) - avgRuns**2)
    if NICE:
        print("OUTPUT Standard Deviation %f Average turns %f Fraction first player %f" % (stdDev, avgRuns, fracFirstPlayer))
    else:
        # Default for Hellman will be the simplest output
        print("OUTPUT %f %f %f" % (stdDev, avgRuns, fracFirstPlayer))

    if unfinished > 0:
        print("IMPORTANT: %i games were stopped before finishing (>100,000 requests for aphids without yields)" % unfinished)

    if INFO:
        print("Aphid starvations:")
        global AphidStarvations
        print(str(AphidStarvations))

    if TIMED:
        # time.time() returns a float for the number of seconds since epoch
        end_time = time.time()
        diff_times = end_time - start_time
        print("Total run time: %f s" % (diff_times))
        if NICE:
            print("Average run time: %f s per run" % (diff_times/nruns))


def runSimulation(gameBoard, nPlayers):
    players = initPlayers(nPlayers)
    deck = initDeck()
    # Initialize the number of global aphids (yes, it's in global scope, because lazy)
    global TotalAphids
    TotalAphids = 50
    global AphidStarvations
    AphidStarvations.append(0)

    playerAtEnd = -1
    # list of the players that have finished
    playersReachedEnd = []
    # start with the lowest number player, and cycle through in ascending order
    playersTurn = 0
    totalTurns = 0

    # Each player should play until someone has won
    # TODO Keep going until all players have finished, so that you count the number of turns they take
    #while playerAtEnd == -1:
    while len(playersReachedEnd) < nPlayers:
        
        # if this player has reached the end, just continue on to the next player
        if playersTurn in playersReachedEnd:
            playersTurn = (playersTurn + 1) % len(players)
            continue

        if AphidStarvations[-1] >= 100000:
            # Too many aphid requests with no yields. We should stop
            print("There were %i aphid starvations in this game. It's.... time to stop" % AphidStarvations[-1])
            #exit(-1)
            return (totalTurns, -1)

        if VERBOSE:
            #print("Player %i's turn. Let's see how many aphids are in play" % playersTurn)
            aphidsInPlay = 0
            pnum = 0
            for p in players:
                #print("Player %i has %i aphids" % (pnum, p.aphids))
                aphidsInPlay += p.aphids
                pnum += 1
            #print("%i aphids in play" % aphidsInPlay)

        totalTurns += 1
        if INFO:
            print("Player %i is making his/her turn" % playersTurn)
        players[playersTurn].do_turn(deck, gameBoard)


        if VERBOSE:
            print("Checking win-condition")
        # check if this player has won
        #print("Player %i is at %s, and the last board piece is %s" % (playersTurn, players[playersTurn].location, gameBoard[-1].order))
        if players[playersTurn].location == gameBoard[-1].order:
            if NICE:
                print("Player %i has reached the end!" % playersTurn)
            if playerAtEnd == -1:
                playerAtEnd = playersTurn
            # Add their aphids back into the pool
            TotalAphids += players[playersTurn].aphids
            playersReachedEnd.append(playersTurn)

        # Next player's turn
        playersTurn = (playersTurn + 1) % len(players)
    if NICE:
        print("Player %i won!" % playerAtEnd)
    return (totalTurns, playerAtEnd)

def initBoard():
    # Square initializaiton: 
    #   order
    #   next        -1= order+1
    #   previous    -1 = order-1
    #   aphid
    #   mantis
    #   spaces
    #   moves
    board = [
        # Start state
        Square(0, -1, 0),
        # first real square
        Square(1, -1, -1, 0, 1),
        Square(2, -1, -1),
        Square(3, -1, -1, 3),
        Square(4, -1, -1, 0, 1),
        Square(5, -1, -1),

        # VINE SQUARE
        Square(6, [7, 13] , -1),
        Square(7, -1, -1),
        Square(8, -1, -1, 0, 1),

        # MANTIS SQUARE
        Square(9, -1, -1, 0, -1),
        Square(10, -1, -1),
        Square(11, -1, -1),
        Square(12, -1, -1),
        # end of vine
        Square(13, -1, -1, 2),
        Square(14, -1, -1),
        Square(15, -1, -1, 5),
        Square(16, -1, -1),
        Square(17, -1, -1, -2),
        Square(18, -1, -1),
        Square(19, -1, -1),
        Square(20, -1, -1, 0, 0, -2),
        Square(21, -1, -1),
        Square(22, -1, -1),

        # ANT VALLEY JUNCTION
        Square(23, -1, -1),
        Square(24, -1, -1, 2),
        Square(25, -1, -1, -1),
        Square(26, -1, -1, 2),

        # ANT VALLEY DIVERSION
        Square(27, [35, 28], -1, -10),
        Square(28, -1, -1),
        Square(29, -1, -1, 1),
        Square(30, -1, -1),
        Square(31, -1, -1, 3),
        Square(32, -1, -1),
        Square(33, -1, -1, 0, 0, -2),
        # JOIN BACK AT JUNCTION
        Square(34, [23], -1),

        # AFTER ANT VALLEY DIVERSION
        Square(35, -1, 27),
        Square(36, -1, -1),

        # LOSE A TURN
        Square(37, -1, -1, 0, 0, 0, -1),
        Square(38, -1, -1),
        Square(39, -1, -1),
        Square(40, -1, -1),
        Square(41, -1, -1, 0, 0, 0, -1),
        Square(42, -1, -1),

        # END SQUARE
        Square(43, None, -1)
    ]
    return board

def initPlayers(nPlayers):
    players = []
    for i in range(0, nPlayers):
        players.append(Player())
    return players

def initDeck():
    cardList = []

    # get aphids card
    cardList.extend([Card(1, 0, 0, 0)])
    cardList.extend([Card(2, 0, 0, 0)])
    cardList.extend([Card(3, 0, 0, 0)]*2)
    cardList.extend([Card(4, 0, 0, 0)])

    # Move, and go agains
    cardList.extend([Card(0, 0, 2, 1)]*4)
    cardList.extend([Card(0, 0, 3, 1)]*3)
    cardList.extend([Card(0, 0, 4, 1)]*2)

    # Move cards
    cardList.extend([Card(0, 0, 1, 0)]*4)
    cardList.extend([Card(0, 0, 2, 0)]*4)
    cardList.extend([Card(0, 0, 3, 0)]*4)
    cardList.extend([Card(0, 0, 5, 0)]*2)
    cardList.extend([Card(0, 0, 6, 0)]*2)

    # go backs
    cardList.extend([Card(0, 0, -1, 0)])
    cardList.extend([Card(0, 0, -2, 0)])
    cardList.extend([Card(0, 0, -3, 0)])
    cardList.extend([Card(0, 0, -4, 1)]*2)

    return Deck(cardList)

class Action:
    def __init__(self, aphids=0, mantis=0, spaces=0, moves=0):
        self.aphids=aphids
        self.mantis=mantis
        self.spaces=spaces
        self.moves=moves

    def __repr__(self):
        return "Card with aphids=%i, mantis=%i, spaces=%i, and moves=%i" % (self.aphids, self.mantis, self.spaces, self.moves)

class Square(Action):
    # if next_square = None, that means it's the end square
    def __init__(self, order, next_square=-1, previous_square=None, aphids=0, mantis=0, spaces=0, moves=0):
        super().__init__(aphids, mantis, spaces, moves)
        if next_square == -1:
            next_square = [order+1]
        if previous_square == None or previous_square == -1:
            previous_square = order-1
        self.order=order
        self.next_square=next_square
        self.previous_square=previous_square

    # A player has landed on a square
    # Will return the number of extra moves the player gets
    def handle_land(self, player, board):
        current_square = self
        #print("Handling landing on a square")

        # landed on the mantis square, which will either take a mantis, or send the player back to the start
        if current_square.mantis == -1:
            if INFO:
                print("There's an interrogation being led by mantises")
            if player.mantis > 0:
                player.mantis -= 1
                player.moves_left += 1
                if INFO:
                    print("BUG OFF!!!!")
            else:
                player.location = board[0].order
                player.next_space = board[0].next_square[0]
            # not-required, but could be an addition spot: mantis spot has more attribute changes
        # landed on the ants square
        elif current_square.aphids == -10:
            if INFO:
                print("Interaction with the ants!")
            if player.aphids >= 10:
                # aphids collected by the ants go back into the aphid pool
                incrementAphids(player, -10)
                player.moves_left += 1
            else:
                # the player will move to the last option. (It's assumed that there will only be two choices
                # the last option is the diversion (long way)
                player.next_space = current_square.next_square[-1]
        # Landed on the tarzan vine
        elif current_square.next_square is not None and len(current_square.next_square) == 2:
            if INFO:
                print("Someone gets to be Tarzan! Yay!")
            player.location = current_square.next_square[-1]
            nextSquare = None
            for sqr in board:
                if sqr.order == player.location:
                    nextSquare = sqr
            if nextSquare is None:
                print("DIDN'T FIND A SQUARE OH MY GOD AHHHH")
            player.next_space = nextSquare.next_square[0]
            nextSquare.handle_land(player, board)
        else:
            incrementAphids(player, current_square.aphids)
            player.mantis += current_square.mantis
            player.moves_left += current_square.moves
            if current_square.spaces != 0:
                player.move(board, current_square.spaces)
        #print("Done handling the square")


# Global function to do aphids
def incrementAphids(player, nAphids):
    global TotalAphids
    global AphidStarvations
    #print("Requesting %i aphids. Total left: %i", nAphids)
    if TotalAphids >= nAphids:
        # we need to make sure we only decrement down to 0
        if nAphids < -1 * player.aphids:
            TotalAphids += player.aphids
            player.aphids = 0
        else:
            # Increment as normal
            player.aphids += nAphids
            TotalAphids -= nAphids
    else:
        # They are trying to take more aphids then there are. Just give them the rest.
        if VERBOSE:
            print("Someone wanted %i aphids, but there were only %i" % (nAphids, TotalAphids))
        player.aphids += TotalAphids
        TotalAphids = 0
        AphidStarvations[-1] += 1

class Card(Action):
    def __init__(self, aphids=0, mantis=0, spaces=0, moves=0):
        super().__init__(aphids, mantis, spaces, moves)

    def handle_draw(self, player, board):
        player.mantis += self.mantis
        incrementAphids(player, self.aphids)
        player.moves_left += self.moves
        player.move(board, self.spaces)

class Player:
    def __init__(self, location=0, aphids=0, mantis=0):
        self.location=location
        self.aphids=aphids
        self.mantis=mantis
        self.moves_left = 0

        # Assumes that the next space players will move to is the index 1
        # This will mainly be used in the decision making (splits)
        self.next_space = 1

    def do_turn(self, deck, board):
        #print("Starting turn")
        self.moves_left += 1
        while self.moves_left > 0:
            #print("Player has %i moves left" % self.moves_left)
            self.moves_left -= 1
            #print("Player still has %i moves left" % self.moves_left)
            draw_card = deck.draw()
            if VERBOSE:
                print("Drew card: %s" % str(draw_card))
            draw_card.handle_draw(self, board)
            if self.next_space == None:
                if VERBOSE:
                    print("Player is declaring that they have won. It's possible that they were trying to move again, so they may foreit the turn")
                break
        #print("ending turn")

    # TODO: If you are going to overshoot the end, stay where you are
    def move(self, board, spaces):
        if INFO:
            print("Moving a player %i spaces" % spaces)
        if spaces == 0:
            return
        if self.location + spaces > board[-1].order:
            # this move will go PAST the end square. Damn.
            if INFO:
                print("A player needs to wait again, to try and end on the final piece perfectly")
            if VERBOSE:
                print("Location: %i" % self.location, "Spaces: %i" % spaces, "End position: %i", board[-1].order)
            return

        current_square = None
        for square in board:
            if square.order == self.location:
                current_square = square
                break

        if current_square == None:
            print("The player's current position is outside the gameboard. Wtf, mate?")
            exit(-1)
        while spaces != 0:
            if spaces < 0:
                #print("Moving backwards")
                spaces += 1
                # Since there are no branches for previous squares, there is no need to do the verification like with going forward
                self.next_square = self.location
                self.location = current_square.previous_square
                #print("Previous square: %s" % current_square.previous_square)
                if self.location < 0:
                    # reached the first space on the board. We can stop moving backwards now
                    self.location = 0
                    spaces = 0
            elif spaces > 0:
                spaces -= 1
                # verify that the target of the player is an option in the board
                if current_square.next_square is not None and self.next_space not in current_square.next_square:
                    print("The player's target square is not an option. Targetting %i, but can only choose %s" % (self.next_space, str(current_square.next_square)))
                    print("Player is at: %i" % self.location, "Player target: %i" % self.next_space, "Current Square: %i" % current_square.order, "Next Square: %s" % current_square.next_square, "Previous Square: %i" % current_square.previous_square)
                    # So just end
                    exit(-1)

                self.location = self.next_space

            # Updated the player's new location
            for square in board:
                if square.order == self.location:
                    current_square = square
                    break
            # Player's next target defaults to the first optional square (which is USUALLY right). If it's not, then handle_land() will correct it
            if current_square.next_square != None:
                self.next_space = current_square.next_square[0]
            else:
                self.next_space = None
            # Stop moving if the player lands on mantis or ants
            if current_square.aphids == -10 or current_square.mantis == -1:
                spaces = 0
            if self.next_space is None:
                if INFO:
                    print("Some player probably won. They want to move to None next")
                spaces = 0
                break
            elif self.next_space > len(board):
                spaces = 0
                if INFO:
                    print("A player has moved outside the board. Maybe they won")
                break
        current_square.handle_land(self, board)


class Deck:
    cards = []
    discard = []

    def __init__(self, cardList):
        if INFO:
            print("Made a deck consisting of %i cards" % len(cardList))
        self.cards = cardList
        random.shuffle(self.cards)

    def draw(self):
        if len(self.cards) == 0:
            if INFO:
                print("Shuffling the deck")
            self.cards = self.discard
            self.discard = []
            random.shuffle(self.cards)

        # pop the last item in the last off, and return it, but also put it in the discard pile
        self.discard.append(self.cards.pop())
        return self.discard[-1]


# Shows things that are nice to know
NICE=False
if '-n' in sys.argv:
    NICE=True

# Shows things that aren't necesarry, but MAYBE useful
INFO=False
if '-i' in sys.argv:
    NICE=True
    INFO=True


# Include every single print statement possible
VERBOSE=False
if '-v' in sys.argv:
    NICE=True
    INFO=True
    VERBOSE=True

TIMED=False
if '-t' in sys.argv:
    TIMED=True


main()
