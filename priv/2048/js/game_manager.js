function GameManager(size, InputManager, Actuator) {
  this.size           = size; // Size of the grid
  this.inputManager   = new InputManager;
  this.actuator       = new Actuator;
  this.actuator.manager = this;
  this.game_connection = new GameConnection;
  this.game_connection.connectCallback = this.handleConnected.bind(this);
  this.game_connection.onVotes = this.handleVotesNotification.bind(this);
  this.game_connection.onMove = this.move.bind(this);
  this.game_connection.onBoard = this.updateBoard.bind(this);
  this.game_connection.onNewTiles = this.handleNewTiles.bind(this);
  this.game_connection.onGameEnd = this.gameEnd.bind(this);
  this.game_connection.onPlayerJoin = this.playerJoins.bind(this);
  this.game_connection.onPlayerLeave = this.playerLeaves.bind(this);
  this.game_connection.onPlayersSinceStart = this.handlePlayersSinceStart.bind(this);
  this.game_connection.onNickConflict = this.handleNickConflict.bind(this);
  this.game_connection.joinSuccess = this.joinSuccess.bind(this);

  this.result = null;

  this.startTiles = 2;

  this.namedPlayers = new Set();
  this.guestPlayers = 0;
  this.playersSinceStart = new Set();

  this.inputManager.on("move", this.vote.bind(this));

  this.game_connection.connect();
}

GameManager.prototype.isOver = function () {
  return this.result != null;
};

GameManager.prototype.joinWithNick = function(nick) {
    this.game_connection.joinRoom(nick);
}

GameManager.prototype.handleNickConflict = function() {
    alert('This nickname is already in use!');
}

GameManager.prototype.joinSuccess = function() {
    this.inputManager.enabled = true;
    this.actuator.hideNickForm();
    this.actuator.hideConnectionUI();
    this.actuator.showGameUI();
}

GameManager.prototype.handleConnected = function(state) {
    if (state == 1) {
        this.actuator.setConnectionStatus("Connecting...");
    } else if (state == 2) {
        this.actuator.setConnectionStatus("Connected!");
        this.actuator.showNickForm();
    } else {
        this.actuator.setConnectionStatus("Disconnected. Please try to refresh the page.");
        this.actuator.showConnectionUI();
        this.actuator.hideGameUI();
        this.inputManager.enabled = false;
    }
}

GameManager.prototype.playerJoins = function(nick, isGuest) {
    if(isGuest)
        this.guestPlayers++;
    else
        this.namedPlayers.add(nick);
    this.actuator.setPlayers(this.namedPlayers, this.guestPlayers, this.playersSinceStart);
}

GameManager.prototype.playerLeaves = function(nick, isGuest) {
    if(isGuest)
        this.guestPlayers--;
    else
        this.namedPlayers.delete(nick);
    this.actuator.setPlayers(this.namedPlayers, this.guestPlayers, this.playersSinceStart);
}

GameManager.prototype.gameEnd = function(result) {
    this.result = result;
    this.actuate();
}

GameManager.prototype.handleVotesNotification = function(votes) {
    this.actuator.setVotes(votes);
    this.actuator.setGameTime(parseInt(votes.gametime));
};

GameManager.prototype.updateBoard = function(board) {
  this.grid = new Grid(4, board);
  this.result = null;

  // Update the actuator
  this.actuate();
}

GameManager.prototype.handlePlayersSinceStart = function(players) {
    this.playersSinceStart = players;
    this.actuator.setPlayers(this.namedPlayers, this.guestPlayers, this.playersSinceStart);
}

GameManager.prototype.handleNewTiles = function(tiles) {
  for(var i = 0; i < tiles.length; i++)
      this.addTile(tiles[i].position, tiles[i].value);

  this.actuate();
};

// Adds a tile in a given position
GameManager.prototype.addTile = function (position, value) {
  var tile = new Tile(position, value);
  this.grid.insertTile(tile);
};

// Sends the updated grid to the actuator
GameManager.prototype.actuate = function () {
  this.actuator.actuate(this.grid, { result: this.result });
};

// Move a tile and its representation
GameManager.prototype.moveTile = function (tile, cell) {
  this.grid.cells[tile.x][tile.y] = null;
  this.grid.cells[cell.x][cell.y] = tile;
  tile.updatePosition(cell);
};

// Move tiles on the grid in the specified direction
GameManager.prototype.vote = function (direction) {
  // 0: up, 1: right, 2: down, 3: left

  if (this.isOver()) return; // Don't do anything if the game's over

  switch (direction) {
    case 0: this.setVote('up'); break;
    case 1: this.setVote('right'); break;
    case 2: this.setVote('down'); break;
    case 3: this.setVote('left'); break;
  }
};

GameManager.prototype.prepareTiles = function () {
  this.grid.eachCell(function (x, y, tile) {
    if (tile) {
      tile.mergedFrom = null;
      tile.savePosition();
    }
  });
};

GameManager.prototype.move = function (move) {
  // 0: up, 1: right, 2: down, 3: left
  var direction = null;
  switch(move) {
    case 'up': direction = 0; break;
    case 'right': direction = 1; break;
    case 'down': direction = 2; break;
    case 'left': direction = 3; break;
  };
  var self = this;

  if (this.isOver()) return; // Don't do anything if the game's over

  this.actuator.setPlayerVote('none');
  this.actuator.setVotes({ up: 0, left: 0, right: 0, down: 0 });

  var cell, tile;

  var vector     = this.getVector(direction);
  var traversals = this.buildTraversals(vector);
  var moved      = false;

  this.prepareTiles();

  // Traverse the grid in the right direction and move tiles
  traversals.x.forEach(function (x) {
    traversals.y.forEach(function (y) {
      cell = { x: x, y: y };
      tile = self.grid.cellContent(cell);

      if (tile) {
        var positions = self.findFarthestPosition(cell, vector);
        var next      = self.grid.cellContent(positions.next);

        // Only one merger per row traversal?
        if (next && next.value === tile.value && !next.mergedFrom) {
          var merged = new Tile(positions.next, tile.value * 2);
          merged.mergedFrom = [tile, next];

          self.grid.insertTile(merged);
          self.grid.removeTile(tile);

          // Converge the two tiles' positions
          tile.updatePosition(positions.next);

          // Update the score
          self.score += merged.value;

          // The mighty 2048 tile
          if (merged.value === 2048) self.won = true;
        } else {
          self.moveTile(tile, positions.farthest);
        }

        if (!self.positionsEqual(cell, tile)) {
          moved = true; // The tile moved from its original cell!
        }
      }
    });
  });

  if (moved)
    this.actuate();
};

GameManager.prototype.setVote = function (vote) {
    this.actuator.setPlayerVote(vote);
    this.game_connection.sendVote(vote);
}

// Get the vector representing the chosen direction
GameManager.prototype.getVector = function (direction) {
  // Vectors representing tile movement
  var map = {
    0: { x: 0,  y: -1 }, // Up
    1: { x: 1,  y: 0 },  // Right
    2: { x: 0,  y: 1 },  // Down
    3: { x: -1, y: 0 }   // Left
  };

  return map[direction];
};

// Build a list of positions to traverse in the right order
GameManager.prototype.buildTraversals = function (vector) {
  var traversals = { x: [], y: [] };

  for (var pos = 0; pos < this.size; pos++) {
    traversals.x.push(pos);
    traversals.y.push(pos);
  }

  // Always traverse from the farthest cell in the chosen direction
  if (vector.x === 1) traversals.x = traversals.x.reverse();
  if (vector.y === 1) traversals.y = traversals.y.reverse();

  return traversals;
};

GameManager.prototype.findFarthestPosition = function (cell, vector) {
  var previous;

  // Progress towards the vector direction until an obstacle is found
  do {
    previous = cell;
    cell     = { x: previous.x + vector.x, y: previous.y + vector.y };
  } while (this.grid.withinBounds(cell) &&
           this.grid.cellAvailable(cell));

  return {
    farthest: previous,
    next: cell // Used to check if a merge is required
  };
};

// Check for available matches between tiles (more expensive check)
GameManager.prototype.tileMatchesAvailable = function () {
  var self = this;

  var tile;

  for (var x = 0; x < this.size; x++) {
    for (var y = 0; y < this.size; y++) {
      tile = this.grid.cellContent({ x: x, y: y });

      if (tile) {
        for (var direction = 0; direction < 4; direction++) {
          var vector = self.getVector(direction);
          var cell   = { x: x + vector.x, y: y + vector.y };

          var other  = self.grid.cellContent(cell);

          if (other && other.value === tile.value) {
            return true; // These two tiles can be merged
          }
        }
      }
    }
  }

  return false;
};

GameManager.prototype.positionsEqual = function (first, second) {
  return first.x === second.x && first.y === second.y;
};
