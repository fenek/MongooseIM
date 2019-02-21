function HTMLActuator() {
  this.tileContainer    = document.querySelector(".tile-container");
  this.messageContainer = document.querySelector(".game-message");
  this.playerVoteContainer = document.querySelector(".player-vote");
  this.gameTimeContainer = document.querySelector(".game-time");
  this.totalPlayersContainer = document.querySelector(".total-players");
  this.guestPlayersContainer = document.querySelector(".guest-players");
  this.namedPlayersListContainer = document.querySelector("#player-list");
  this.highScoresContainer = document.querySelector("#high-scores");
  this.highScoresHead = document.querySelector("#high-scores-h");
  this.votesContainers = {
      up: document.querySelector(".up-votes"),
      down: document.querySelector(".down-votes"),
      left: document.querySelector(".left-votes"),
      right: document.querySelector(".right-votes")
  };

  this.manager = null;
  var self = this;

  $('input[name="yup"]').click(function() { self.manager.joinWithNick($('input[name="nick"]').val()); });
  $('input[name="nah"]').click(function() { self.manager.joinWithNick(null); });
}

HTMLActuator.prototype.actuate = function (grid, metadata) {
  var self = this;

  window.requestAnimationFrame(function () {
    self.clearContainer(self.tileContainer);

    grid.cells.forEach(function (column) {
      column.forEach(function (cell) {
        if (cell) {
          self.addTile(cell);
        }
      });
    });

    if(metadata.result != null)
      self.message(metadata.result == 'won');
    else
      self.clearMessage();
  });
};

HTMLActuator.prototype.hideConnectionUI = function() {
    $('#connection-ui').hide();
}

HTMLActuator.prototype.showConnectionUI = function() {
    $('#connection-ui').show();
}

HTMLActuator.prototype.showGameUI = function() {
    $('#game-ui').show();
}

HTMLActuator.prototype.hideGameUI = function() {
    $('#game-ui').hide();
}

HTMLActuator.prototype.showNickForm = function() {
    $('#nick-form').show();
}

HTMLActuator.prototype.hideNickForm = function() {
    $('#nick-form').hide();
}

HTMLActuator.prototype.setConnectionStatus = function(text) {
    $('#connection-status').text(text);
}

HTMLActuator.prototype.clearContainer = function (container) {
  while (container.firstChild) {
    container.removeChild(container.firstChild);
  }
};

HTMLActuator.prototype.zeroPad = function(val) {
    var zeroPad = '';
    if(val % 60 < 10)
        zeroPad = '0';
    return zeroPad + val;
};

HTMLActuator.prototype.updateScores = function (scores) {
    this.highScoresHead.style.display = 'block';
    this.clearContainer(this.highScoresContainer);
    scores.forEach(score => {
        var hours = Math.floor(score.time / 3600);
        var minutes = Math.floor(score.time / 60) % 60;
        var seconds = score.time % 60;
        var formattedTime = this.zeroPad(hours) + ':' + this.zeroPad(minutes) + ':' + this.zeroPad(seconds);
        var formattedPlayers = "only guests";
        if(score.nicks.length > 0) {
            formattedPlayers = '';
            score.nicks.forEach(nick => {
                formattedPlayers = formattedPlayers + ', ' + nick;
            });
            formattedPlayers = formattedPlayers.substring(2);
        }
        var scoreText = formattedTime + ' - ' + formattedPlayers;
        var liNode = document.createElement("li");
        var text = document.createTextNode(scoreText);
        liNode.appendChild(text);
        this.highScoresContainer.appendChild(liNode);
    });
}

HTMLActuator.prototype.setPlayers = function (namedPlayers, guestPlayers, playersSinceStart) {
    this.totalPlayersContainer.textContent = namedPlayers.size + guestPlayers;
    this.guestPlayersContainer.textContent = guestPlayers;
    var pList = this.namedPlayersListContainer;
    while (pList.firstChild) {
        pList.removeChild(pList.firstChild);
    }
    namedPlayersList = Array.from(namedPlayers);
    namedPlayersList.sort();
    namedPlayersList.forEach(pName => {
        var liNode = document.createElement("li");
        if(playersSinceStart.has(pName)) {
            var text = document.createTextNode(pName);
            var uNode = document.createElement("u");
            uNode.appendChild(text);
            liNode.appendChild(uNode);
        } else {
            var text = document.createTextNode(pName);
            liNode.appendChild(text);
        }
        pList.appendChild(liNode);
    });
}

HTMLActuator.prototype.setPlayerVote = function (vote) {
    this.playerVoteContainer.textContent = vote;
}

HTMLActuator.prototype.setVotes = function (votes) {
    this.votesContainers.up.textContent = votes.up;
    this.votesContainers.down.textContent = votes.down;
    this.votesContainers.left.textContent = votes.left;
    this.votesContainers.right.textContent = votes.right;
}

HTMLActuator.prototype.setGameTime = function (seconds) {
    var zeroPad = '';
    if(seconds % 60 < 10)
        zeroPad = '0';
    var formattedValue = Math.floor(seconds / 60) + ":" + zeroPad + (seconds % 60);
    this.gameTimeContainer.textContent = formattedValue;
}

HTMLActuator.prototype.addTile = function (tile) {
  var self = this;

  var wrapper   = document.createElement("div");
  var inner     = document.createElement("div");
  var position  = tile.previousPosition || { x: tile.x, y: tile.y };
  var positionClass = this.positionClass(position);

  // We can't use classlist because it somehow glitches when replacing classes
  var classes = ["tile", "tile-" + tile.value, positionClass];

  if (tile.value > 2048) classes.push("tile-super");

  this.applyClasses(wrapper, classes);

  inner.classList.add("tile-inner");
  inner.textContent = tile.value;

  if (tile.previousPosition) {
    // Make sure that the tile gets rendered in the previous position first
    window.requestAnimationFrame(function () {
      classes[2] = self.positionClass({ x: tile.x, y: tile.y });
      self.applyClasses(wrapper, classes); // Update the position
    });
  } else if (tile.mergedFrom) {
    classes.push("tile-merged");
    this.applyClasses(wrapper, classes);

    // Render the tiles that merged
    tile.mergedFrom.forEach(function (merged) {
      self.addTile(merged);
    });
  } else {
    classes.push("tile-new");
    this.applyClasses(wrapper, classes);
  }

  // Add the inner part of the tile to the wrapper
  wrapper.appendChild(inner);

  // Put the tile on the board
  this.tileContainer.appendChild(wrapper);
};

HTMLActuator.prototype.applyClasses = function (element, classes) {
  element.setAttribute("class", classes.join(" "));
};

HTMLActuator.prototype.normalizePosition = function (position) {
  return { x: position.x + 1, y: position.y + 1 };
};

HTMLActuator.prototype.positionClass = function (position) {
  position = this.normalizePosition(position);
  return "tile-position-" + position.x + "-" + position.y;
};

HTMLActuator.prototype.message = function (won) {
  var type    = won ? "game-won" : "game-over";
  var message = won ? "You win!" : "Game over!";
  message = message + ' Game will reset in a moment...';

  this.messageContainer.classList.add(type);
  this.messageContainer.getElementsByTagName("p")[0].textContent = message;
};

HTMLActuator.prototype.clearMessage = function () {
  // IE only takes one value to remove at a time.
  this.messageContainer.classList.remove("game-won");
  this.messageContainer.classList.remove("game-over");
};
