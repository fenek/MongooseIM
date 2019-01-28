function HTMLActuator() {
  this.tileContainer    = document.querySelector(".tile-container");
  this.messageContainer = document.querySelector(".game-message");
  this.playerVoteContainer = document.querySelector(".player-vote");
  this.gameTimeContainer = document.querySelector(".game-time");
  this.votesContainers = {
      up: document.querySelector(".up-votes"),
      down: document.querySelector(".down-votes"),
      left: document.querySelector(".left-votes"),
      right: document.querySelector(".right-votes")
  };
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

HTMLActuator.prototype.clearContainer = function (container) {
  while (container.firstChild) {
    container.removeChild(container.firstChild);
  }
};

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
