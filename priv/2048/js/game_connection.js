// Copyright 2018 by Piotr Nosek

function GameConnection() {
    this.WS_ENDPOINT = 'ws://192.168.0.102:5280/ws-xmpp';
    this.connection = null;
    this.NS2048 = 'urn:xmpp:2048';
    this.NS2048_VOTE = this.NS2048 + '#vote';
    this.NS2048_VOTES = this.NS2048 + '#votes';
    this.NS2048_MOVE = this.NS2048 + '#move';
    this.NS2048_BOARD = this.NS2048 + '#board';
    this.NS2048_NEW_TILES = this.NS2048 + '#new-tiles';
    this.NS2048_WON = this.NS2048 + '#won';
    this.NS2048_LOST = this.NS2048 + '#lost';

    this.onVotes = null;
    this.onMove = null;
    this.onBoard = null;
    this.onNewTiles = null;
    this.onGameEnd = null;
}

GameConnection.prototype.rawInput = function (data) {
    log.debug('RECV ' + data);
}

GameConnection.prototype.rawOutput = function (data) {
    log.debug('SENT ' + data);
}

GameConnection.prototype.onConnect = function (status) {
    if (status == Strophe.Status.CONNECTING) {
        log.info('Strophe is connecting.');
    } else if (status == Strophe.Status.CONNFAIL) {
        log.error('Strophe failed to connect.');
    } else if (status == Strophe.Status.DISCONNECTING) {
        log.info('Strophe is disconnecting.');
    } else if (status == Strophe.Status.DISCONNECTED) {
        log.info('Strophe is disconnected.');
    } else if (status == Strophe.Status.CONNECTED) {
        log.info('Strophe is connected.');

        this.connection.addHandler(this.gameUpdateHandler.bind(this),
                                   this.NS2048, 'message', 'groupchat', null,
                                   'game2048@muc.localhost',
                                   {'ignoreNamespaceFragment': true});

        this.joinRoom();
    }
}

GameConnection.prototype.gameUpdateHandler = function (msg) {
    var xElement = msg.getElementsByTagName('x')[0];
    switch (xElement.getAttribute("xmlns")) {
        case this.NS2048_VOTES: this.parseVotesAndNotify(xElement); break;
        case this.NS2048_MOVE: this.parseMoveAndNotify(xElement); break;
        case this.NS2048_BOARD: this.parseBoardAndNotify(xElement); break;
        case this.NS2048_NEW_TILES: this.parseNewTilesAndNotify(xElement); break;
        case this.NS2048_WON: this.onGameEnd('won'); break;
        case this.NS2048_LOST: this.onGameEnd('lost'); break;
    }

    return true;
}

GameConnection.prototype.parseVotesAndNotify = function(xElement) {
    var children = xElement.children;
    var votes = { up: 0, down: 0, left: 0, right: 0 };

    for(var i = 0; i < children.length; i++) {
        var child = children[i];
        var option = child.nodeName;
        var count = child.textContent;
        votes[option] = count;
    }

    this.onVotes(votes);
}

GameConnection.prototype.parseMoveAndNotify = function(xElement) {
    this.onMove(xElement.children[0].nodeName);
}

GameConnection.prototype.parseBoardAndNotify = function(xElement) {
    var children = xElement.children;
    var board = [];
    for(var x = 0; x < 4; x++) {
        board[x] = [];
        for(var y = 0; y < 4; y++)
            board[x][y] = false;
    }
    
    for(var i = 0; i < children.length; i++) {
        var child = children[i];
        var x = parseInt(child.getAttribute('x')) - 1;
        var y = parseInt(child.getAttribute('y')) - 1;
        var value = child.textContent;
        if(value == 'null')
            board[x][y] = false;
        else
            board[x][y] = { position: { x: x, y: y }, value: parseInt(value) };
    }

    this.onBoard(board);
}

GameConnection.prototype.parseNewTilesAndNotify = function(xElement) {
    var children = xElement.children;
    var newTiles = [];
    
    for(var i = 0; i < children.length; i++) {
        var child = children[i];
        var x = parseInt(child.getAttribute('x')) - 1;
        var y = parseInt(child.getAttribute('y')) - 1;
        var value = child.textContent;
        newTiles.push({ position: { x: x, y: y }, value: parseInt(value) });
    }

    this.onNewTiles(newTiles);
}


GameConnection.prototype.joinRoom = function() {
    var username = Strophe.getNodeFromJid(this.connection.jid);
    var presence = $pres({type: 'available', to: 'game2048@muc.localhost/' + username});
    this.connection.sendPresence(presence);
}

GameConnection.prototype.sendVote = function (voteText) {
    var iq = $iq({type: 'set', to: 'game2048@muc.localhost'});
    var query = iq.c('query', {xmlns: this.NS2048_VOTE});
    query.c('vote', {}, voteText);
    this.connection.sendIQ(iq);
}

GameConnection.prototype.connect = function() {
    this.connection = new Strophe.Connection(this.WS_ENDPOINT);
    this.connection.rawInput = this.rawInput.bind(this);
    this.connection.rawOutput = this.rawOutput.bind(this);
    this.connection.connect("localhost", "", this.onConnect.bind(this));
};

