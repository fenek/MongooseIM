// Copyright 2018 by Piotr Nosek

function GameConnection() {
    this.WS_ENDPOINT = 'ws://127.0.0.1:5280/ws-xmpp';
    this.connection = null;
    this.NS2048 = 'urn:xmpp:2048';
    this.NS2048_VOTE = this.NS2048 + '#vote';
    this.NS2048_VOTES = this.NS2048 + '#votes';
    this.NS2048_MOVE = this.NS2048 + '#move';
    this.NS2048_BOARD = this.NS2048 + '#board';
    this.NS2048_PLAYERS_SINCE_START = this.NS2048 + '#players-since-start';
    this.NS2048_NEW_TILES = this.NS2048 + '#new-tiles';
    this.NS2048_WON = this.NS2048 + '#won';
    this.NS2048_LOST = this.NS2048 + '#lost';
    this.NS2048_SCORES = this.NS2048 + '#scores';

    this.connectCallback = null;
    this.onNickConflict = null;
    this.onVotes = null;
    this.onMove = null;
    this.onBoard = null;
    this.onNewTiles = null;
    this.onGameEnd = null;
    this.onPlayerJoin = null;
    this.onPlayerLeave = null;
    this.onPlayersSinceStart = null;
    this.joinSuccess = null;
    this.onScores = null;

    this.nick = '';
    this.isConnected = false;
}

GameConnection.prototype.rawInput = function (data) {
    log.debug('RECV ' + data);
}

GameConnection.prototype.rawOutput = function (data) {
    log.debug('SENT ' + data);
}

GameConnection.prototype.onConnect = function (status) {
    if (status == Strophe.Status.CONNECTING) {
        this.connectCallback(1);
        log.info('Strophe is connecting.');
    } else if (status == Strophe.Status.CONNFAIL) {
        this.connectCallback(3);
        this.isConnected = false;
        this.nick = '';
        log.error('Strophe failed to connect.');
    } else if (status == Strophe.Status.DISCONNECTING) {
        log.info('Strophe is disconnecting.');
    } else if (status == Strophe.Status.DISCONNECTED) {
        this.connectCallback(3);
        this.nick = '';
        this.isConnected = false;
        log.info('Strophe is disconnected.');
    } else if (status == Strophe.Status.CONNECTED) {
        log.info('Strophe is connected.');
        this.isConnected = true;

        this.connection.addHandler(this.gameUpdateHandler.bind(this),
                                   this.NS2048, 'message', 'groupchat', null,
                                   'game2048@muc.localhost',
                                   {'ignoreNamespaceFragment': true});
        this.connection.addHandler(this.playerPresenceHandler.bind(this),
                                   null, 'presence', null, null,
                                   'game2048@muc.localhost',
                                   {'matchBareFromJid': true});

        this.connectCallback(2);
    }
}

GameConnection.prototype.gameUpdateHandler = function (msg) {
    var xElement = msg.getElementsByTagName('x')[0];
    switch (xElement.getAttribute("xmlns")) {
        case this.NS2048_VOTES: this.parseVotesAndNotify(xElement); break;
        case this.NS2048_MOVE: this.parseMoveAndNotify(xElement); break;
        case this.NS2048_BOARD: this.parseBoardAndNotify(xElement); break;
        case this.NS2048_PLAYERS_SINCE_START: this.parsePlayersSinceStartAndNotify(xElement); break;
        case this.NS2048_NEW_TILES: this.parseNewTilesAndNotify(xElement); break;
        case this.NS2048_WON: this.onGameEnd('won'); break;
        case this.NS2048_LOST: this.onGameEnd('lost'); break;
        case this.NS2048_SCORES: this.parseScoresAndNotify(xElement); break;
    }

    return true;
}

GameConnection.prototype.playerPresenceHandler = function (presence) {
    if(presence.getAttribute('type') == 'error') {
        this.onNickConflict();
        return;
    }

    var fromJID = presence.getAttribute('from');
    var rawNick = Strophe.getResourceFromJid(fromJID);

    if(rawNick == this.nick) {
        this.joinSuccess();
    }

    var isGuest = false;
    var nick = rawNick;
    if(rawNick.startsWith('--GUEST--')) {
        isGuest = true;
        nick = rawNick.substring(9);
    }
    if((presence.hasAttribute('type') == false) || (presence.getAttribute('type') == 'available')) {
        this.onPlayerJoin(nick, isGuest);
    } else if(presence.getAttribute('type') == 'unavailable') {
        this.onPlayerLeave(nick, isGuest);
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

GameConnection.prototype.parsePlayersSinceStartAndNotify = function(xElement) {
    var children = xElement.children;
    var players = new Set();
    for(var i = 0; i < children.length; i++)
        players.add(children[i].textContent);
    this.onPlayersSinceStart(players);
}

GameConnection.prototype.parseScoresAndNotify = function(xElement) {
    var children = xElement.children;
    scores = [];
    for(var i = 0; i < children.length; i++) {
        scores[i] = { time: 0, nicks: [] };
        scores[i].time = parseInt(children[i].getAttribute('time'));
        nicks = [];
        for(var j = 0; j < children[i].children.length; j++)
            nicks[j] = children[i].children[j].textContent;
        scores[i].nicks = nicks;
    }
    this.onScores(scores);
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


GameConnection.prototype.joinRoom = function(nick) {
    if(nick == null)
        nick = '--GUEST--' + Strophe.getNodeFromJid(this.connection.jid);
    this.nick = nick;
    var presence = $pres({type: 'available', to: 'game2048@muc.localhost/' + nick});
    this.connection.sendPresence(presence);
}

GameConnection.prototype.sendVote = function (voteText) {
    var iq = $iq({type: 'set', to: 'game2048@muc.localhost'});
    var query = iq.c('query', {xmlns: this.NS2048_VOTE});
    query.c('vote', {}, voteText);
    this.connection.sendIQ(iq);
}

GameConnection.prototype.connect = function() {
    if(this.isConnected)
        return;
    if(this.connection == null) {
        this.connection = new Strophe.Connection(this.WS_ENDPOINT);
        this.connection.rawInput = this.rawInput.bind(this);
        this.connection.rawOutput = this.rawOutput.bind(this);
    }
    this.connection.connect("localhost", "", this.onConnect.bind(this));
};

