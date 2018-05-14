// Wait till the browser is ready to render the game (avoids glitches)
game_manager = null;

window.requestAnimationFrame(function () {
  log.setLevel(log.levels.DEBUG);
  game_manager = new GameManager(4, KeyboardInputManager, HTMLActuator);
});
