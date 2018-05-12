// Wait till the browser is ready to render the game (avoids glitches)
window.requestAnimationFrame(function () {
  log.setLevel(log.levels.DEBUG);
  new GameManager(4, KeyboardInputManager, HTMLActuator);
});
