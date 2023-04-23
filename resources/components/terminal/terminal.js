$('.terminal').terminal(function(command, term) {
  term.pause();
  term.echo("I can't do much yet.").resume();
}, {
  greetings: 'Welcome to `jake.isnt.online`.'
});
