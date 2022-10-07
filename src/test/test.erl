-module(test).

-export([
	main/0
	]).


main()->

	synthdef:load("/home/rexmalebka/Documentos/erlang/scotp/priv/supercollider/sequencer.scsyndef"),

	buffer:load("/home/rexmalebka/Documentos/erlang/scotp/priv/soundbank/hh/hh.wav", 1),
	buffer:load("/home/rexmalebka/Documentos/erlang/scotp/priv/soundbank/hh/hh2.wav", 2),
	buffer:load("/home/rexmalebka/Documentos/erlang/scotp/priv/soundbank/hh/hh3.wav", 3),
	buffer:load("/home/rexmalebka/Documentos/erlang/scotp/priv/soundbank/hh/hh4.wav", 4),
	buffer:load("/home/rexmalebka/Documentos/erlang/scotp/priv/soundbank/hh/hh7.wav", 5),
	buffer:load("/home/rexmalebka/Documentos/erlang/scotp/priv/soundbank/hh/hh6.wav", 6),
	buffer:load("/home/rexmalebka/Documentos/erlang/scotp/priv/soundbank/hh/hh5.wav", 7),
	buffer:load("/home/rexmalebka/Documentos/erlang/scotp/priv/soundbank/hh/hh1.wav", 8),
	buffer:load("/home/rexmalebka/Documentos/erlang/scotp/priv/soundbank/kick/kick.wav",9),
	buffer:load("/home/rexmalebka/Documentos/erlang/scotp/priv/soundbank/kick/kick5.wav",10),


	buffer:alloc(11, 1024*2, 2),

	synth:load("sequencer", 666 , #{bufnum => 11}),
	
	buffer:alloc(12, 1024*2, 2),

	synth:load("sequencer", 667 , #{bufnum => 11}),
	synth:load("sequencer", 668 , #{bufnum => 11}),
	synth:load("sequencer", 669 , #{bufnum => 11}),


	ok.


