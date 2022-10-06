erlang Supercollider wrapper.
=====


Erlang Supercollider wrapper based on [Server Command Reference](https://doc.sccode.org/Reference/Server-Command-Reference.html)


Build
-----
    $ rebar3 compile


Installation
-----

Just inject the dependency into your deps 
 
```erlang
{deps, [
	{sc,
	 {
	  git, "https://github.com/rexmalebka/sc_erl",{
		 branch, "main"
		}
	 }
	}
       ]}.
```

Usage
-----

```erlang

% create an OSC Client for SC communication
{ok, SC} = osc_client:connect(localhost, 57110).

% load a synthdef from file
sc:add_synthdef(SC, "priv/sequencer.scsyndef").

% create a synth
sc:add_synth(SC, "sequencer", 1003).

% set control of synth
sc:set_synth(SC, 1003, #{freq=> 200}).

% remove synth
sc:remove_synth(SC, 1003).

% add a buffer
sc:add_buffer(SC, "priv/sound.wav", 0).

% remove a buffer
sc:remove_buffer(SC, "priv/sound.wav", 0).
```

for further referencces please check the [wiki](https://github.com/rexmalebka/sc_erl/wiki)
