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
synthdef:load(SC, "priv/sequencer.scsyndef").

% create a synth
synth:load(SC, "sequencer", 1003).

% set control of synth
synth:set(SC, 1003, #{freq=> 200}).

% remove synth
synth:remove(SC, 1003).

% add a buffer
buffer:load(SC, "priv/sound.wav", 0).

% remove a buffer
buffer:remove(SC, "priv/sound.wav", 0).
```

for further referencces please check the [wiki](https://github.com/rexmalebka/sc_erl/wiki)
