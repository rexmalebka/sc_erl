erlang Supercollider wrapper.
=====


Erlang Supercollider wrapper based on [Server Command Reference](https://doc.sccode.org/Reference/Server-Command-Reference.html)


Build
-----
    $ rebar3 compile

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
```

for further referencces please check the wiki
