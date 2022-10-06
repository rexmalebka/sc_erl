-module(test1).

main()->
	{
	'Out',[
	       0,
	       {'Sum2',[
		       {
			'SinOsc',[
				  freq
				  , 0.5
				  , 0.1			 
				 ],
			ar,
			0
		       },
		       {
			'SinOsc',[
				 freq2
				 , 0
				 , 0
				 ],
			ar,
			0
		       }
		       ],
		ar,
		0
	      ],
	ar,
	0
	}.

%	synthdef:new("playbuf", #{freq=400},
%		     fun(
%		       #{ugens:=
%			 #{
%			   'SinOsc':=SinOsc
%			  },
%			 ops:=
%			 #{
%			   'Sum':=Sum
%			  }
%			})->
%				     A = SinOsc(freq,0.5,01),
%				     B = SinOSc(freq2, 0,0),
%
%				     C = Sum([A,B])
%				     Out(0, A)
%		     end
%		    ).
