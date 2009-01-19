-module(amazon_test_001).
-export([instructions/0]).

-include("excavator.hrl").

-define(ROOT_URL, "http://www.amazon.com/s/qid=1232337093/ref=sr_pg_2/182-6187393-2698830?ie=UTF8&rs=14545&rh=n%3A75%2Cn%3A14545&page=2").

instructions() ->
	[ #push{ url=?ROOT_URL, xpath="//div[@id='searchTemplate']" },
	  #pop{ num=one, name=scope, type=xml },
	
	  [ #push{ regexp="<div class=\"resultCount\">Showing [0-9]+ - [0-9]+ of ([0-9,]+) Results</div>", source=scope },
		#pop{ num=one, name=total_results, type=int32, action=commit }
	  ],
	
	  [ #push{ xpath="//div[@class='result firstResultRow' | 'result']", source=scope },
	 	#pop{ num=one, name=scope, type=xml },
	
	    [ #push{ regexp="<div class=\"productTitle\"><a href=\"([^\"]+)\"> ([\\w]+)</a>", source=scope },
		  #pop{ num=one, name=product_url, type=string, action=commit },
		  #pop{ num=one, name=product_title, type=string, action=commit },
		
		  [ #push{ url=product_url },
			#pop{ num=one, name=scope, type=xml },
			
			[ #push{ regexp="<b class=\"priceLarge\">$([0-9\\.]+)</b>", source=scope },
		      #pop{ num=one, name=product_price, type=decimal, action=commit }
			],
			
			[ #push{ xpath=#pretty{ format="//related_items[@title=~s]", args=[product_title] }, source=scope },
			  #pop{ num=all, name=scope, type=xml },
			
			  [ #push{ regexp="<div name=\"item\">([0-9]+)</div>", source=scope },
				#pop{ num=one, name=related_id, type=int32, action=commit }
			  ]  
			]
		  ]
	    ]
	  ]
	].
	
%% push sets the current scope frame to X elements
%% pop adds V elements to the state dictionary
