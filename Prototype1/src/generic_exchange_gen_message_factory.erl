-module(generic_exchange_gen_message_factory).
-export([reject/1]).

-include("../include/generic_exchange.hrl").

reject(MSG )->
	MSG#generic_msg{
		type             = reject,
		%target 		     = Target,
		caller 		     = MSG#generic_msg.callee,
		callee 		     = MSG#generic_msg.caller
	}.
