-module(rabbitmq_sns_publish).

-behaviour(rabbit_exchange_type).
-include_lib("rabbit_common/include/rabbit.hrl").

-rabbit_boot_step({?MODULE, [
  {description,   "SNS Publishing Exchange"},
  {mfa,           {rabbit_registry, register, [exchange, <<"x-sns-publish">>, ?MODULE]}},
  {cleanup,       {rabbit_registry, unregister, [exchange, <<"x-sns-publish">>]}},
  {requires,      rabbit_registry},
  {enables,       kernel_ready}
]}).

-export([
  description/0,
  route/2,
  info/1,
  info/2,
  serialise_events/0,
  validate/1,
  create/2,
  recover/2,
  delete/3,
  policy_changed/2,
  add_binding/3,
  remove_bindings/3,
  validate_binding/2,
  assert_args_equivalence/2
]).

description() ->
  [{name, <<"x-sns-publish">>}, {description, <<"Publish messages to a Amazon SNS topic.">>}].

route(_X,
      #delivery{message = #basic_message{routing_keys = _Keys,
                                         content      = _Content0}}) ->
  [].  %% Drop the message

info(_X) -> [].
info(_X, _) -> [].
serialise_events() -> false.
validate(_X) -> ok.
create(_Tx, _X) -> ok.
recover(_X, _Bs) -> ok.
delete(_Tx, _X, _Bs) -> ok.
policy_changed(_X1, _X2) -> ok.
add_binding(_Tx, _X, _B) -> ok.
remove_bindings(_Tx, _X, _Bs) -> ok.
validate_binding(_X, _B) -> ok.
assert_args_equivalence(X, Args) ->
  rabbit_exchange:assert_args_equivalence(X, Args).