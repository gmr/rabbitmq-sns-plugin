-module(rabbitmq_sns_endpoint).

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").

-behaviour(rabbit_mgmt_extension).

-export([dispatcher/0,
         web_ui/0,
         init/2,
         content_types_accepted/2,
         allowed_methods/2,
         process_sns_payload/2]).

-record(state, {exchange}).

dispatcher() ->
  case is_enabled() of
    true  ->
      rabbit_log:info("rabbitmq-sns-plugin `/api/sns` endpoint publishing exchange: ~s", [get_exchange()]),
      [{"/sns", ?MODULE, #state{exchange = get_exchange()}}];
    false ->
      rabbit_log:info("rabbitmq-sns-plugin `/api/sns` endpoint disabled"),
      []
  end.

web_ui() -> [].

init(Req, State) ->
  {cowboy_rest, Req, State}.

content_types_accepted(ReqData, Context) ->
  {[{{<<"application">>, <<"json">>, '*'}, process_sns_payload},
    {{<<"text">>, <<"plain">>, '*'}, process_sns_payload}], ReqData, Context}.

allowed_methods(ReqData, Context) ->
  {[<<"POST">>], ReqData, Context}.

process_sns_payload(Req, State) ->
  {ok, Body, _Req} = cowboy_req:read_body(Req),
  Message = rabbit_json:decode(Body),
  Properties = #'P_basic'{app_id       = <<"rabbitmq-sns-plugin">>,
                          content_type = <<"application/json">>,
                          headers      = message_headers(Req),
                          message_id   = maps:get(<<"MessageId">>, Message),
                          timestamp    = os:system_time(seconds),
                          type         = maps:get(<<"Type">>, Message)},
  Content = rabbit_basic:build_content(Properties, [Body]),
  {ok, Msg} = rabbit_basic:message(rabbit_misc:r(<<"/">>, exchange, State#state.exchange),
                                   maps:get(<<"TopicArn">>, Message),
                                   Content),
  rabbit_basic:publish(rabbit_basic:delivery(false, false, Msg, undefined)),
  {true, Req, State}.

format_remote_ip(Address) ->
  [[123, Value, 125]] = io_lib:format("~p", [Address]),
  list_to_binary(lists:flatten([comma_to_period(V) || V <- Value])).

comma_to_period(44)    -> ".";
comma_to_period(Value) -> Value.

message_headers(Req) ->
  {IP, _Port} = cowboy_req:peer(Req),
  [{<<"remote-ip">>, longstr, format_remote_ip(IP)},
   {<<"x-amz-sns-message-type">>, longstr, cowboy_req:header(<<"x-amz-sns-message-type">>, Req, <<"">>)},
   {<<"x-amz-sns-message-id">>, longstr, cowboy_req:header(<<"x-amz-sns-message-id">>, Req, <<"">>)},
   {<<"x-amz-sns-subscription-arn">>, longstr, cowboy_req:header(<<"x-amz-sns-subscription-arn">>, Req, <<"">>)},
   {<<"x-amz-sns-topic-arn">>, longstr, cowboy_req:header(<<"x-amz-sns-topic-arn">>, Req, <<"">>)}].


is_enabled() ->
  case application:get_env(rabbitmq_sns_plugin, notifications_enabled) of
    undefined   -> true;
    {ok, Value} -> Value
  end.

get_exchange() ->
  case application:get_env(rabbitmq_sns_plugin, notifications_exchange) of
    undefined   -> <<"aws.sns.notifications">>;
    {ok, Value} -> Value
  end.