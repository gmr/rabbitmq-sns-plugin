-module(rabbitmq_sns_endpoint).

-include_lib("amqp_client/include/amqp_client.hrl").

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
  
  % Establish a connection to the local RabbitMQ server
  % Using custom connection parameters for port 5671 (TLS)
  Params = #amqp_params_network{
    username = <<"guest">>,
    password = <<"guest">>,
    port = 5671,
    ssl_options = [
      {verify, verify_none},
      {fail_if_no_peer_cert, false}
    ]
  },
  ConnectionResult = amqp_connection:start(Params),
  case ConnectionResult of
    {ok, Connection} ->
      ChannelResult = amqp_connection:open_channel(Connection),
      case ChannelResult of
        {ok, Channel} ->
          % Build the AMQP message properties
          Props = #'P_basic'{
            app_id = <<"rabbitmq-sns-plugin">>,
            content_type = <<"application/json">>,
            content_encoding = <<"UTF-8">>,
            headers = message_headers(Req),
            message_id = maps:get(<<"MessageId">>, Message, undefined),
            timestamp = os:system_time(seconds),
            type = maps:get(<<"Type">>, Message, undefined)
          },
          
          % Create the publish command 
          Publish = #'basic.publish'{
            exchange = State#state.exchange,
            routing_key = maps:get(<<"TopicArn">>, Message, <<"">>)
          },
          
          % Create and publish the AMQP message
          AmqpMessage = #amqp_msg{props = Props, payload = Body},
          amqp_channel:cast(Channel, Publish, AmqpMessage),
          
          % Clean up - close the channel and connection
          amqp_channel:close(Channel),
          amqp_connection:close(Connection),
          
          {true, Req, State};
        _ChannelError ->
          rabbit_log:error("Failed to open channel: ~p", [ChannelResult]),
          {false, Req, State}
      end;
    _ConnectionError ->
      rabbit_log:error("Failed to connect to RabbitMQ: ~p", [ConnectionResult]),
      {false, Req, State}
  end.

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
    {ok, Value} -> list_to_binary(Value)
  end.