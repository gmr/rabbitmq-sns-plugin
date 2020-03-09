# RabbitMQ SNS Plugin

The RabbitMQ SNS plugin provides interoperation with AWS Simple Notification Service as both an HTTP(S) endpoint for
receiving SNS messages and an exchange for publishing messages to SNS.

## Example of a received SNS notification

![Received Notification](https://raw.githubusercontent.com/gmr/rabbitmq-sns-plugin/master/images/received.png "Received Notification Screenshot")

## Configuration Options

| Key | Type or Valid Values | Description |
| --- | -------------------- | ----------- |
| `sns_plugin.notifications_enabled` | `true`, `false` | Enable / Disable the `/api/sns` endpoint for receiving SNS notifications |
| `sns_plugin.notifications_exchange` | `string` | The exchange to publish received notifications from AWS to. Default: `aws.sns.notifications` |
