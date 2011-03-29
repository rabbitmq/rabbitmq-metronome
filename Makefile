PACKAGE=rabbitmq-metronome
APPNAME=rabbit_metronome
DEPS=rabbitmq-server rabbitmq-erlang-client

TEST_APPS=amqp_client rabbit_metronome
TEST_COMMANDS=eunit:test(rabbit_metronome_tests,[verbose])
START_RABBIT_IN_TESTS=true

include ../include.mk
