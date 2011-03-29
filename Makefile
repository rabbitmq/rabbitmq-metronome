PACKAGE=rabbitmq-metronome
DEPS=rabbitmq-server rabbitmq-erlang-client

TEST_APPS=amqp_client rabbit_metronome
TEST_COMMANDS=rabbit_metronome_tests:test()
START_RABBIT_IN_TESTS=true

include ../include.mk
