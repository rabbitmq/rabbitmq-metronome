PROJECT = rabbitmq_pg_auth
PROJECT_DESCRIPTION = Rabbitmq Postgresql driven authentication
PROJECT_MOD = rabbit_pgauth

DEPS = rabbit_common rabbit amqp_client rabbitmq_management epgsql rabbitmq_mqtt

DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

include rabbitmq-components.mk
include erlang.mk
