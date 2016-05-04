PROJECT = env
PROJECT_DESCRIPTION = Environment variable util
PROJECT_VERSION = 0.0.1

DIALYZER_DIRS = ebin

include erlang.mk

PLT_APPS = tools runtime_tools compiler eunit
