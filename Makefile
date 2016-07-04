PROJECT = env
PROJECT_DESCRIPTION = Environment variable util
PROJECT_VERSION = 0.0.1

TEST_DEPS = ct_common

dep_ct_common = git git@github.com:DBoroujerdi/ct_common.git master

DIALYZER_DIRS = ebin

include erlang.mk

PLT_APPS = tools runtime_tools compiler eunit
