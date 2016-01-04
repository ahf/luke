PROJECT = luke

PROJECT_DESCRIPTION = A New Hope
PROJECT_VERSION = 1.0.0

CFLAGS = -Wall -Wextra -O3 -fomit-frame-pointer

TEST_DEPS = triq

dep_triq = git https://github.com/krestenkrab/triq master

include erlang.mk
