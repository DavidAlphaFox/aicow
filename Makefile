PROJECT = aicow
PROJECT_DESCRIPTION = Cowboy helper for productions of ailink.io
PROJECT_VERSION = 0.1.0

ERLC_OPTS = +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard -DENABLE_LOG

DEPS = cowboy jiffy ailib


dep_cowboy_commit = 2.7.0
dep_jiffy_commit = 1.0.1

dep_ailib = git https://github.com/DavidAlphaFox/ailib.git tag-0.4.1


include erlang.mk
