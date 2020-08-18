PROJECT = aicow
PROJECT_DESCRIPTION = Cowboy helper for productions of ailink.io
PROJECT_VERSION = 0.2.0

ERLC_OPTS = +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard -DENABLE_LOG

DEPS = cowboy ailib jiffy

dep_cowboy_commit = 2.7.0

dep_jiffy = git https://github.com/DavidAlphaFox/jiffy rebar3
dep_ailib = git https://github.com/DavidAlphaFox/ailib.git v0.4.5


include erlang.mk
