REBAR=$(shell [ -f ./rebar ] && echo "./rebar" || echo "rebar")
RELX=$(shell [ -f ./relx ] && echo "./relx" || echo "relx")
ERL_EXEC=$(shell which erl)
ERL_BIN=$(shell dirname ${ERL_EXEC})
ERL_TOP=$(shell dirname ${ERL_BIN})
PLT_DEPS = $(filter-out deps/rebar,$(wildcard deps/*))
ERL_LIB_DIR=${ERL_TOP}/lib
ERL_APPS = $(shell ls $(ERL_LIB_DIR) | grep -v interface | grep -v tsung | sed -e 's/-[0-9.]*//')

all: get-deps compile release

get-deps:
	@${REBAR} get-deps

fast-clean:
	@${REBAR} skip_deps=true clean

clean:
	@${REBAR} clean

deps-clean:
	@rm -rf deps

compile:
	@${REBAR} compile

fast-compile:
	@${REBAR} skip_deps=true compile

test:
	@${REBAR} -v skip_deps=true ct

build-plt:
	if [ ! -f .plt ]; then \
		rm -f deps/rebar/ebin/getopt.beam ; \
		dialyzer --build_plt --output_plt .plt -r ${ERL_LIB_DIR} -r deps ; \
	fi

clean-plt:
	@rm .plt

dialyze:
	@ERL_LIBS=deps dialyzer --verbose --fullpath -Wno_undefined_callbacks \
		--plts .plt \
		--src src -r ebin \
		| grep -v -f ./dialyzer.ignore-warnings

release-clean:
	@rm -rf _rel

release:
	@$(RELX) -o _cluster/n1 --overlay_vars files/config/vars/n1.config -c relx.config
	@$(RELX) -o _cluster/n2 --overlay_vars files/config/vars/n2.config -c relx.config
	@$(RELX) -o _cluster/n3 --overlay_vars files/config/vars/n3.config -c relx.config

.PHONY: all get-deps fast-clean clean deps-clean compile fast-compile test build-plt clean-plt dialyze release-clean release
