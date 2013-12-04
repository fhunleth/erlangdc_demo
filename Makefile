
SDCARD_LOCATION=/dev/sdc

all:
	rebar get-deps compile
	if [ -n "$(NERVES_ROOT)" ]; then $(NERVES_ROOT)/scripts/rel2fw.sh _rel; fi

burn:
	sudo env PATH=$(PATH) fwtool -t complete -d $(SDCARD_LOCATION) run _images/bbb.fw

clean:
	rebar clean
	-rm -fr _rel _images
