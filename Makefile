.PHONY: all test clean images
.DEFAULT_GOAL := all

# Boiler plate for bulding Docker containers.
# All this must go at top of file I'm afraid.
IMAGE_PREFIX := quay.io/weaveworks/
IMAGE_TAG := $(shell ./tools/image-tag)
UPTODATE := .uptodate

%/$(UPTODATE): %/Dockerfile
	mkdir -p $(@D)/.output
	cp $(@D)/Dockerfile $(@D)/.output/
	$(SUDO) docker run $(RM) -ti \
		-v $(shell pwd):$(shell pwd) \
		--entrypoint=/usr/local/bin/copy-libraries \
		quay.io/weaveworks/build-haskell:latest \
		$(BINARY_DIR)/$(shell basename $(@D)) \
		$(shell pwd)/$(@D)/.output
	$(SUDO) docker build -t $(IMAGE_PREFIX)$(shell basename $(@D)) $(@D)/.output
	$(SUDO) docker tag $(IMAGE_PREFIX)$(shell basename $(@D)) $(IMAGE_PREFIX)$(shell basename $(@D)):$(IMAGE_TAG)
	rm $(@D)/.output/Dockerfile
	touch $@

# Get a list of directories containing Dockerfiles
DOCKERFILES := $(shell find . -name tools -prune -o -name vendor -prune -o -type f -name 'Dockerfile' -print)
UPTODATE_FILES := $(patsubst %/Dockerfile,%/$(UPTODATE),$(DOCKERFILES))
DOCKER_IMAGE_DIRS := $(patsubst %/Dockerfile,%,$(DOCKERFILES))
IMAGE_NAMES := $(foreach dir,$(DOCKER_IMAGE_DIRS),$(patsubst %,$(IMAGE_PREFIX)%,$(shell basename $(dir))))
images:
	$(info $(IMAGE_NAMES))
	@echo > /dev/null

# stack builds all the binaries. This assumes the binaries are defined by
# 'Main.hs' files in cmd/<executable-name>/Main.hs
BINARY_DIR := $(shell pwd)/.output
MAIN_HS := $(shell find . -name tools -prune -o -type f -name 'Main.hs' -print)
EXES := $(foreach exe, $(patsubst ./cmd/%/Main.hs, %, $(MAIN_HS)), $(BINARY_DIR)/$(exe))
HS_FILES := $(shell find . -name tools -prune -o -name cmd -prune -o -type f -name '*.hs' -print)
define dep_exe
$(1): cmd/$(shell basename $(1))/Main.hs $(HS_FILES)
$(dir $(1))$(UPTODATE): $(1)
endef
$(foreach exe, $(EXES), $(eval $(call dep_exe, $(exe))))

# Manually declared dependancies And what goes into each exe
all: $(UPTODATE_FILES)
test:

# All the boiler plate for building golang follows:
SUDO := $(shell docker info >/dev/null 2>&1 || echo "sudo -E")
BUILD_IN_CONTAINER := true
RM := --rm

cmd/compare-revisions/$(UPTODATE): $(EXES)


ifeq ($(BUILD_IN_CONTAINER),true)

$(EXES) lint test shell:
	mkdir -p $(BINARY_DIR)
	$(SUDO) time docker run $(RM) -ti \
		-e SRC_PATH=$(shell pwd) \
		-e STACK_IN_CONTAINER=1 \
		-e STACK_ROOT=$(HOME)/.stack \
		-e HOME=$(shell pwd)/.stack-work/docker/_home \
		-v $(HOME):$(HOME) \
		-v $(HOME)/.stack:$(HOME)/.stack \
		-v $(shell pwd):$(shell pwd) \
		-v $(shell pwd)/.stack-work/docker/_home:$(shell pwd)/.stack-work/docker/_home \
		quay.io/weaveworks/build-haskell:latest $@

else

$(EXES):
	stack --internal-re-exec-version=1.3.2 \
		--internal-docker-entrypoint "DockerEntrypoint {deUser = Nothing}" \
		--docker \
		--local-bin-path $(BINARY_DIR) \
		--copy-bins \
		build

lint:
	./tools/lint -notestpackage .

test:
	stack --internal-re-exec-version=1.3.2 \
		--internal-docker-entrypoint "DockerEntrypoint {deUser = Nothing}" \
		--docker \
		test

shell:
	bash

endif

clean:
	$(SUDO) docker rmi $(IMAGE_NAMES) >/dev/null 2>&1 || true
	rm -rf $(UPTODATE_FILES) $(EXES)
	stack clean
