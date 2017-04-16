.PHONY: all test clean images
.DEFAULT_GOAL := all

# Boiler plate for bulding Docker containers.
# All this must go at top of file I'm afraid.
IMAGE_PREFIX := quay.io/weaveworks/
IMAGE_TAG := $(shell ./tools/image-tag)
UPTODATE := .uptodate

# Building Docker images is now automated. The convention is every directory
# with a Dockerfile in it builds an image calls quay.io/weaveworks/<dirname>.
# Dependencies (i.e. things that go in the image) still need to be explicitly
# declared.
%/$(UPTODATE): %/Dockerfile
	$(SUDO) docker build -t $(IMAGE_PREFIX)$(shell basename $(@D)) $(@D)/
	$(SUDO) docker tag $(IMAGE_PREFIX)$(shell basename $(@D)) $(IMAGE_PREFIX)$(shell basename $(@D)):$(IMAGE_TAG)
	touch $@

# Get a list of directories containing Dockerfiles
DOCKERFILES := $(shell find . -name tools -prune -o -name vendor -prune -o -type f -name 'Dockerfile' -print)
UPTODATE_FILES := $(patsubst %/Dockerfile,%/$(UPTODATE),$(DOCKERFILES))
DOCKER_IMAGE_DIRS := $(patsubst %/Dockerfile,%,$(DOCKERFILES))
IMAGE_NAMES := $(foreach dir,$(DOCKER_IMAGE_DIRS),$(patsubst %,$(IMAGE_PREFIX)%,$(shell basename $(dir))))
images:
	$(info $(IMAGE_NAMES))
	@echo > /dev/null

# Building binaries is now automated.  The convention is to build a binary
# for every directory with main.go in it, in the ./cmd directory.
MAIN_HS := $(shell find . -name tools -prune -o -type f -name 'Main.hs' -print)
EXES := $(foreach exe, $(patsubst ./cmd/%/Main.hs, %, $(MAIN_HS)), ./cmd/$(exe)/$(exe))
HS_FILES := $(shell find . -name tools -prune -o -name cmd -prune -o -type f -name '*.hs' -print)
define dep_exe
$(1): $(dir $(1))/Main.hs $(HS_FILES)
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

ifeq ($(BUILD_IN_CONTAINER),true)

$(EXES) lint test shell:
	$(SUDO) time docker run $(RM) -ti \
		-e SRC_PATH=$(shell pwd) \
		-e STACK_IN_CONTAINER=1 \
		-e STACK_ROOT=$(HOME) \
		-e HOME=$(shell pwd)/.stack-work/docker/_home \
		-v $(HOME):$(HOME) \
		-v $(HOME)/.stack:$(HOME)/.stack \
		-v $(shell pwd):$(shell pwd) \
		-v $(shell pwd)/.stack-work/docker/_home:$(shell pwd)/.stack-work/docker/_home \
		quay.io/weaveworks/build-haskell:latest $@

else

$(EXES):
	stack --internal-re-exec-version=1.3.2 --internal-docker-entrypoint "DockerEntrypoint {deUser = Nothing}" --docker build

lint:
	./tools/lint -notestpackage .

test:
	stack test

shell:
	bash

endif

clean:
	$(SUDO) docker rmi $(IMAGE_NAMES) >/dev/null 2>&1 || true
	rm -rf $(UPTODATE_FILES) $(EXES)
	stack clean
