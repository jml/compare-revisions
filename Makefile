.PHONY: check clean docs format image lint

BUILD_IMAGE = fpco/stack-build:lts-8.6
IMAGE_NAME := compare-revisions
IMAGE_TAG := $(shell ./scripts/image-tag)
EXE_NAME := compare-revisions

LOCAL_BINARY_PATH = $(shell stack path --local-install-root)
LINUX_BINARY_PATH = $(shell stack --docker path --local-install-root)

check:
	stack test --fast

clean:
	stack clean
	stack --docker clean

docs:
	stack haddock

lint:
	hlint -q .

# XXX: This didn't work until I did a manual `docker pull fpco/stack-build:lts-8.6`
# XXX: The stack-build image is also way too big
# XXX: Building all the dependencies takes way too long--can we cache deps somehow as a build image?
# XXX: 'image' is phony, and will build even if everything is up-to-date
image:
	stack --docker build
	./scripts/build-image \
		$(BUILD_IMAGE) \
		$(LINUX_BINARY_PATH)/bin/$(EXE_NAME) \
		$(IMAGE_NAME) \
		$(IMAGE_TAG)

$(LOCAL_BINARY_PATH)/bin/$(EXE_NAME):
	stack build

bash_completion.sh: $(LOCAL_BINARY_PATH)/bin/$(EXE_NAME)
	stack exec $(EXE_NAME) -- --bash-completion-script $(LOCAL_BINARY_PATH)/bin/$(EXE_NAME) > bash_completion.sh
