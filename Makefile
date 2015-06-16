CP=../copilot-unsafe/Copilot/

PACKAGES= \
  $(CP)/lib/copilot-core \
  $(CP)/lib/copilot-language \
  $(CP)/lib/copilot-libraries \
  $(CP)/lib/copilot-sbv \
  $(CP)/lib/copilot-c99 \
  $(CP)/lib/copilot-cbmc \
  $(CP)


default:
		build

cabal.sandbox.config:
		cabal sandbox init

.PHONY: build
build: 	cabal.sandbox.config
		echo $(PACKAGES)
		cabal sandbox add-source $(PACKAGES)
		cabal install $(PACKAGES)