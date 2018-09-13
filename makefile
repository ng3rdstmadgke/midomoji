MKFILE_DIR := $(shell dirname $(abspath $(lastword $(MAKEFILE_LIST))))
RESOURCES  := $(MKFILE_DIR)/src/main/resources
DICTIONARY := $(MKFILE_DIR)/dictionary
TARGET     := $(MKFILE_DIR)/target/scala-2.12
CONFIG     := $(MKFILE_DIR)/config
IPADIC     := $(ipadic)

.PHONY: build
build: $(RESOURCES)/dict.bin $(RESOURCES)/matrix.bin $(RESOURCES)/config.bin $(RESOURCES)/pos_info.bin $(RESOURCES)/meta_info.bin
	@echo [info] sbt assembly 2
	cd $(MKFILE_DIR); sbt assembly

$(RESOURCES)/dict.bin: $(DICTIONARY)/morpheme.tsv $(TARGET)/midomoji.jar
	@echo [info] build dict.bin
	@java -jar $(TARGET)/midomoji.jar build-dict $(DICTIONARY)
	@cp $(DICTIONARY)/dict.bin $@

$(RESOURCES)/matrix.bin: $(DICTIONARY)/matrix.tsv $(TARGET)/midomoji.jar
	@echo [info] build matrix.bin
	@java -jar $(TARGET)/midomoji.jar build-matrix $(DICTIONARY)
	@cp $(DICTIONARY)/matrix.bin $@

$(RESOURCES)/config.bin: $(DICTIONARY)/char.tsv  $(DICTIONARY)/char_type.tsv $(DICTIONARY)/unk.tsv $(TARGET)/midomoji.jar
	@echo [info] build config.bin
	@java -jar $(TARGET)/midomoji.jar build-config $(DICTIONARY)
	@cp $(DICTIONARY)/config.bin $@

$(RESOURCES)/pos_info.bin: $(DICTIONARY)/pos.tsv $(TARGET)/midomoji.jar
	@echo [info] build pos_info.bin
	@java -jar $(TARGET)/midomoji.jar build-pos-info $(DICTIONARY)
	@cp $(DICTIONARY)/pos_info.bin $@

$(RESOURCES)/meta_info.bin: $(DICTIONARY)/morpheme.tsv $(TARGET)/midomoji.jar
	@echo [info] build meta_info.bin
	@java -jar $(TARGET)/midomoji.jar build-meta-info $(DICTIONARY)
	@cp $(DICTIONARY)/meta_info.bin $@

$(DICTIONARY)/matrix.tsv: $(IPADIC)/matrix.def
	@echo [info] convert matrix.tsv
	@nkf -Luw $< | tr " " "\t"  > $@

$(DICTIONARY)/char.tsv: $(CONFIG)/char.def
	@echo [info] convert char.tsv
	@cat $< | sed -r "s/[[:space:]]*#.*//" | grep -v "^$$" | cat  > $@

$(DICTIONARY)/char_type.tsv: $(CONFIG)/char_type.def
	@echo [info] convert char_type.tsv
	@cat $< | sed -r "s/[[:space:]]*#.*//" | grep -v "^$$" | cat > $@

$(DICTIONARY)/morpheme.tsv: $(DICTIONARY)/raw_morpheme.tsv $(DICTIONARY)/pos.tsv
	@echo [info] convert morpheme.tsv
	@cat $(word 1, $^) | python3 $(MKFILE_DIR)/morpheme_converter.py morpheme $(word 2, $^) > $@

$(DICTIONARY)/unk.tsv: $(CONFIG)/unk.def $(DICTIONARY)/pos.tsv
	@echo [info] convert unk.tsv
	@cat $(word 1, $^) | sed -r "s/[[:space:]]*#.*//" | grep -v "^$$" | python3 $(MKFILE_DIR)/morpheme_converter.py unk $(word 2, $^) | LC_ALL=C sort | uniq > $@

$(DICTIONARY)/pos.tsv: $(DICTIONARY)/raw_morpheme.tsv $(CONFIG)/pos.def
	@echo [info] convert pos.tsv
	@cat $(word 1, $^) | awk -F$$'\t' '{print $$5"\t"$$6"\t"$$7"\t"$$8"\t"$$9"\t"$$10}' | LC_ALL=C sort | uniq > $@
	@cat $(word 2, $^) | sed -r "s/[[:space:]]*#.*//" | grep -v "^$$" | cat  >> $@

$(DICTIONARY)/raw_morpheme.tsv: $(IPADIC)
	@echo  [info] create raw_morpheme.tsv
	@nkf -Luw $(IPADIC)/*.csv | tr "," "\t" | python3 $(MKFILE_DIR)/normalize.py > $@

$(TARGET)/midomoji.jar: $(MKFILE_DIR)/src/main/scala/midomoji
	@echo [info] sbt assembly 1
	cd $(MKFILE_DIR); sbt assembly

.PHONY: clean
clean:
	sbt clean
	rm $(RESOURCES)/* || true
	rm $(DICTIONARY)/* || true
