# Unicode org
UNICODE_VER=13.0.0
UNICODE_URI=https://unicode.org/Public/$(UNICODE_VER)/ucd
Downloaded_Files  = UnicodeData.txt
Downloaded_Files += PropList.txt
Downloaded_Files += EastAsianWidth.txt
Downloaded_Files += EmojiSources.txt
Downloaded_Files += NamesList.txt
Generated_Files  = UTF-8
Generated_Files += UTF-8-EAW-EMOJI-FULLWIDTH
Generated_Files += UTF-8-EAW-EMOJI-FULLWIDTH.gz
Generated_Files += EastAsianAmbiguous.txt
Generated_Files += EmojiData.txt
Generated_Files += wcwidth_test_eaw.c
Generated_Files += wcwidth_test_eaw.out
Generated_Files += wcwidth_test_emoji.c
Generated_Files += wcwidth_test_emoji.out
Generated_Files += wcwidth_test_icons.c
Generated_Files += wcwidth_test_icons.out
Generated_Files += locale-eaw-emoji.el

all: $(Generated_Files)

UnicodeData.txt:
	wget -O $@ $(UNICODE_URI)/$@
PropList.txt:
	wget -O $@ $(UNICODE_URI)/$@
EastAsianWidth.txt:
	wget -O $@ $(UNICODE_URI)/$@
EmojiSources.txt:
	wget -O $@ $(UNICODE_URI)/$@
NamesList.txt:
	wget -O $@ $(UNICODE_URI)/$@

%.out: %.c
	gcc -Wall -Wextra $< -o $@

UTF-8: UnicodeData.txt PropList.txt EastAsianWidth.txt
	python3 ./utf8_gen.py \
		-u UnicodeData.txt \
		-e EastAsianWidth.txt \
		-p PropList.txt --unicode_version $(UNICODE_VER)

UTF-8-EAW-EMOJI-FULLWIDTH: UTF-8 NamesList.txt EmojiSources.txt
	ruby generate.rb $(UNICODE_VER)

locale-eaw-emoji.el: UTF-8-EAW-EMOJI-FULLWIDTH.gz
	@ruby generate.rb $(UNICODE_VER)

UTF-8-EAW-EMOJI-FULLWIDTH.gz: UTF-8-EAW-EMOJI-FULLWIDTH
	gzip -n -9 -c $^ > $@

wcwidth_test_eaw.c: UTF-8-EAW-EMOJI-FULLWIDTH.gz
wcwidth_test_emoji.c: UTF-8-EAW-EMOJI-FULLWIDTH.gz
wcwidth_test_icons.c: UTF-8-EAW-EMOJI-FULLWIDTH.gz

clean:
	-rm -rf $(Downloaded_Files) *.out
distclean: clean
	-rm -rf $(Generated_Files) __pycache__
