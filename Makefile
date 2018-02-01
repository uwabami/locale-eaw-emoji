# clean target
Downloaded_Files =
# glibc sources
GLIBC_VER=2.26-6
SOURCE_URL=https://sources.debian.org/data/main/g/glibc/$(GLIBC_VER)
I18N_URI=$(SOURCE_URL)/localedata/locales/i18n
Downloaded_Files += i18n
UTF8_URI=$(SOURCE_URL)/localedata/charmaps/UTF-8
Downloaded_Files += UTF-8
# Unicode org
UNICODE_URI=https://www.unicode.org/Public/UNIDATA/UnicodeData.txt
Downloaded_Files += UnicodeData.txt
EAW_URI=https://www.unicode.org/Public/UNIDATA/EastAsianWidth.txt
Downloaded_Files += EastAsianWidth.txt
EMOJI_URI=https://unicode.org/Public/emoji/5.0/emoji-data.txt
Downloaded_Files += emoji-data.txt

Generated_Files  = UTF-8-EAW-EMOJI-FULLWIDTH
Generated_Files += UTF-8-EAW-EMOJI-FULLWIDTH.gz
Generated_Files += EastAsianAmbiguous.txt
Generated_Files += EmojiData.txt
Generated_Files += wcwidth_test_eaw.c
Generated_Files += wcwidth_test_emoji.c
Generated_Files += mlterm_main_completion

all: $(Generated_Files) i18n wcwidth_test_eaw.out wcwidth_test_emoji.out

%.out: %.c
	gcc -Wall -Wextra $< -o $@

emoji-data.txt:
	curl -o $@ $(EMOJI_URI)

EastAsianWidth.txt:
	curl -o $@ $(EAW_URI)

UnicodeData.txt:
	curl -o $@ $(UNICODE_URI)

i18n:
	curl -o $@ $(I18N_URI)

UTF-8:
	curl -o $@ $(UTF8_URI)

UTF-8-EAW-EMOJI-FULLWIDTH: UTF-8 EastAsianWidth.txt emoji-data.txt UnicodeData.txt
	ruby generate.rb $(UNICODE_VER)

UTF-8-EAW-EMOJI-FULLWIDTH.gz: UTF-8-EAW-EMOJI-FULLWIDTH
	gzip -n -9 -c $^ > $@

wcwidth_test_eaw.c: UTF-8-EAW-EMOJI-FULLWIDTH
wcwidth_test_emoji.c: UTF-8-EAW-EMOJI-FULLWIDTH

distclean: clean
	-rm -rf $(Generated_Files)

clean:
	-rm -rf $(Downloaded_Files) *.out
