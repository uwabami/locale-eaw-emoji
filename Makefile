SOURCE_URL=http://sources.debian.net/data/main/g/glibc/2.26-6
UNICODE_URI=$(SOURCE_URL)/localedata/unicode-gen/UnicodeData.txt
EAW_URI=$(SOURCE_URL)/localedata/unicode-gen/EastAsianWidth.txt
I18N_URI=$(SOURCE_URL)/localedata/locales/i18n
UTF8_URI=$(SOURCE_URL)/localedata/charmaps/UTF-8
EMOJI_URI=http://unicode.org/Public/emoji/5.0/emoji-data.txt

Generated_Files  = UTF-8-EAW-EMOJI-FULLWIDTH UTF-8-EAW-EMOJI-FULLWIDTH.gz
Generated_Files += EastAsianAmbiguous.txt EmojiData.txt
Generated_Files += wcwidth_test_eaw.c
Generated_Files += wcwidth_test_emoji.c
Generated_Files += mlterm_main_completion
Downloaded_Files = emoji-data.txt EastAsianWidth.txt UnicodeData.txt UTF-8 i18n

all: $(Generated_Files) i18n wcwidth_test_eaw.out wcwidth_test_emoji.out

%.out: %.c
	gcc -Wall -Wextra $< -o $@

emoji-data.txt:
	wget -O $@ $(EMOJI_URI)

EastAsianWidth.txt:
	wget -O $@ $(EAW_URI)

UnicodeData.txt:
	wget -O $@ $(UNICODE_URI)

i18n:
	wget -O $@ $(I18N_URI)

UTF-8:
	wget -O $@ $(UTF8_URI)

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
