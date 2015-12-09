UNICODE_VER=7.0.0
UNICODE_URI=http://ftp.unicode.org/Public/$(UNICODE_VER)/ucd/UnicodeData.txt
EAW_URI=http://www.unicode.org/Public/$(UNICODE_VER)/ucd/EastAsianWidth.txt
EMOJI_URI=http://unicode.org/Public/emoji/1.0/emoji-data.txt
# glibc2.22 - localedata
I18N_URI='https://sourceware.org/git/?p=glibc.git;a=blob_plain;f=localedata/locales/i18n;hb=578d080544d77daad68dd697e77f2dcff50c6c7b'
UTF8_URI='https://sourceware.org/git/?p=glibc.git;a=blob_plain;f=localedata/charmaps/UTF-8;hb=578d080544d77daad68dd697e77f2dcff50c6c7b'

Generated_Files  = UTF-8-EAW-EMOJI-FULLWIDTH UTF-8-EAW-EMOJI-FULLWIDTH.gz
Generated_Files += EastAsianAmbiguous.txt EmojiData.txt
Generated_Files += eaw_and_emoji.el
Downloaded_Files = emoji-data.txt EastAsianWidth.txt UnicodeData.txt UTF-8 i18n

all: $(Generated_Files) i18n

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

distclean: clean
	-rm -rf $(Generated_Files)

clean:
	-rm -rf $(Downloaded_Files)
