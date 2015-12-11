#!/usr/bin/env ruby
# -*- coding: utf-8 -*-

eaw_source = 'EastAsianWidth.txt'
emoji_source = 'emoji-data.txt'
unicode_source = 'UnicodeData.txt'
output_EAW_amb = 'EastAsianAmbiguous.txt'
output_EMOJI = 'EmojiData.txt'
utf8_source = 'UTF-8'
utf8_output = 'UTF-8-EAW-EMOJI-FULLWIDTH'
eaw_and_emoji_elisp = 'eaw_and_emoji.el'
wcwidth_test_eaw = 'wcwidth_test_eaw.c'
wcwidth_test_emoji = 'wcwidth_test_emoji.c'

$combining_charactor_range = "0300".to_i(16).."036F".to_i(16)
$variation_selector_range1 = "180B".to_i(16).."180D".to_i(16)
$variation_selector_range2 = "FE00".to_i(16).."FE0F".to_i(16)
$variation_selector_range3 = "E0100".to_i(16).."E01EF".to_i(16)
$private_use_range1  = "E000".to_i(16).."F8FF".to_i(16)
$private_use_range2  = "F0000".to_i(16).."FFFFD".to_i(16)
$private_use_range3 = "100000".to_i(16).."10FFFD".to_i(16)

def hex_rjust(hex)
  @hex = hex
  if @hex.length <= 4
    return @hex.rjust(4,"0")
  elsif @hex.length <= 5
    return @hex.rjust(5,"0")
  elsif @hex.length <= 6
    return @hex.rjust(6,"0")
  end
end

def check_unused_range(hex)
  @hex = hex
  return true if $combining_charactor_range.cover?(@hex.to_i(16))
  return true if $variation_selector_range1.cover?(@hex.to_i(16))
  return true if $variation_selector_range2.cover?(@hex.to_i(16))
  return true if $variation_selector_range3.cover?(@hex.to_i(16))
  return true if $private_use_range1.cover?(@hex.to_i(16))
  return true if $private_use_range2.cover?(@hex.to_i(16))
  return true if $private_use_range3.cover?(@hex.to_i(16))
  return false
end

$unicode_table = {}
File.open(unicode_source).each_line{|line|
  data = line.split(';')
  unless check_unused_range data[0]
    $unicode_table[data[0]] = data[1]
  end
}

def desc_grep(hex)
  @hex = hex_rjust(hex).upcase
  desc = $unicode_table[@hex]
  return desc
end

list_eaw = {}
File.open(eaw_source).each_line {|line|
  if line =~/^([a-fA-F\d\.]+);(\w)\s+#\s+(.*)/
    range = $1
    prop = $2
    desc = $3
    if prop == 'A'
      if range =~/([a-fA-F\d]+)\.\.([a-fA-F\d]+)/
        range_start = $1
        range_end = $2
        unless check_unused_range range_start
          for i in range_start.to_i(16)..range_end.to_i(16)
            list_eaw["#{hex_rjust(i.to_s(16).upcase)}"] = desc_grep("#{i.to_s(16)}")
          end
        end
      else
        unless check_unused_range range
          list_eaw["#{hex_rjust(range).upcase}"] = desc_grep("#{range}")
        end
      end
    end
  end
}
# list_eaw =

File.open(output_EAW_amb, 'w+'){|f|
  list_eaw.sort{|(k1,v1), (k2,v2)| k1.to_i(16) <=> k2.to_i(16)}.each  {|k, v|
    k_int = k.to_i(16)
    if k_int <= "0xffff".to_i(16)
      f.puts sprintf("[%c] U+%04X %s", k_int, k_int, v)
    else
      f.puts sprintf("[%c] U+%08X %s", k_int, k_int, v)
    end
  }
}

File.open(wcwidth_test_eaw, 'w+'){|f|
  f.puts <<-EOS
#define _XOPEN_SOURCE
#include <stdio.h>
#include <locale.h>
#include <wchar.h>

void print_wcwidth(wchar_t c, char *str)
{
  printf("wcwidth('[%lc]') == %d, codepoint [%x], %s\\n", c, wcwidth(c), c, str);
}
int main()
{
  setlocale(LC_CTYPE, "");
EOS
  list_eaw.each {|k, v|
    f.puts "  print_wcwidth(0x" + k.to_i(16).to_s(16) + ", \"#{v}\");"
  }
  f.puts <<-EOS
  return 0;
}
EOS
}

list_emoji = {}
File.open(emoji_source).each_line {|line|
  if line =~/^([a-fA-F\d]+)\s;.*#\sV(\d.\d)\s(.+)/
    range = $1
    ver = $2.to_f
    if ver <= ARGV[0].to_f
      list_emoji["#{range}"] = desc_grep(range)
    end
  end
}
File.open(output_EMOJI, 'w+'){|f|
  list_emoji.sort{|(k1,v1), (k2,v2)| k1.to_i(16) <=> k2.to_i(16)}.each {|k, v|
    k_int = k.to_i(16)
    if k_int <= "0xffff".to_i(16)
      f.puts sprintf("[%c] U+%04X %s", k_int, k_int, v)
    else
      f.puts sprintf("[%c] U+%08X %s", k_int, k_int, v)
    end
  }
}

File.open(wcwidth_test_emoji, 'w+'){|f|
  f.puts <<-EOS
#define _XOPEN_SOURCE
#include <stdio.h>
#include <locale.h>
#include <wchar.h>

void print_wcwidth(wchar_t c, char *str)
{
  printf("wcwidth('[%lc]') == %d, codepoint [%x], %s\\n", c, wcwidth(c), c, str);
}
int main()
{
  setlocale(LC_CTYPE, "");
EOS
  list_emoji.each {|k, v|
    f.puts "  print_wcwidth(0x" + k.to_i(16).to_s(16) + ", \"#{v}\");"
  }
  f.puts <<-EOS
  return 0;
}
EOS
}

list = list_eaw.merge(list_emoji).sort{|(k1,v1), (k2,v2)| k1.to_i(16) <=> k2.to_i(16)}
File.open(eaw_and_emoji_elisp, 'w+'){|f|
  f.puts "(setq east-asian-ambiguous-and-emoji\n      '("
  list.each {|k, v|
    f.puts sprintf("        #x%s ; %s", k, v)
  }
  f.puts '        ))'
  f.puts <<-EOS
(defun eaw-and-emoji-set-width (width)
  (while (char-table-parent char-width-table)
    (setq char-width-table (char-table-parent char-width-table)))
  (let ((table (make-char-table nil)))
    (mapc (lambda (range) (set-char-table-range table range width))
          east-asian-ambiguous-and-emoji)
    (optimize-char-table table)
    (set-char-table-parent table char-width-table)
    (setq char-width-table table)))

(defun eaw-and-emoji-fullwidth ()
  (eaw-and-emoji-set-width 2))

(provide 'eaw_and_emoji)
EOS
}

File.open(utf8_output, 'w+'){|f|
  f.puts File.readlines(utf8_source)[0..-2]
  f.puts "% Add East Asian Width and Emoji"
  list.each {|k, v|
    k_int = k.to_i(16)
    if k_int <= "0xffff".to_i(16)
      f.puts sprintf("<U%04X> 2 %% %s", k_int, v)
    else
      f.puts sprintf("<U%08X> 2 %% %s", k_int, v)
    end
  }
  f.puts "END WIDTH"
}
