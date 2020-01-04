#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
# define sources
eaw_source = 'EastAsianWidth.txt'
emoji_source = 'EmojiSources.txt'
nameslist_source = 'NamesList.txt'
utf8_source = 'UTF-8'
icons_in_terminal_source = 'icons-in-terminal-mapping.txt'

# define output
output_eaw_amb = 'EastAsianAmbiguous.txt'
output_emoji = 'EmojiData.txt'
wcwidth_test_eaw = 'wcwidth_test_eaw.c'
wcwidth_test_emoji = 'wcwidth_test_emoji.c'
wcwidth_test_icons = 'wcwidth_test_icons.c'
utf8_output = 'UTF-8-EAW-EMOJI-FULLWIDTH'
eaw_and_emoji_elisp = 'locale-eaw-emoji.el'

# COMBINING GRAVE ACCENT..COMBINING LATIN SMALL LETTER X
$combining_charactor_range = '0300'.to_i(16)..'036F'.to_i(16)
# MONGOLIAN FREE VARIATION_SELECTOR ONE... THREE
$variation_selector_range1 = '180B'.to_i(16)..'180D'.to_i(16)
# VARIATION SLECTOR-1..VARIATION_SELECTOR-16
$variation_selector_range2 = 'FE00'.to_i(16)..'FE0F'.to_i(16)
# VARIATION SLECTOR-17..VARIATION_SELECTOR-256
$variation_selector_range3 = 'E0100'.to_i(16)..'E01EF'.to_i(16)
# Private Use, First .. Private Use, Last
$private_use_range1  = 'E000'.to_i(16)..'F8FF'.to_i(16)
# Private Use, First .. Private Use, Last
$private_use_range2  = 'F0000'.to_i(16)..'FFFFF'.to_i(16)
# Private Use, First .. Private Use, Last
$private_use_range3 = '100000'.to_i(16)..'10FFFF'.to_i(16)
# Exclude SQUARED THREE D..SQUARED VOD
$exclude_range = '1F19B'.to_i(16)..'1F1AC'.to_i(16)
 # icons-in-terminal range
$icon_in_terminal_range1 = 'E000'.to_i(16)..'EEFF'.to_i(16)
 # icons-in-terminal range
$icon_in_terminal_range2 = 'F000'.to_i(16)..'F8FF'.to_i(16)

def check_unused_range(hex)
  @hex = hex
  return true  if $combining_charactor_range.cover?(@hex.to_i(16))
  return true  if $variation_selector_range1.cover?(@hex.to_i(16))
  return true  if $variation_selector_range2.cover?(@hex.to_i(16))
  return true  if $variation_selector_range3.cover?(@hex.to_i(16))
  return false if $icon_in_terminal_range1.cover?(@hex.to_i(16))
  return false if $icon_in_terminal_range2.cover?(@hex.to_i(16))
  return true  if $private_use_range1.cover?(@hex.to_i(16))
  return true  if $private_use_range2.cover?(@hex.to_i(16))
  return true  if $private_use_range3.cover?(@hex.to_i(16))
  return true  if $exclude_range.cover?(@hex.to_i(16))
  return false
end

def hex_rjust(hex)
  @hex = hex
  if @hex.length <= 4
    return @hex.rjust(4,'0')
  elsif @hex.length <= 5
    return @hex.rjust(5,'0')
  elsif @hex.length <= 6
    return @hex.rjust(6,'0')
  end
end

def desc_grep(hex)
  @hex = hex_rjust(hex).upcase
  desc = $unicode_table[@hex]
  return desc
end

$unicode_table = {}
File.open(nameslist_source).each_line{|line|
  if line =~/^\d/
    data = line.chomp.split("\t")
    unless check_unused_range data[0]
      $unicode_table[data[0]] = data[1]
    end
  end
}

$list_eaw = {}
File.open(eaw_source).each_line {|line|
  if line =~/^([a-fA-F\d\.]+);(\w+)\s+#\s+.*/
    range = $1
    prop = $2
    if prop == 'A'
      if range =~/([a-fA-F\d]+)\.\.([a-fA-F\d]+)/
        range_start = $1
        range_end = $2
        unless check_unused_range range_start
          for i in range_start.to_i(16)..range_end.to_i(16)
            $list_eaw["#{hex_rjust(i.to_s(16).upcase)}"] = desc_grep("#{i.to_s(16)}")
          end
        end
      else
        unless check_unused_range range
          $list_eaw["#{hex_rjust(range).upcase}"] = desc_grep("#{range}")
        end
      end
    end
  end
}

$list_emoji = {}
File.open(emoji_source).each_line {|line|
  if line =~ /^([a-fA-F\d]+);.*/
    range = $1
    $list_emoji["#{range}"] = desc_grep(range)
  end
}

$list_icon = {}
File.open(icons_in_terminal_source).each_line {|line|
  if line =~ /^(\S+)\s+:(.+)/
    range = $2
    $list_icon["#{range}"] = $1
  end
}

for i in 0x1f000..0x1fffd
  unless $list_emoji["#{i.to_s(16).upcase}"]
    $list_emoji["#{i.to_s(16).upcase}"] = desc_grep(i.to_s(16).upcase)
  end
end

File.open(output_eaw_amb, 'w+'){|f|
  $list_eaw.sort{|(k1,v1), (k2,v2)| k1.to_i(16) <=> k2.to_i(16)}.each  {|k, v|
    k_int = k.to_i(16)
    if k_int <= "0xffff".to_i(16)
      f.puts sprintf("[%c] U+%04X %s", k_int, k_int, v)
    else
      f.puts sprintf("[%c] U+%08X %s", k_int, k_int, v)
    end
  }
}

File.open(output_emoji, 'w+'){|f|
  $list_emoji.sort{|(k1,v1), (k2,v2)| k1.to_i(16) <=> k2.to_i(16)}.each {|k, v|
    k_int = k.to_i(16)
    unless v.nil?
      if k_int <= "0xffff".to_i(16)
        f.puts sprintf("[%c] U+%04X %s", k_int, k_int, v)
      else
        f.puts sprintf("[%c] U+%08X %s", k_int, k_int, v)
      end
    end
  }
}

# remove EMOJI from EAW, duplicates
$list_emoji.each {|k, v|
  if $list_eaw[k]
    $list_eaw.delete(k)
  end
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
  $list_eaw.each {|k, v|
    f.puts "  print_wcwidth(0x" + k.to_i(16).to_s(16) + ", \"#{v}\");" unless v.nil?
  }
  f.puts <<-EOS
  return 0;
}
EOS
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
  $list_emoji.each {|k, v|
    f.puts "  print_wcwidth(0x" + k.to_i(16).to_s(16) + ", \"#{v}\");" unless v.nil?
  }
  f.puts <<-EOS
  return 0;
}
EOS
}

File.open(wcwidth_test_icons, 'w+'){|f|
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
  $list_icon.each {|k, v|
    f.puts "  print_wcwidth(0x" + k.to_i(16).to_s(16) + ", \"#{v}\");" unless v.nil?
  }
  f.puts <<-EOS
  return 0;
}
EOS
}


File.open(eaw_and_emoji_elisp, 'w+'){|f|
  f.puts <<-EOS
;;; locale-eaw-emoji.el --- set UAX11 and Emoji as double width

;; Copyright (C) 2010-2016 Youhei SASAKI <uwabami@gfd-dennou.org>

;; Author: Youhei SASAKI <uwabami@gfd-dennou.org>
;; Created: 2015-12-10 08:09:00 +0900
;; Updated: #{Time.now}
;; Version: 0.0.1
;; Package-Version: #{Time.now.strftime("%Y%m%d.%H%M%S")}
;; Package-Requires: nil
;; Keywords: tools
;; URL: https://github.com/uwabami/locale-eaw-emoji
;;
;; This file is not part of GNU Emacs.
;;
;; License:
;;
;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:
;; .
;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.
;; .
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;
;;; Commentary:
;;
;; This file is auto-generated file, please sea `generate.rb' in this repos.
;;
;;; Code:

EOS
  f.puts "(setq east-asian-ambiguous-and-emoji\n  '("
  $list_eaw.each {|k, v|
    f.puts sprintf("    #x%s ; %s", k, v) unless v.nil?
  }
  $list_emoji.each {|k, v|
    f.puts sprintf("    #x%s ; %s", k, v) unless v.nil?
  }
  $list_icon.each {|k, v|
    f.puts sprintf("    #x%s ; %s", k, v) unless v.nil?
  }
  f.puts "        ))\n"
  f.puts <<-EOS
;;;###autoload
(defun eaw-and-emoji-set-width (width)
  "Set character width in east-asian-ambiguous-and-emoji as `WIDTH'."
  (while (char-table-parent char-width-table)
    (setq char-width-table (char-table-parent char-width-table)))
  (let ((table (make-char-table nil)))
    (mapc (lambda (range) (set-char-table-range table range width))
          east-asian-ambiguous-and-emoji)
    (optimize-char-table table)
    (set-char-table-parent table char-width-table)
    (setq char-width-table table)))

;;;###autoload
(defun eaw-and-emoji-fullwidth ()
  "Just shortcut of (eaw-and-emoji-set-width 2)."
  (setq nobreak-char-display nil)
  (eaw-and-emoji-set-width 2))

(provide 'locale-eaw-emoji)

;;; locale-eaw-emoji.el ends here
EOS
}

list = $list_eaw.merge($list_emoji).merge($list_icon).sort{|(k1,v1), (k2,v2)| k1.to_i(16) <=> k2.to_i(16)}
File.open(utf8_output, 'w+'){|f|
  f.puts File.readlines(utf8_source)[0..-2]
  f.puts "% Add East Asian Ambiguous Width and Emoji as fullwidth"
  list.each {|k, v|
    k_int = k.to_i(16)
    unless v.nil?
      if k_int <= "0xffff".to_i(16)
        f.puts sprintf("<U%04X> 2 %% %s", k_int, v)
      else
        f.puts sprintf("<U%08X> 2 %% %s", k_int, v)
      end
    end
  }
  f.puts "END WIDTH"
}
